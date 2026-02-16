# Deep Technical Code Review: R Shiny i18n Architecture
## Senior Architect Audit - Production-Grade Assessment

---

## Executive Summary

**Critical Finding**: The current implementation has **fundamental architectural flaws** that create **cross-session contamination risks**, **race conditions**, and **maintainability debt**. While the application attempts to handle multilingual iframe scenarios, it uses anti-patterns that will cause production failures under concurrent load.

**Severity**: **HIGH** - The global state mutation pattern (`<<-` in UI function) combined with session-scoped language detection creates unpredictable behavior in multi-user scenarios.

**Recommendation**: **Immediate refactoring required** before production deployment. The architecture needs a complete redesign of language state management.

---

## 1. Major Architectural Issues

### 1.1 Global State Mutation in UI Function (CRITICAL)

**Location**: Lines 438-457

**Problem**:
```r
ui <- function(request = NULL) {
  lang <- "en"
  # ... language detection ...
  currentlanguage <<- i18n$set_translation_language(lang)  # GLOBAL MUTATION
  pmReferences <<- if (lang == "en") pmReferences_en else pmReferences_fr  # GLOBAL MUTATION
  # ... 10+ more global mutations using <<-
}
```

**Why This Is Broken**:
1. **Cross-Session Contamination**: The `<<-` operator mutates global environment variables. When User A loads the app with `?lang=fr`, it sets global `pmReferences`, `pollnames`, etc. to French. When User B immediately loads with `?lang=en` in a different session, User A's UI may suddenly switch to English mid-session.

2. **Race Conditions**: In a multi-process Shiny deployment (the standard production setup), multiple R processes serve different users. However, if sessions share the same process (common in development or low-concurrency scenarios), global mutations will cause unpredictable language switching.

3. **UI Function Execution Timing**: The `ui` function is called **once per session initialization**, but global state persists across sessions. This means:
   - Session 1: `ui(request)` sets `pmReferences <<- pmReferences_fr`
   - Session 2: `ui(request)` sets `pmReferences <<- pmReferences_en`
   - Session 1's reactive expressions now see English data even though the session was initialized as French

4. **Non-Reactive**: Global variables are not reactive. If language needs to change mid-session (e.g., user switches language), the UI won't update because it was built once with the initial language.

**Evidence of Impact**:
- Line 2742-2898+: Hundreds of reactive expressions like `canproresults1a_endpoint()` that depend on `get_session_lang()` but reference global variables (`canproresults1a()$endpoint` vs `canproresults1a()$paramètre`). These reactives will see inconsistent language state.

**Production Failure Scenario**:
```
Time T0: User A (French) loads app → global state = French
Time T1: User B (English) loads app → global state = English
Time T2: User A's reactive `cpi_year()` executes → reads global `cpi_en` (WRONG - should be `cpi_fr`)
Result: User A sees mixed English/French content
```

---

### 1.2 Dual Language Detection (Inconsistent State)

**Location**: 
- UI function: Lines 430-437 (detects from `request`)
- Server function: Lines 2609-2643 (detects from `session$clientData$url_search`)

**Problem**:
The UI function and server function independently detect language, creating potential mismatches:

```r
# UI function (runs once at session start)
ui <- function(request = NULL) {
  lang <- "en"
  if (!is.null(request)) {
    query <- parseQueryString(qs)
    if (!is.null(query$lang)) lang <- query$lang  # Sets global state
  }
  # ... mutates global variables based on lang ...
}

# Server function (runs continuously)
server <- function(input, output, session) {
  get_session_lang <- function() {
    url_search <- session$clientData$url_search
    # ... different detection logic ...
    session$userData$lang <- query$lang  # Sets session state
  }
}
```

**Why This Is Broken**:
1. **Timing Mismatch**: UI is built **before** server initializes. If URL changes between UI build and server initialization, they'll have different languages.

2. **State Divergence**: UI sets global state (`pmReferences <<-`), server uses session state (`session$userData$lang`). These can diverge.

3. **Hidden Input Fallback**: Line 460 creates `input$session_lang` from UI's detected language, but server's `get_session_lang()` (line 2634) falls back to this. This creates a circular dependency: UI sets it, server reads it, but they may disagree.

**Production Failure Scenario**:
```
User loads app with ?lang=fr
→ UI function detects "fr", sets global state to French
→ Server initializes, but URL query string is stripped by proxy/load balancer
→ Server's get_session_lang() falls back to input$session_lang (which is "fr")
→ BUT: If another session already mutated global state to "en", reactive expressions see English data
Result: UI shows French labels, but data shows English column names
```

---

### 1.3 Translation Function Mutates Global i18n State (Race Condition)

**Location**: Lines 2652-2657

**Problem**:
```r
get_session_t <- function(key) {
  old <- i18n$get_translation_language()
  on.exit(i18n$set_translation_language(old))
  i18n$set_translation_language(get_session_lang())  # MUTATES GLOBAL i18n
  i18n$t(key)
}
```

**Why This Is Broken**:
1. **Not Thread-Safe**: The `i18n` object is a global singleton. If two reactive expressions call `get_session_t()` simultaneously:
   - Thread 1: Saves old="en", sets to "fr", translates
   - Thread 2: Saves old="fr" (Thread 1's value!), sets to "en", translates
   - Thread 1: Restores to "en" (WRONG - should restore to original "en")
   - Result: Global i18n state is corrupted

2. **Reactive Context Issues**: In Shiny, reactive expressions can execute in unpredictable order. If `get_session_t("key1")` and `get_session_t("key2")` are called from different reactive contexts that execute concurrently, the `on.exit()` restoration may happen in the wrong order.

3. **Performance**: Every translation call does 3 operations: get current, set new, restore old. With hundreds of translations, this is expensive.

**Production Failure Scenario**:
```
Reactive A (User 1, French): get_session_t("Hello") 
  → saves "en", sets "fr", translates
Reactive B (User 2, English): get_session_t("Hello")
  → saves "fr" (User 1's value!), sets "en", translates
Reactive A: restores to "en" (should be "en" but timing is wrong)
Reactive B: restores to "fr" (WRONG - should restore to "en")
Result: User 2's next translation is in French
```

---

### 1.4 Direct i18n Usage Bypasses Session Language

**Location**: Lines 2702, 2716, 9552, 9561

**Problem**:
```r
# Line 2702 - WRONG: Uses global i18n state
header_message <- i18n$t("Your data has been uploaded")

# Line 9552 - WRONG: Uses global i18n state
filename = function() {
  i18n$t("Toxics.xlsx")
}
```

**Why This Is Broken**:
These calls use `i18n$t()` directly instead of `get_session_t()`. They will use whatever language was last set globally, which may not match the current session's language.

**Impact**: Error messages, download filenames, and other UI elements will appear in the wrong language if global state was mutated by another session.

---

## 2. Language State Problems

### 2.1 No Single Source of Truth

**Current State**: Language is stored in **4 different places**:
1. Global `currentlanguage` variable (line 24, 438)
2. Global i18n object state (line 438)
3. `session$userData$lang` (line 2615, 2630, 2635)
4. `input$session_lang` hidden input (line 460)

**Problem**: These can diverge, and there's no mechanism to keep them synchronized.

**Correct Pattern**: **One source of truth** - `session$userData$lang` should be the **only** language state. All other code should read from this.

---

### 2.2 Language Detection Logic Duplication

**Location**: 
- UI: Lines 430-437
- Server: Lines 2625-2643

**Problem**: The same URL parsing logic exists in two places with slight variations. This violates DRY and creates maintenance risk.

**Correct Pattern**: Language detection should happen **once** in the server, and the UI should receive language as a parameter or read it from a reactive.

---

### 2.3 Language Cannot Change Mid-Session

**Current Limitation**: Language is detected once at session start and never changes. If a user wants to switch languages, they must reload the page.

**Why This Matters**: 
- User experience: Users expect language switchers in modern web apps
- Iframe scenarios: Parent page may change language context, but iframe app won't respond

**Note**: This may be intentional for your use case, but it's worth documenting as a limitation.

---

## 3. i18n Best Practices Violations

### 3.1 Anti-Pattern: Column Name Switching

**Location**: Lines 2742-2898+ (hundreds of reactive expressions)

**Problem**:
```r
canproresults1a_endpoint <- reactive({
  if (get_session_lang() == "en") canproresults1a()$endpoint else canproresults1a()$paramètre
})

cpi_year <- reactive({
  if (get_session_lang() == "en") cpi_en$year else cpi_fr$année
})
```

**Why This Is Broken**:
1. **Maintainability Nightmare**: Every data frame column name change requires updating 2 reactive expressions. With 100+ data frames, this is 200+ reactive expressions to maintain.

2. **Error-Prone**: Easy to forget to create the reactive wrapper, leading to runtime errors when accessing wrong column names.

3. **Performance**: Each reactive adds overhead. With hundreds of these, you're creating unnecessary reactive dependencies.

4. **Not Scalable**: Adding a third language (e.g., Spanish) requires rewriting all 200+ reactive expressions.

**Correct Pattern**: Use a **data access layer** that abstracts column name differences:

```r
# BETTER: Single reactive that returns language-appropriate data frame
get_data_frame <- function(df_name, lang = get_session_lang()) {
  lang_suffix <- if (lang == "en") "_en" else "_fr"
  get(paste0(df_name, lang_suffix), envir = .GlobalEnv)
}

# BETTER: Column name mapping
get_column <- function(df, col_key, lang = get_session_lang()) {
  col_map <- list(
    en = list(endpoint = "endpoint", year = "year"),
    fr = list(endpoint = "paramètre", year = "année")
  )
  actual_col <- col_map[[lang]][[col_key]]
  df[[actual_col]]
}

# USAGE (much cleaner):
canproresults1a_endpoint <- reactive({
  get_column(canproresults1a(), "endpoint")
})
```

---

### 3.2 Translation Keys Not Validated

**Problem**: No validation that translation keys exist in `translations.json`. If a key is missing, `i18n$t()` returns the key itself, which may go unnoticed.

**Correct Pattern**: Add validation in development mode:

```r
get_session_t <- function(key) {
  # ... translation logic ...
  result <- i18n$t(key)
  if (getOption("shiny.i18n.validate", FALSE) && result == key) {
    warning("Missing translation key: ", key)
  }
  result
}
```

---

### 3.3 No Translation Context

**Problem**: Translation keys like `"NL"`, `"PE"` are ambiguous. In a different context, "NL" might mean "Netherlands" not "Newfoundland and Labrador".

**Correct Pattern**: Use namespaced keys:
```r
# BETTER
i18n$t("province.NL")
i18n$t("province.PE")
```

---

## 4. Refactoring Recommendations

### 4.1 Eliminate Global State Mutation

**Current (BROKEN)**:
```r
ui <- function(request = NULL) {
  lang <- detect_lang_from_request(request)
  pmReferences <<- if (lang == "en") pmReferences_en else pmReferences_fr  # GLOBAL MUTATION
  # ...
}
```

**Refactored (CORRECT)**:
```r
ui <- function(request = NULL) {
  # Don't mutate global state - just detect language for UI rendering
  lang <- detect_lang_from_request(request)
  
  # Pass language to UI components, but don't store it globally
  fluidPage(
    tags$div(style = "display: none;", textInput("session_lang", value = lang)),
    # ... UI components that use i18n$t() with lang parameter if needed ...
  )
}

server <- function(input, output, session) {
  # Initialize session language ONCE from hidden input or URL
  session$userData$lang <- isolate({
    if (!is.null(input$session_lang)) {
      input$session_lang
    } else {
      detect_lang_from_url(session) %||% "en"
    }
  })
  
  # Create reactive for language (read-only after initialization)
  session_lang <- reactive({
    session$userData$lang
  })
  
  # All data access goes through session-scoped functions
  get_pmReferences <- reactive({
    if (session_lang() == "en") pmReferences_en else pmReferences_fr
  })
}
```

---

### 4.2 Create Session-Scoped i18n Wrapper

**Current (BROKEN)**:
```r
get_session_t <- function(key) {
  old <- i18n$get_translation_language()
  on.exit(i18n$set_translation_language(old))
  i18n$set_translation_language(get_session_lang())
  i18n$t(key)
}
```

**Refactored (CORRECT)**:
```r
# Create a session-specific translator that doesn't mutate global state
create_session_translator <- function(session) {
  lang <- reactive({ session$userData$lang })
  
  # Load translations once per session
  translations <- fromJSON("translations.json")
  
  list(
    t = function(key) {
      current_lang <- lang()
      if (is.null(translations[[current_lang]][[key]])) {
        warning("Missing translation: ", key, " for language: ", current_lang)
        return(key)
      }
      translations[[current_lang]][[key]]
    },
    get_language = function() lang()
  )
}

# In server:
server <- function(input, output, session) {
  i18n_session <- create_session_translator(session)
  
  # Usage:
  output$title <- renderText({
    i18n_session$t("Welcome")
  })
}
```

**Alternative (if you must use shiny.i18n package)**:
```r
# Create one Translator instance per session (stored in session$userData)
server <- function(input, output, session) {
  if (is.null(session$userData$i18n)) {
    session$userData$i18n <- Translator$new(translation_json_path = "translations.json")
    session$userData$lang <- detect_lang(session)
    session$userData$i18n$set_translation_language(session$userData$lang)
  }
  
  get_session_t <- function(key) {
    session$userData$i18n$t(key)  # No global mutation!
  }
}
```

---

### 4.3 Data Access Layer Abstraction

**Current (BROKEN - 200+ reactive expressions)**:
```r
canproresults1a_endpoint <- reactive({
  if (get_session_lang() == "en") canproresults1a()$endpoint else canproresults1a()$paramètre
})
```

**Refactored (CORRECT - Single pattern)**:
```r
# Define column mappings once
COLUMN_MAPPINGS <- list(
  canproresults1a = list(endpoint = c(en = "endpoint", fr = "paramètre")),
  cpi = list(year = c(en = "year", fr = "année")),
  # ... all mappings in one place ...
)

# Generic column accessor
get_column <- function(df_name, col_key, session_lang) {
  df <- get(df_name, envir = .GlobalEnv)()
  mapping <- COLUMN_MAPPINGS[[df_name]][[col_key]]
  if (is.null(mapping)) {
    stop("No mapping for ", df_name, "$", col_key)
  }
  col_name <- mapping[[session_lang]]
  df[[col_name]]
}

# Usage (much simpler):
canproresults1a_endpoint <- reactive({
  get_column("canproresults1a", "endpoint", session_lang())
})
```

---

### 4.4 Centralize Language Detection

**Refactored**:
```r
# Single function for language detection (used by both UI and server)
detect_language <- function(request_or_session) {
  # Handle both UI request and server session
  if (is.environment(request_or_session) && "clientData" %in% names(request_or_session)) {
    # Server session
    url_search <- request_or_session$clientData$url_search
  } else if (is.environment(request_or_session) && "QUERY_STRING" %in% names(request_or_session)) {
    # UI request
    url_search <- request_or_session$QUERY_STRING
  } else {
    return("en")  # Default
  }
  
  if (!is.null(url_search) && nzchar(url_search)) {
    query <- parseQueryString(url_search)
    if (!is.null(query$lang) && query$lang %in% c("en", "fr")) {
      return(query$lang)
    }
  }
  "en"  # Default
}

# UI uses it:
ui <- function(request = NULL) {
  lang <- detect_language(request)
  # ... build UI with lang ...
}

# Server uses it:
server <- function(input, output, session) {
  session$userData$lang <- isolate(detect_language(session))
  # ...
}
```

---

## 5. Production-Grade Architecture Proposal

### 5.1 Ideal Architecture

```r
# ============================================================================
# MODULE: Language Management
# ============================================================================

LanguageManager <- R6::R6Class("LanguageManager",
  public = list(
    initialize = function(session, default_lang = "en") {
      private$session <- session
      private$lang <- reactiveVal(default_lang)
      private$detect_initial_language()
      private$setup_translator()
    },
    
    get_language = function() {
      private$lang()
    },
    
    translate = function(key, ...) {
      private$translator$t(key, ...)
    },
    
    get_data = function(data_name) {
      lang_suffix <- if (private$lang() == "en") "_en" else "_fr"
      full_name <- paste0(data_name, lang_suffix)
      get(full_name, envir = .GlobalEnv)
    },
    
    get_column = function(df, col_key) {
      mapping <- private$column_mappings[[col_key]]
      if (is.null(mapping)) {
        stop("No column mapping for: ", col_key)
      }
      col_name <- mapping[[private$lang()]]
      df[[col_name]]
    }
  ),
  
  private = list(
    session = NULL,
    lang = NULL,
    translator = NULL,
    column_mappings = list(
      endpoint = c(en = "endpoint", fr = "paramètre"),
      year = c(en = "year", fr = "année"),
      # ... all mappings ...
    ),
    
    detect_initial_language = function() {
      lang <- isolate({
        url_search <- private$session$clientData$url_search
        if (!is.null(url_search) && nzchar(url_search)) {
          query <- parseQueryString(url_search)
          if (!is.null(query$lang) && query$lang %in% c("en", "fr")) {
            return(query$lang)
          }
        }
        "en"
      })
      private$lang(lang)
    },
    
    setup_translator = function() {
      private$translator <- Translator$new(translation_json_path = "translations.json")
      observe({
        private$translator$set_translation_language(private$lang())
      })
    }
  )
)

# ============================================================================
# SERVER: Clean implementation
# ============================================================================

server <- function(input, output, session) {
  # Initialize language manager (session-scoped, no global state)
  lang_mgr <- LanguageManager$new(session)
  
  # All translations use lang_mgr
  output$title <- renderText({
    lang_mgr$translate("Welcome")
  })
  
  # All data access uses lang_mgr
  canproresults1a_endpoint <- reactive({
    df <- lang_mgr$get_data("canproresults1a")
    lang_mgr$get_column(df, "endpoint")
  })
  
  # All reactive expressions are clean and maintainable
}
```

---

### 5.2 Key Principles

1. **Session Isolation**: Every session has its own `LanguageManager` instance. No global state.

2. **Single Source of Truth**: Language is stored in `LanguageManager$private$lang` reactiveVal. All other code reads from this.

3. **Reactive Translation**: Translations are reactive - if language changes (future enhancement), all UI updates automatically.

4. **Data Abstraction**: Column name differences are abstracted away. Adding a new language only requires updating the mapping table.

5. **Type Safety**: Column mappings are validated at initialization, catching errors early.

---

## 6. Potential Failure Scenarios

### 6.1 Concurrent Session Language Contamination

**Scenario**: Two users load the app simultaneously:
- User A: `?lang=fr`
- User B: `?lang=en`

**Current Behavior** (BROKEN):
1. User A's UI function executes → sets global `pmReferences <<- pmReferences_fr`
2. User B's UI function executes → sets global `pmReferences <<- pmReferences_en`
3. User A's reactive `canproresults1a_endpoint()` executes → reads `canproresults1a()$endpoint` (English column, but should be French)
4. **Result**: User A sees mixed languages

**Fixed Behavior** (CORRECT):
- Each session has isolated language state
- No global mutations
- Each session's reactives read from session-scoped data

---

### 6.2 Race Condition in Translation Function

**Scenario**: Two reactive expressions call `get_session_t()` simultaneously from different sessions.

**Current Behavior** (BROKEN):
```
Time T0: Reactive A (Session 1, French) calls get_session_t("Hello")
  → Saves global i18n state: "en"
  → Sets global i18n state: "fr"
Time T1: Reactive B (Session 2, English) calls get_session_t("World")
  → Saves global i18n state: "fr" (Session 1's value!)
  → Sets global i18n state: "en"
Time T2: Reactive A finishes
  → Restores global i18n state: "en" (WRONG - should restore to "en" but timing is off)
Time T3: Reactive B finishes
  → Restores global i18n state: "fr" (WRONG - should restore to "en")
Result: Global i18n state is "fr" when it should be "en"
```

**Fixed Behavior** (CORRECT):
- Each session has its own translator instance
- No global state mutation
- No race conditions

---

### 6.3 Iframe Language Context Loss

**Scenario**: Parent page changes language context, but iframe app doesn't respond.

**Current Behavior**: Language is detected once at session start. If parent page changes `?lang=fr` to `?lang=en`, the iframe app doesn't update.

**Impact**: User sees inconsistent language between parent page and iframe.

**Mitigation** (if needed):
```r
# Add observer to watch for URL changes
observe({
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query$lang) && query$lang != session$userData$lang) {
    session$userData$lang <- query$lang
    # Trigger UI update (would require reactive UI, which is complex)
  }
})
```

**Note**: This may not be necessary if language is fixed per iframe load.

---

### 6.4 Missing Translation Key Silent Failure

**Scenario**: Developer adds new UI text but forgets to add translation key.

**Current Behavior**: `i18n$t("NewKey")` returns `"NewKey"` (the key itself). No error, no warning. User sees raw key in production.

**Mitigation**: Add validation in development mode (see Section 3.2).

---

## 7. Migration Strategy

### Phase 1: Immediate Fixes (Critical)
1. **Remove all `<<-` operators from UI function** - Replace with session-scoped initialization
2. **Fix direct `i18n$t()` calls** - Replace with session-scoped translation function
3. **Add session language initialization** - Ensure `session$userData$lang` is set once at server start

### Phase 2: Architecture Refactoring (High Priority)
1. **Create LanguageManager class** - Centralize all language logic
2. **Create data access layer** - Abstract column name differences
3. **Remove duplicate reactive expressions** - Replace with generic accessors

### Phase 3: Enhancements (Medium Priority)
1. **Add translation validation** - Catch missing keys in development
2. **Add language switching support** - If needed for future requirements
3. **Performance optimization** - Cache translations, optimize reactive dependencies

---

## Conclusion

The current architecture has **fundamental flaws** that will cause production issues under concurrent load. The primary issues are:

1. **Global state mutation** creating cross-session contamination
2. **Race conditions** in translation function
3. **Maintainability debt** from hundreds of duplicate reactive expressions
4. **Inconsistent language state** across UI and server

**Recommendation**: Implement the refactored architecture before production deployment. The migration can be done incrementally (see Migration Strategy), but the global state mutations must be fixed immediately.

**Estimated Refactoring Effort**: 
- Phase 1 (Critical): 2-3 days
- Phase 2 (Architecture): 1-2 weeks
- Phase 3 (Enhancements): 1 week

**Risk of Not Fixing**: High probability of language mixing bugs in production, especially under concurrent user load.

