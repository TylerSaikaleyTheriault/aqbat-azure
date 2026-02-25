# prettier-ignore

library(data.table)
library(demography)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(jsonlite)
library(rsconnect)
library(sf)
library(shiny)
library(shiny.i18n)
library(shinybusy)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(tmap)
library(triangulr)
library(writexl)

# https://cran.r-project.org/web/packages/shiny.i18n/shiny.i18n.pdf
i18n <- Translator$new(translation_json_path = "translations.json")

currentlanguage <- i18n$set_translation_language("en")
# currentlanguage <- i18n$set_translation_language("fr")

# editor.formatOnSave

# SHINY OPTIONS - https://shiny.posit.co/r/reference/shiny/1.0.3/shiny-options.html
options(shiny.autoreload = TRUE)

options(shiny.sanitize.errors = TRUE)

# PAGES
source("pages/summary.R", local = TRUE)

# TEMPLATE FUNCTIONS
createRestoreInstructions <- function(i18n) {
  htmlTemplate("templates/restore-instructions.html", i18n = i18n)
}

createLoader <- function(loadingMessage) {
  htmlTemplate(
    filename = "templates/loader.html",
    loadingMessage = loadingMessage
  )
}

createWarningModal <- function(message, type, i18n) {
  htmlTemplate("templates/data-warning.html", message = message, type = type, i18n = i18n)
}

createDisconnectMessage <- function(i18n) {
  htmlTemplate("templates/disconnect-message.html", i18n = i18n)
}

createDisconnectNoticeModal <- function(message, type, i18n) {
  htmlTemplate("templates/disconnect-notice.html", message = message, type = type, i18n = i18n)
}

createClearDataButton <- function(page, i18n) {
  htmlTemplate("templates/clear-data-button.html", page = page, i18n = i18n)
}

createErrorMessage <- function(message, i18n, identifier = "") {
  htmlTemplate(
    "templates/error.html",
    message = message,
    identifier = identifier,
    i18n = i18n
  )
}

createStepUI <- function(i18n, stepNumber, totalSteps, progressMax) {
  htmlTemplate(
    "templates/progress.html",
    i18n = i18n,
    stepNumber = stepNumber,
    totalSteps = totalSteps,
    progressMax = progressMax,
  )
}

# load reference data (both languages for URL-based lang= switch)
pmReferences_en <- fromJSON("data/pmReferences.json")
pmReferences_fr <- fromJSON("data/pmReferences_fr.json")
pmReferences <- pmReferences_en
otherReferences_en <- fromJSON("data/otherReferences.json")
otherReferences_fr <- fromJSON("data/otherReferences_fr.json")
otherReferences <- otherReferences_en
valuationReferences_en <- fromJSON("data/valuationReferences.json")
valuationReferences_fr <- fromJSON("data/valuationReferences_fr.json")
valuationReferences <- valuationReferences_en

# setup app reload
jscode <- "shinyjs.reload = function() { location.reload(); }"


# load files of pollutant and province names for maps
pollnames_en <- read.csv("pollnames.csv", stringsAsFactors = FALSE)
pollnames_fr <- read.csv("pollnames_fr.csv", stringsAsFactors = FALSE)

pollnames <- if (currentlanguage == "en") pollnames_en else pollnames_fr

prov_en <- read.csv("prov.csv", stringsAsFactors = FALSE)
prov_fr <- read.csv("prov_fr.csv", stringsAsFactors = FALSE)

prov <- if (currentlanguage == "en") prov_en else prov_fr

xprov_en <- read.csv("xprov.csv", stringsAsFactors = FALSE)
xprov_fr <- read.csv("xprov_fr.csv", stringsAsFactors = FALSE)

xprov <- if (currentlanguage == "en") xprov_en else xprov_fr

# load census division map shapefile, select CDUID and CDname variables and transform to simple features object
cdmap2 <- st_read(dsn = "cdmap", layer = "cdmap")

cdmap1 <- cdmap2[c(1:2)]

# extra steps for French version - provincial map download
cdmap1_download <- cdmap1
names(cdmap1_download)[1] <- paste("IDUDR")
names(cdmap1_download)[2] <- paste("NOMDR")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 1"] <- paste("R", intToUtf8(0xe9), "gion 1", sep = "")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 2"] <- paste("R", intToUtf8(0xe9), "gion 2", sep = "")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 3"] <- paste("R", intToUtf8(0xe9), "gion 3", sep = "")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 4"] <- paste("R", intToUtf8(0xe9), "gion 4", sep = "")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 5"] <- paste("R", intToUtf8(0xe9), "gion 5", sep = "")
cdmap1_download$NOMDR[cdmap1_download$NOMDR == "Region 6"] <- paste("R", intToUtf8(0xe9), "gion 6", sep = "")

cdmap <- as(cdmap1, "sf")

# reformat French CD names for both languages
cdmap$CDNAME <- ifelse(cdmap$CDUID == 2451, paste("Maskinong", intToUtf8(0xe9), sep = ""),
  ifelse(cdmap$CDUID == 2457, paste("La Vall", intToUtf8(0xe9), "e-du-Richelieu", sep = ""),
    ifelse(cdmap$CDUID == 2466, paste("Montr", intToUtf8(0xe9), "al", sep = ""),
      ifelse(cdmap$CDUID == 2483, paste("La Vall", intToUtf8(0xe9), "e-de-la-Gatineau", sep = ""),
        ifelse(cdmap$CDUID == 2485, paste("T", intToUtf8(0xe9), "miscamingue", sep = ""),
          ifelse(cdmap$CDUID == 2489, paste("La Vall", intToUtf8(0xe9), "e-de-l'Or", sep = ""),
            ifelse(cdmap$CDUID == 2499, paste("Nord-du-Qu", intToUtf8(0xe9), "bec", sep = ""),
              ifelse(cdmap$CDUID == 2402, paste("Le Rocher-Perc", intToUtf8(0xe9), sep = ""),
                ifelse(cdmap$CDUID == 2404, paste("La Haute-Gasp", intToUtf8(0xe9), "sie", sep = ""),
                  ifelse(cdmap$CDUID == 2407, paste("La Matap", intToUtf8(0xe9), "dia", sep = ""),
                    ifelse(cdmap$CDUID == 2413, paste("T", intToUtf8(0xe9), "miscouata", sep = ""),
                      ifelse(cdmap$CDUID == 2423, paste("Qu", intToUtf8(0xe9), "bec", sep = ""),
                        ifelse(cdmap$CDUID == 2425, paste("L", intToUtf8(0xe9), "vis", sep = ""),
                          ifelse(cdmap$CDUID == 2435, paste("M", intToUtf8(0xe9), "kinac", sep = ""),
                            ifelse(cdmap$CDUID == 2438, paste("B", intToUtf8(0xe9), "cancour", sep = ""),
                              ifelse(cdmap$CDUID == 2445, paste("Memphr", intToUtf8(0xe9), "magog", sep = ""),
                                ifelse(cdmap$CDUID == 2475, paste("La Rivi", intToUtf8(0xe8), "re-du-Nord", sep = ""),
                                  ifelse(cdmap$CDUID == 2497, paste("Sept-Rivi", intToUtf8(0xe8), "res--Caniapiscau", sep = ""),
                                    ifelse(cdmap$CDUID == 2412, paste("Rivi", intToUtf8(0xe8), "re-du-Loup", sep = ""),
                                      ifelse(cdmap$CDUID == 2433, paste("Lotbini", intToUtf8(0xe8), "re", sep = ""),
                                        ifelse(cdmap$CDUID == 2495, paste("La Haute-C", intToUtf8(0xf4), "te-Nord", sep = ""),
                                          ifelse(cdmap$CDUID == 2401, paste("Les ", intToUtf8(0xce), "les-de-la-Madeleine", sep = ""),
                                            ifelse(cdmap$CDUID == 2432, paste("L'", intToUtf8(0xce), "rable", sep = ""),
                                              ifelse(cdmap$CDUID == 2441, paste("Le Haut-Saint-Fran", intToUtf8(0xe7), "ois", sep = ""),
                                                ifelse(cdmap$CDUID == 2442, paste("Le Val-Saint-Fran", intToUtf8(0xe7), "ois", sep = ""),
                                                  ifelse(cdmap$CDUID == 2473, paste("Th", intToUtf8(0xe9), "r", intToUtf8(0xe8), "se-De Blainville", sep = ""),
                                                    ifelse(cdmap$CDUID == 2403, paste("La C", intToUtf8(0xf4), "te-de-Gasp", intToUtf8(0xe9), sep = ""),
                                                      ifelse(cdmap$CDUID == 2421, paste("La C", intToUtf8(0xf4), "te-de-Beaupr", intToUtf8(0xe9), sep = ""),
                                                        ifelse(cdmap$CDUID == 2420, paste("L'", intToUtf8(0xce), "le-d'Orl", intToUtf8(0xe9), "ans", sep = ""), cdmap$CDNAME)
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

cdmap_en <- cdmap
cdmap_fr <- cdmap

# extra steps for French version - interactive map
names(cdmap_fr)[1] <- paste("IDUDR")
names(cdmap_fr)[2] <- paste("NOMDR")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 1"] <- paste("R", intToUtf8(0xe9), "gion 1", sep = "")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 2"] <- paste("R", intToUtf8(0xe9), "gion 2", sep = "")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 3"] <- paste("R", intToUtf8(0xe9), "gion 3", sep = "")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 4"] <- paste("R", intToUtf8(0xe9), "gion 4", sep = "")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 5"] <- paste("R", intToUtf8(0xe9), "gion 5", sep = "")
cdmap_fr$NOMDR[cdmap_fr$NOMDR == "Region 6"] <- paste("R", intToUtf8(0xe9), "gion 6", sep = "")

cdmap <- if (currentlanguage == "en") cdmap_en else cdmap_fr

three_en <- read.csv("Baseline_population_data.csv", stringsAsFactors = FALSE) # data includes 2 components: baseline rate and population projection(4)from 2001 to 2063, CD, province(NL)
three_fr <- read.csv("Baseline_population_data_fr.csv", stringsAsFactors = FALSE) # French version

three <- if (currentlanguage == "en") three_en else three_fr

cpi_en <- read.csv("cpi.csv", stringsAsFactors = FALSE) # Consumer Price Index data from 1990 to 2020
cpi_fr <- read.csv("cpi_fr.csv", stringsAsFactors = FALSE) # French version

cpi <- if (currentlanguage == "en") cpi_en else cpi_fr

# TODO: remove this
# xsample1_en <- read.csv("pollutants_2016wtox_fr.csv", stringsAsFactors = FALSE)
xsample1_en <- read.csv("pollutants_2016wtox.csv", stringsAsFactors = FALSE) # sample pollutant excel sheet, scenario has to be a number
xsample1_fr <- read.csv("pollutants_2016wtox_fr.csv", stringsAsFactors = FALSE) # French version

xsample1 <- if (currentlanguage == "en") xsample1_en else xsample1_fr

# Build geocode for both languages so server can use session-scoped version (cross-tab fix)
.geocode_lang <- function(lang) {
  old <- i18n$get_translation_language()
  on.exit(i18n$set_translation_language(old))
  i18n$set_translation_language(lang)
  data.frame(
    Name = c(
      "Canada", i18n$t("NL"), i18n$t("PE"), i18n$t("NS"), i18n$t("NB"), i18n$t("QC"), i18n$t("ON"), i18n$t("MB"), i18n$t("SK"),
      i18n$t("AB"), i18n$t("BC"), i18n$t("YK"), i18n$t("NT"), i18n$t("NU")
    ),
    geocode = c(1, 10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62),
    geotype = c(i18n$t("National"), "Province", "Province", "Province", "Province", "Province", "Province", "Province", "Province", "Province", "Province", i18n$t("Territory"), i18n$t("Territory"), i18n$t("Territory"))
  )
}
geocode_en <- .geocode_lang("en")
geocode_fr <- .geocode_lang("fr")
geocode <- geocode_en # default for source-time use

# define functions (log_linear_label optional: pass get_session_t("log-linear") from server for session language)
beta <- function(rtype, rr, incr, log_linear_label = NULL) {
  ll <- if (is.null(log_linear_label)) i18n$t("log-linear") else log_linear_label
  ifelse(rtype == ll, log(rr) / incr, (rr - 1) / incr)
} # transfer RR to beta
se <- function(rtype, urr, lrr, incr, log_linear_label = NULL) {
  ll <- if (is.null(log_linear_label)) i18n$t("log-linear") else log_linear_label
  ifelse(rtype == ll, (log(urr) - log(lrr)) / (2 * 1.96 * incr), (urr - lrr) / (2 * 1.96 * incr))
} # transfer 95%CI to se

chrochg <- function(statusquo, counterfactual) {
  return(statusquo - counterfactual)
} # function for SCHIF
af <- function(rtype, beta, deltap, log_linear_label = NULL) {
  ll <- if (is.null(log_linear_label)) i18n$t("log-linear") else log_linear_label
  ifelse(rtype == ll, ((exp(beta * deltap) - 1) / exp(beta * deltap)), beta * deltap)
} # function for attributable fraction
afa <- function(deltap, beta) {
  (exp(beta * deltap) - 1) / exp(beta * deltap)
}
afb <- function(deltap, beta) {
  (beta * deltap)
}
thr1 <- function(pollutant1, vecp, vthr) {
  pmax(outer(pollutant1, vecp, "*"), vthr)
} # function for acute exposure outcomes
thr2 <- function(pctexcess, wt) {
  sweep(pctexcess, 2, wt, "*")
} # function for acute exposure outcomes
thr3 <- function(pctexcess) {
  apply(pctexcess, 1, sum)
} # function for acute exposure outcomes

af2 <- function(cshape, cscale, cdiff) {
  return((exp(cshape * cscale * cdiff) - 1) / exp(cshape * cscale * cdiff))
} # function (Gamma) for mean value for 4 cause-specific mortality
af3 <- function(cquantile, pdiff) {
  return((exp(cquantile * pdiff) - 1) / exp(cquantile * pdiff))
} # function to get 95%CI from  gamma distribution
chg <- function(forecast, statusquo, thr) {
  return(pmax(forecast, thr) - pmax(statusquo, thr))
} # function for a threshold concentration for acute exposure type health outcomes
chg2 <- function(x) {
  ifelse(x < 0, 0, x)
}
chg3 <- function(b, a) {
  ifelse(b == 0, 0, a / b)
}
chg4 <- function(k) {
  ifelse(k == " ", "na", k)
}
chg5 <- function(u, w) {
  return(u / w)
} # only used for 4 cause-specific mortality (e.g., divided by 10)for gamma distribution (CRF)
chg6 <- function(s) {
  ifelse(s - 2.4 < 0, 0, s - 2.4)
} # SCHIF: threshold for pm2.5 is 2.4
chg7 <- function(c, p) {
  ifelse(p == 0, 0, c / p)
}
# function: format number
format_numbers <- function(x) {
  formatted1 <- format(round(as.numeric(x), 1), big.mark = ",", decimal.mark = ".", scientific = FALSE)
  return(formatted1)
}

format_numbers2 <- function(x) {
  formatted2 <- format(round(as.numeric(x), 1), big.mark = " ", decimal.mark = ",", scientific = FALSE)
  return(formatted2)
}
# function: Create a function to spell out the full name for a province or CD

merge_data <- function(df1, df2, by.x, by.y) {
  if (missing(by.x) || missing(by.y)) {
    stop("Please specify the column names to join by using the 'by.x' and 'by.y' arguments.")
  }

  merged_df <- merge(df1, df2, by.x = by.x, by.y = by.y)
  return(merged_df)
}


popnonasthma <- function(pasthma, popage5_19, pop20plus) {
  return((1 - 0.01 * pasthma) * popage5_19 + pop20plus)
} # function for non-asthma population

# change in life expectancy functions - piecewise polynomial functions of percent change in mortality
lifeyr <- function(pct1) {
  ifelse((abs(pct1) * 100 >= 0 & abs(pct1) * 100 <= 20), (8.6401 * pct1 - 3.5068 * pct1 * pct1),
    ifelse((abs(pct1) * 100 > 20 & abs(pct1) * 100 <= 50), 0.0524 + 8.1531 * pct1 - 2.3153 * pct1 * pct1,
      ifelse((abs(pct1) * 100 > 50 & abs(pct1) * 100 <= 100), (0.3007 + 7.213 * pct1 - 1.4085 * pct1 * pct1),
        ifelse((abs(pct1) * 100 > 100 & abs(pct1) * 100 <= 200), (1.0694 + 5.7626 * pct1 - 0.7134 * pct1 * pct1),
          ifelse((abs(pct1) * 100 > 200 & abs(pct1) * 100 <= 500), (3.3495 + 3.6966 * pct1 - 0.2345 * pct1 * pct1),
            ifelse((abs(pct1) * 100 > 500 & abs(pct1) * 100 <= 1000), (7.3796 + 2.0646 * pct1 - 0.0662 * pct1 * pct1), 999)
          )
        )
      )
    )
  )
}

lifeyr2 <- function(pct2) {
  ifelse((abs(pct2) * 100 >= 0 & abs(pct2) * 100 <= 20), (0.7739 * pct2 - 0.046 * pct2 * pct2),
    ifelse((abs(pct2) * 100 > 20 & abs(pct2) * 100 <= 50), (0.0001 + 0.7727 * pct2 - 0.0431 * pct2 * pct2),
      ifelse((abs(pct2) * 100 > 50 & abs(pct2) * 100 <= 100), (0.0013 + 0.7685 * pct2 - 0.0392 * pct2 * pct2),
        ifelse((abs(pct2) * 100 > 100 & abs(pct2) * 100 <= 200), (0.0084 + 0.7556 * pct2 - 0.0333 * pct2 * pct2),
          ifelse((abs(pct2) * 100 > 200 & abs(pct2) * 100 <= 500), (0.0648 + 0.7077 * pct2 - 0.0229 * pct2 * pct2),
            ifelse((abs(pct2) * 100 > 500 & abs(pct2) * 100 <= 1000), (0.3352 + 0.6043 * pct2 - 0.0128 * pct2 * pct2), 999)
          )
        )
      )
    )
  )
}

lifeyr3 <- function(pct2) {
  ifelse((abs(pct2) * 100 >= 0 & abs(pct2) * 100 <= 20), (0.1763 * pct2 - 0.0015 * pct2 * pct2),
    ifelse((abs(pct2) * 100 > 20 & abs(pct2) * 100 <= 50), (0.0000007 + 0.1763 * pct2 - 0.0015 * pct2 * pct2),
      ifelse((abs(pct2) * 100 > 50 & abs(pct2) * 100 <= 100), (0.0000007 + 0.1762 * pct2 - 0.0015 * pct2 * pct2),
        ifelse((abs(pct2) * 100 > 100 & abs(pct2) * 100 <= 200), (0.00005 + 0.1762 * pct2 - 0.0015 * pct2 * pct2),
          ifelse((abs(pct2) * 100 > 200 & abs(pct2) * 100 <= 500), (0.0006 + 0.1757 * pct2 - 0.0014 * pct2 * pct2),
            ifelse((abs(pct2) * 100 > 500 & abs(pct2) * 100 <= 1000), (0.0052 + 0.174 * pct2 - 0.0012 * pct2 * pct2), 999)
          )
        )
      )
    )
  )
}

lifeyr4 <- function(pct2) {
  ifelse((abs(pct2) * 100 >= 0 & abs(pct2) * 100 <= 20), (0.4012 * pct2 - 0.0109 * pct2 * pct2),
    ifelse((abs(pct2) * 100 > 20 & abs(pct2) * 100 <= 50), (0.00001 + 0.4011 * pct2 - 0.0106 * pct2 * pct2),
      ifelse((abs(pct2) * 100 > 50 & abs(pct2) * 100 <= 100), (0.0001 + 0.4006 * pct2 - 0.0102 * pct2 * pct2),
        ifelse((abs(pct2) * 100 > 100 & abs(pct2) * 100 <= 200), (0.001 + 0.399 * pct2 - 0.0095 * pct2 * pct2),
          ifelse((abs(pct2) * 100 > 200 & abs(pct2) * 100 <= 500), (0.0099 + 0.3916 * pct2 - 0.0079 * pct2 * pct2),
            ifelse((abs(pct2) * 100 > 500 & abs(pct2) * 100 <= 1000), (0.0706 + 0.3689 * pct2 - 0.0057 * pct2 * pct2), 999)
          )
        )
      )
    )
  )
}

lifeyr5 <- function(pct2) {
  ifelse((abs(pct2) * 100 >= 0 & abs(pct2) * 100 <= 20), (1.2587 * pct2 - 0.1021 * pct2 * pct2),
    ifelse((abs(pct2) * 100 > 20 & abs(pct2) * 100 <= 50), (0.0004 + 1.255 * pct2 - 0.0935 * pct2 * pct2),
      ifelse((abs(pct2) * 100 > 50 & abs(pct2) * 100 <= 100), (0.0037 + 1.243 * pct2 - 0.0823 * pct2 * pct2),
        ifelse((abs(pct2) * 100 > 100 & abs(pct2) * 100 <= 200), (0.0227 + 1.2083 * pct2 - 0.0663 * pct2 * pct2),
          ifelse((abs(pct2) * 100 > 200 & abs(pct2) * 100 <= 500), (0.1558 + 1.0943 * pct2 - 0.0414 * pct2 * pct2),
            ifelse((abs(pct2) * 100 > 500 & abs(pct2) * 100 <= 1000), (0.6989 + 0.8846 * pct2 - 0.0207 * pct2 * pct2), 999)
          )
        )
      )
    )
  )
}

lifeyr6 <- function(pct2) {
  ifelse((abs(pct2) * 100 >= 0 & abs(pct2) * 100 <= 20), (0.8689 * pct2 - 0.0256 * pct2 * pct2),
    ifelse((abs(pct2) * 100 > 20 & abs(pct2) * 100 <= 50), (0.00003 + 0.8687 * pct2 - 0.0251 * pct2 * pct2),
      ifelse((abs(pct2) * 100 > 50 & abs(pct2) * 100 <= 100), (0.0003 + 0.8678 * pct2 - 0.0243 * pct2 * pct2),
        ifelse((abs(pct2) * 100 > 100 & abs(pct2) * 100 <= 200), (0.002 + 0.8647 * pct2 - 0.0228 * pct2 * pct2),
          ifelse((abs(pct2) * 100 > 200 & abs(pct2) * 100 <= 500), (0.0209 + 0.849 * pct2 - 0.0195 * pct2 * pct2),
            ifelse((abs(pct2) * 100 > 500 & abs(pct2) * 100 <= 1000), (0.1641 + 0.7958 * pct2 - 0.0145 * pct2 * pct2), 999)
          )
        )
      )
    )
  )
}

# functions for changes in life expectancy


valdist <- function(y, n, a1, a2, a3, a4, a5, normal_label = NULL, discrete_label = NULL, triangular_label = NULL) {
  nlab <- if (is.null(normal_label)) i18n$t("normal") else normal_label
  dlab <- if (is.null(discrete_label)) i18n$t("discrete") else discrete_label
  tlab <- if (is.null(triangular_label)) i18n$t("triangular") else triangular_label
  if (y == nlab) {
    return(rnorm(n, a1, a2))
  } else if (y == dlab) {
    return(sample(c(a1, a2, a3), size = n, replace = TRUE, prob = c(a4, a5, 1 - a4 - a5)))
  } else if (y == tlab) {
    return(rtri(n, as.double(a2), as.double(a3), as.double(a1)))
  }
} # loop for valuation distribution

# user interface (language from URL ?lang=fr for French; requires enableBookmarking("url") in server)
ui <- function(request = NULL) {
  lang <- "en"
  if (!is.null(request)) {
    qs <- if (is.environment(request)) request$QUERY_STRING else request[["QUERY_STRING"]]
    if (!is.null(qs) && nzchar(qs)) {
      query <- parseQueryString(qs)
      if (!is.null(query$lang) && query$lang %in% c("en", "fr")) lang <- query$lang
    }
  }
  # Set i18n for this request so UI strings (i18n$t) are correct when page is built. No other globals.
  currentlanguage <<- i18n$set_translation_language(lang)
  pmRefs <- if (lang == "en") pmReferences_en else pmReferences_fr
  otherRefs <- if (lang == "en") otherReferences_en else otherReferences_fr
  valuationRefs <- if (lang == "en") valuationReferences_en else valuationReferences_fr
  fluidPage(
    lang = lang, # root <html lang="..."> for screen readers (correct voice for FR/EN)
    # Hidden input: language for this session (set when UI is built; avoids URL reactive / cross-session issues)
    tags$div(style = "display: none;", textInput("session_lang", label = NULL, value = lang)),
    # add_loading_state(
    #   ".shiny-plot-output", # selector
    #   spinner = "circle",
    #   text = i18n$t("Please wait..."),
    #   timeout = 600,
    #   svgColor = "#383838",
    #   svgSize = "45px",
    #   messageColor = "#383838",
    #   messageFontSize = "14px",
    #   backgroundColor = "#ffffff"
    # ),
    title = i18n$t("AQBAT"),
    tags$head(
      tags$title(i18n$t("Air Quality Benefits Assessment Tool"), "|", i18n$t("Health Canada"), i18n$t("Infobase")),
      tags$script(src = "scripts/iframeResizer.contentWindow.min.js", type = "application/javascript"),
      tags$script(src = "scripts/save-load-url.js", type = "application/javascript"),
      tags$script(src = "scripts/utilities.js", type = "application/javascript"),
      tags$script(src = "scripts/jquery.magnific-popup.min.js", type = "application/javascript"),
      # tags$script(src = "scripts/handle-internal-anchor.js", type = "application/javascript"),
      tags$link(rel = "stylesheet", href = "styles/theme.min.css", type = "text/css"), # Canada.ca theming
      tags$link(rel = "stylesheet", href = "styles/aqbat.css", type = "text/css") # Custom styles
    ),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "reload"),

    # format error messages in bold red font
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
    ),
    tags$style(HTML("
    #shiny-disconnected-overlay {
      z-index: 2 !important;
    }
  ")),
    tags$body(class = "container hidden test"), # the container class from theme.min.css perfectly replicates the width of Canada.c
    # createDisconnectMessage(i18n),
    createErrorMessage("An error occurred. Please refresh the page and start again.", i18n),
    useShinyjs(),
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    createDisconnectNoticeModal(i18n$t("You have been disconnected from the server. All data has been lost. This can happen if the application has been updated on the server. Please refresh the page to restart the app. You may press F5 to refresh."), "disconnect-warning", i18n),
    createWarningModal(i18n$t("You are about to reset this application, along with all of its data."), "data-warning", i18n),
    tags$nav(
      class = "btn-tabs",
      tabsetPanel(
        id = "alltabpanel",
        tabPanel(
          i18n$t("Summary"),
          value = i18n$t("summary"),
          tabindex = "-1",
          summary_ui(i18n),
        ),
        tabPanel(
          i18n$t("CRFs"),
          value = i18n$t("crfs"),
          tabindex = "-1",
          createStepUI(i18n, stepNumber = 1, totalSteps = 4, progressMax = 4),
          p(i18n$t("Review concentration response functions (CRFs) and other parameters. The default values provided below are Health Canada-endorsed concentration-response functions to support the health impact assessment of air pollution. Change them as needed for your scenario by entering your desired values.")),
          tags$nav(
            h3(i18n$t("On this page")),
            tags$ul(
              tags$li(tags$a(
                i18n$t("Threshold concentration"),
                href = "#threshold",
              )),
              tags$li(tags$a(
                i18n$t("PM2.5 CRFs"),
                href = "#pm25",
              )),
              tags$li(tags$a(
                i18n$t("CRFs (O3, CO, SO2, NO2)"),
                href = "#crfs",
              )),
              tags$li(tags$a(
                i18n$t("Toxics"),
                href = "#toxics",
              )),
              tags$li(tags$a(
                i18n$t("Other parameters"),
                href = "#other",
              ))
            )
          ),
          h2(i18n$t("Threshold concentration"), id = "threshold"),
          p(i18n$t("Use this section to specify concentrations below which there is no association between air pollution and adverse health effects."), class = "crf-description"),
          tabindex = "0",
          useShinyjs(),
          createRestoreInstructions(i18n),
          p(i18n$t("The default value for each pollutant's threshold concentration is zero.")),
          actionButton("resetthreshold", i18n$t("Restore default values"), class = "btn-primary", tabindex = "0"),
          br(),
          br(),
          p(i18n$t("Click \"Clear all data and restart\" to restart the app for a new scenario. Data in all tabs will be reset to defaults. The default value for each pollutant is zero.")),
          createClearDataButton("crfs", i18n),
          br(),
          br(),
          div(
            id = "formthreshold",
            class = "parameter-box",
            fluidRow(
              column(4, numericInput("pmthr", i18n$t("PM2.5 (ug/m3)"), value = 0, min = 0, step = 0.001, max = 3)),
              column(4, numericInput("o3thr", i18n$t("O3 (ppb)"), 0, min = 0, step = 0.1, max = 30)),
              column(4, numericInput("summero3thr", i18n$t("Summer O3 (ppb)"), 0, min = 0, step = 0.1, max = 30))
            ),
            br(),
            fluidRow(
              column(4, numericInput("no2thr", i18n$t("NO2 (ppb)"), 0, min = 0, step = 0.001, max = 1)),
              column(4, numericInput("so2thr", i18n$t("SO2 (ppb)"), 0, min = 0, step = 0.001, max = 1)),
              column(4, numericInput("cothr", i18n$t("CO (ppm)"), 0, min = 0, step = 0.001, max = 1)),
            ),
          ),
          hr(),
          h2(i18n$t("PM2.5 CRFs"), id = "pm25"),
          p(i18n$t("This section provides concentration response functions (CRFs) for fine particulate matter (with a 10 ug/m3 change) and ten associated health endpoints. You may edit and use your own values."), class = "crf-description"),
          tabindex = "0",
          useShinyjs(),
          createRestoreInstructions(i18n),
          div(
            id = "formpm25",
            fluidRow(
              column(4, actionButton("resetpm25", i18n$t("Restore default values"), tabindex = "0", class = "btn-primary")),
              # column(4, numericInput("pmthr", "PM2.5 Threshold Concentration (ug/m3)", 0, min = 0, step=0.001,max = 2.5)),
            ),
            br(),
            fluidRow(
              column(
                12,
                p(i18n$t("\"L95%CI\" refers to the lower 95% confidence interval and \"U95%CI\" refers to the upper 95% confidence interval."), )
              )
            ),
            div(
              class = "grid-linear",
              h3(strong(i18n$t("Chronic exposure mortality")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      p(i18n$t("Crouse et al. 2012")),
                    ),
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      p(i18n$t("Log-linear")),
                      # hidden select with default choice log-linear
                      hidden(selectInput("rtype", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    ),
                  ),
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(6, numericInput("crf", i18n$t("HR/RR/OR"), 1.1, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("incr", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100)),
                ),
                fluidRow(
                  column(6, numericInput("l95", i18n$t("L95%CI"), 1.05, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("u95", i18n$t("U95%CI"), 1.15, min = 1, step = 0.01, max = 5)),
                ),
              ),
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Acute respiratory symptom days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Krupnick et al. 1990")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Linear")),
                      # Hidden select input for regression type, default choice set to linear
                      hidden(selectInput("pm25_rtype2", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf2", i18n$t("HR/RR/OR"), 1.0266, min = 0, step = 0.001, max = 5)),
                  column(6, numericInput("pm25_incr2", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_2", i18n$t("L95%CI"), 0.9994, min = 0.8, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_u95_2", i18n$t("U95%CI"), 1.0538, min = 0, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Adult chronic bronchitis cases")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Abbey et al. 1995")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype3", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf3", i18n$t("HR/RR/OR"), 1.14, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_incr3", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_3", i18n$t("L95%CI"), 1, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_u95_3", i18n$t("U95%CI"), 1.3, min = 1, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Asthma symptom days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Weinmayr et al. 2010; Ward and Ayres 2004; Dell et al. 2010")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype4", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf4", i18n$t("HR/RR/OR"), 1.0676, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("pm25_incr4", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_4", i18n$t("L95%CI"), 1.0137, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("pm25_u95_4", i18n$t("U95%CI"), 1.125, min = 1, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Cardiac emergency room visits")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Burnett et al. 1995; Stieb et al. 2000")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Linear")),
                      # Hidden select input for regression type, default choice set to linear
                      hidden(selectInput("pm25_rtype5", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf5", i18n$t("HR/RR/OR"), 1.00711, min = 1, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_incr5", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_5", i18n$t("L95%CI"), 1.00378, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_u95_5", i18n$t("U95%CI"), 1.0104, min = 0, step = 0.0001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Cardiac hospital admissions")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Burnett et al. 1995")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Linear")),
                      # Hidden select input for regression type, default choice set to linear
                      hidden(selectInput("pm25_rtype6", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf6", i18n$t("HR/RR/OR"), 1.00711, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_incr6", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_6", i18n$t("L95%CI"), 1.00378, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_u95_6", i18n$t("U95%CI"), 1.0104, min = 0, step = 0.0001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Child acute bronchitis episodes")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Heok et al. 2012; Dockery et al. 1996")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype7", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf7", i18n$t("HR/RR/OR"), 1.0934, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_incr7", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_7", i18n$t("L95%CI"), 0.977, min = 0.9, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_u95_7", i18n$t("U95%CI"), 1.224, min = 1, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Respiratory emergency room visits")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Burnett et al. 1995; Stieb et al. 2000")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype8", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf8", i18n$t("HR/RR/OR"), 1.00754, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_incr8", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_8", i18n$t("L95%CI"), 1.00495, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_u95_8", i18n$t("U95%CI"), 1.01013, min = 0, step = 0.0001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Respiratory hospital admissions")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Burnett et al. 1995")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype9", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf9", i18n$t("HR/RR/OR"), 1.00754, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_incr9", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_9", i18n$t("L95%CI"), 1.00495, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("pm25_u95_9", i18n$t("U95%CI"), 1.01013, min = 0, step = 0.0001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Restricted activity days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p("Ostro 1987"),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("pm25_rtype10", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("pm25_crf10", i18n$t("HR/RR/OR"), 1.05, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_incr10", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("pm25_l95_10", i18n$t("L95%CI"), 1.03, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("pm25_u95_10", i18n$t("U95%CI"), 1.07, min = 1, step = 0.01, max = 5))
                )
              )
            ),

            # chronic exposure lung cancer
            div(
              class = "grid-linear",
              h3(strong(i18n$t("Chronic exposure lung cancer mortality")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      p(paste(i18n$t("Health Canada"), "2022"))
                    ),
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      p(i18n$t("Log-linear")),
                      # hidden select with default choice log-linear
                      hidden(selectInput("rtypelung", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    ),
                  ),
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(6, numericInput("crflung", i18n$t("HR/RR/OR"), 1.127, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("incrlung", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100)),
                ),
                fluidRow(
                  column(6, numericInput("l95lung", i18n$t("L95%CI"), 1.085, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("u95lung", i18n$t("U95%CI"), 1.17, min = 1, step = 0.01, max = 5)),
                ),
              ),
            ),


            # Chronic exposure cerebrovascular mortality
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Chronic exposure cerebrovascular mortality")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    column(
                      12,
                      div(
                        class = "",
                        tags$label(i18n$t("Source"), class = "control-label"),
                        p(i18n$t("Shin et al. 2014")),
                      ),
                    ),
                    column(
                      12,
                      div(
                        class = "form-group shiny-input-container",
                        tags$label(i18n$t("Regression type"), class = "control-label"),
                        p("Gamma"),
                        hidden(selectInput("rtypecerebro", i18n$t("Regression type"), c("Gamma", "")))
                      ),
                    ),
                  ),
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("crfcerebro", i18n$t("Shape"), 4.884, min = 1, step = 0.01, max = 50)),
                column(12, numericInput("scalecerebro", i18n$t("Scale"), 0.03375, min = 0, step = 0.0001, max = 1)),
                column(12, numericInput("incrcerebro", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
              ),
            ),
            # Chronic exposure COPD mortality
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Chronic exposure COPD mortality")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    column(
                      12,
                      div(
                        class = "",
                        tags$label(i18n$t("Source"), class = "control-label"),
                        p(i18n$t("Shin et al. 2014")),
                      )
                    ),
                    column(
                      12,
                      div(
                        class = "form-group shiny-input-container",
                        tags$label(i18n$t("Regression type"), class = "control-label"),
                        p("Gamma"),
                        hidden(selectInput("rtypcopd", i18n$t("Regression type"), c("Gamma", "", "")))
                      )
                    )
                  )
                )
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("crfcopd", i18n$t("Shape"), 14.57, min = 1, step = 0.01, max = 150)),
                column(12, numericInput("scalecopd", i18n$t("Scale"), 0.00601, min = 0, step = 0.0001, max = 1)),
                column(12, numericInput("incrcopd", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
              )
            ),
            # Chronic exposure ischemic heart disease mortality
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Chronic exposure ischemic heart disease mortality")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    column(
                      12,
                      div(
                        class = "",
                        tags$label(i18n$t("Source"), class = "control-label"),
                        p(i18n$t("Shin et al. 2014")),
                      )
                    ),
                    column(
                      12,
                      div(
                        class = "form-group shiny-input-container",
                        tags$label(i18n$t("Regression type"), class = "control-label"),
                        p("Gamma"),
                        hidden(selectInput("rtypIschem", i18n$t("Regression type"), c("Gamma", "", "")))
                      )
                    )
                  )
                )
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("crfIschem", i18n$t("Shape"), 1.156, min = 1, step = 0.01, max = 50)),
                column(12, numericInput("scaleIschem", i18n$t("Scale"), 0.2117, min = 0, step = 0.0001, max = 1)),
                column(12, numericInput("incrIschem", i18n$t("Increment (ug/m3)"), 10, min = 1, step = 1, max = 100))
              )
            ),
            hr(),
          ),
          # CRFs (O3, CO, SO2, NO2)
          h2(i18n$t("CRFs (O3, CO, SO2, NO2)"), id = "crfs"),
          p(i18n$t("This section lists CRFs for ozone (O3), carbon monoxide (CO), sulfur dioxide (SO2), nitrogen dioxide (NO2), and eight associated health endpoints. You may edit and use your own values."), class = "crf-description"),
          p(i18n$t("Please note that the CRFs for acute exposure mortality associated with SO2 and CO are still considered 'suggestive' rather than 'causal' or 'likely causal'. We will continue to monitor and incorporate any new scientific evidence."), class = "crf-description"),
          tabindex = "0",
          useShinyjs(),
          div(
            id = "form2",
            createRestoreInstructions(i18n),
            fluidRow(
              column(4, actionButton("resetother", i18n$t("Restore default values"), class = "btn-primary", tabindex = "0")),
            ),
            br(),
            fluidRow(
              column(
                12,
                p(i18n$t("\"L95%CI\" refers to the lower 95% confidence interval and \"U95%CI\" refers to the upper 95% confidence interval."), )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("O3 and acute exposure mortality")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Burnett et al. 2004")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("o3_rtype1", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf1", i18n$t("HR/RR/OR"), 1.0084, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("o3_incr1", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_1", i18n$t("L95%CI"), 1.0057, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("o3_u95_1", i18n$t("U95%CI"), 1.011, min = 1, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and chronic exposure respiratory mortality")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Jerrett et al. 2009")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("o3_rtype2", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf2", i18n$t("HR/RR/OR"), 1.04, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("o3_incr2", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_2", i18n$t("L95%CI"), 1.0134, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("o3_u95_2", i18n$t("U95%CI"), 1.0672, min = 1, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and acute respiratory symptom days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Krupnick et al. 1990")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Linear")),
                      # Hidden select input for regression type, default choice set to linear
                      hidden(selectInput("o3_rtype3", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf3", i18n$t("HR/RR/OR"), 1.00786, min = 0, step = 0.0001, max = 5)),
                  column(6, numericInput("o3_incr3", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_3", i18n$t("L95%CI"), 1.0002944, min = 1, step = 0.0001, max = 5)),
                  column(6, numericInput("o3_u95_3", i18n$t("U95%CI"), 1.01543, min = 1, step = 0.0001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and asthma symptom days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Mortimer et al. 2002; Schildcrout et al. 2006")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("o3_rtype4", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf4", i18n$t("HR/RR/OR"), 1.0241, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("o3_incr4", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_4", i18n$t("L95%CI"), 0.9811, min = 0.9, step = 0.01, max = 5)),
                  column(6, numericInput("o3_u95_4", i18n$t("U95%CI"), 1.07, min = 1, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and minor restricted activity days")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  column(
                    12,
                    div(
                      class = "",
                      tags$label(i18n$t("Source"), class = "control-label"),
                      # Source information
                      p(i18n$t("Ostro and Rothschild 1989")),
                    )
                  ),
                  column(
                    12,
                    div(
                      class = "form-group shiny-input-container",
                      tags$label(i18n$t("Regression type"), class = "control-label"),
                      # Regression type information
                      p(i18n$t("Log-linear")),
                      # Hidden select input for regression type, default choice set to log-linear
                      hidden(selectInput("o3_rtype5", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear")))),
                    )
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf5", i18n$t("HR/RR/OR"), 1.0053, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("o3_incr5", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_5", i18n$t("L95%CI"), 0.95, min = 0.9, step = 0.001, max = 5)),
                  column(6, numericInput("o3_u95_5", i18n$t("U95%CI"), 1.0643, min = 1, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and respiratory emergency room visits")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    # Source information
                    p(i18n$t("Burnett et al. 1997a; Stieb et al. 2000")),
                    hidden(textInput("o3_source6", i18n$t("Source"), value = "Burnett et al. 1997a; Stieb et al. 2000"))
                  ),
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Regression type"), class = "control-label"),
                    # Regression type information
                    p(i18n$t("Log-linear")),
                    # Hidden select input for regression type, default choice set to log-linear
                    hidden(selectInput("o3_rtype6", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear"))))
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf6", i18n$t("HR/RR/OR"), 1.008, min = 0.8, step = 0.001, max = 5)),
                  column(6, numericInput("o3_incr6", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_6", i18n$t("L95%CI"), 1.001, min = 0.8, step = 0.001, max = 5)),
                  column(6, numericInput("o3_u95_6", i18n$t("U95%CI"), 1.015, min = 0.9, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("Summer O3 and respiratory hospital admissions")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    # Source information
                    p(i18n$t("Burnett et al. 1997a")),
                    hidden(textInput("o3_source7", i18n$t("Source"), value = "Burnett et al. 1997a"))
                  ),
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Regression type"), class = "control-label"),
                    # Regression type information
                    p(i18n$t("Log-linear")),
                    # Hidden select input for regression type, default choice set to log-linear
                    hidden(selectInput("o3_rtype7", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear"))))
                  )
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("o3_crf7", i18n$t("HR/RR/OR"), 1.008, min = 0.8, step = 0.001, max = 5)),
                  column(6, numericInput("o3_incr7", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("o3_l95_7", i18n$t("L95%CI"), 1.0003, min = 0.8, step = 0.001, max = 5)),
                  column(6, numericInput("o3_u95_7", i18n$t("U95%CI"), 1.015, min = 0.9, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("NO2 and acute exposure mortality")), class = "grid-heading"),
              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  # Source information
                  p(i18n$t("Burnett et al. 2004")),
                  hidden(textInput("no2_out", i18n$t("Source"), value = "Burnett et al. 2004"))
                ),
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Regression type"), class = "control-label"),
                  # Regression type information
                  p(i18n$t("Log-linear")),
                  # Hidden select input for regression type, default choice set to log-linear
                  hidden(selectInput("no2_rtype", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear"))))
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("no2_crf", i18n$t("HR/RR/OR"), 1.0075, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("no2_incr", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("no2_l95", i18n$t("L95%CI"), 1.0026, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("no2_u95", i18n$t("U95%CI"), 1.0124, min = 1, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("SO2 and acute exposure mortality")), class = "grid-heading"),

              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  # Source information
                  p(i18n$t("Burnett et al. 2004")),
                  hidden(textInput("so2_out", i18n$t("Source"), value = "Burnett et al. 2004"))
                ),
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Regression type"), class = "control-label"),
                  # Regression type information
                  p(i18n$t("Log-linear")),
                  # Hidden select input for regression type, default choice set to log-linear
                  hidden(selectInput("so2_rtype", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear"))))
                )
              ),

              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("so2_crf", i18n$t("HR/RR/OR"), 1.0046, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("so2_incr", i18n$t("Increment (ppb)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("so2_l95", i18n$t("L95%CI"), 1.00028, min = 1, step = 0.001, max = 5)),
                  column(6, numericInput("so2_u95", i18n$t("U95%CI"), 1.00894, min = 1, step = 0.001, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("24h-CO and acute exposure mortality")), class = "grid-heading"),

              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  # Source information
                  p(i18n$t("Burnett et al. 2004")),
                  hidden(textInput("co24_source", i18n$t("Source"), value = "Burnett et al. 2004"))
                ),
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Regression type"), class = "control-label"),
                  # Regression type information
                  p(i18n$t("Log-linear")),
                  # Hidden select input for regression type, default choice set to log-linear
                  hidden(selectInput("co24_rtype", i18n$t("Regression type"), c(i18n$t("log-linear"), i18n$t("linear"))))
                )
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("co24_crf", i18n$t("HR/RR/OR"), 1.0192, min = 1, step = 0.01, max = 5)),
                  column(6, numericInput("co24_incr", i18n$t("Increment (ppm)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("co24_l95", i18n$t("L95%CI"), 0.969, min = 0.9, step = 0.01, max = 5)),
                  column(6, numericInput("co24_u95", i18n$t("U95%CI"), 1.072, min = 1, step = 0.01, max = 5))
                )
              )
            ),
            div(
              class = "grid-linear",
              # Title Header
              h3(strong(i18n$t("1h-CO and elderly cardiac hospital admissions")), class = "grid-heading"),

              # Left parameter box containing Source and Regression type
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  # Source information
                  p(i18n$t("Burnett et al. 1997b; Schwartz and Morris 1995")),
                  hidden(textInput("co1_source", i18n$t("Source"), value = "Burnett et al. 1997b; Schwartz and Morris 1995"))
                ),
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Regression type"), class = "control-label"),
                  # Regression type information
                  p(i18n$t("Linear")),
                  # Hidden select input for regression type, default choice set to linear
                  hidden(selectInput("co1_rtype", i18n$t("Regression type"), c(i18n$t("linear"), i18n$t("log-linear"))))
                )
              ),

              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(6, numericInput("co1_crf", i18n$t("HR/RR/OR"), 1.277, min = 0, step = 0.001, max = 5)),
                  column(6, numericInput("co1_incr", i18n$t("Increment (ppm)"), 10, min = 1, step = 1, max = 100))
                ),
                # Second row with L95%CI and U95%CI inputs
                fluidRow(
                  column(6, numericInput("co1_l95", i18n$t("L95%CI"), 1.02612, min = 0, step = 0.001, max = 5)),
                  column(6, numericInput("co1_u95", i18n$t("U95%CI"), 1.5279, min = 0, step = 0.001, max = 5))
                )
              )
            ),
          ),
          hr(),
          h2(i18n$t("Toxics"), id = "toxics"),
          p(i18n$t("This section provides reference concentrations for benzene, formaldehyde, and acetaldehyde."), class = "crf-description"),
          tabindex = "0",
          useShinyjs(),
          createRestoreInstructions(i18n),
          div(
            id = "form3",
            fluidRow(
              column(4, actionButton("resetother1", i18n$t("Restore default values"), class = "btn-primary", tabindex = "0")),
            ),
            br(),
            fluidRow(
              column(
                12,
                p(i18n$t("\"DALYs\" refers to disability-adjusted life years."), )
              )
            ),
            div(
              class = "grid-linear mt-32",
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  p(i18n$t("Health Canada 2015")), # Updated source
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Pollutant"), class = "control-label"),
                  p(i18n$t("1,3-Butadiene")), # Updated pollutant
                  hidden(textInput("btname1", "Pollutant", value = i18n$t("1,3-Butadiene")))
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Outcome"), class = "control-label"),
                  p("Cancer"), # Updated outcome
                  hidden(textInput("btoutcome1", "Outcome", value = "Cancer"))
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(12, numericInput("btiur", i18n$t("Inhalation unit risk per ug/m3"), 0.00000588, min = 0, step = 0.0000001, max = 0.00001)),
                  column(12, numericInput("btdaly", i18n$t("DALYs per case"), 13.7, min = 0, step = 0.1, max = 50)),
                ),
              ),
            ),
            div(
              class = "grid-linear mt-32",
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  # Regression type information
                  p(i18n$t("Health Canada 2010")),
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Pollutant"), class = "control-label"),
                  # Source information
                  p(i18n$t("Benzene")),
                  hidden(textInput("bzname1", "Pollutant", value = i18n$t("Benzene")))
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Outcome"), class = "control-label"),
                  # Source information
                  p("Cancer"),
                  hidden(textInput("bzoutcome1", "Outcome", value = "Cancer"))
                ),
              ),
              # Right parameter box containing numeric inputs
              div(
                class = "parameter-box right-parameter-box",
                # First row with HR/RR/OR and Increment inputs
                fluidRow(
                  column(12, numericInput("bziur", i18n$t("Inhalation unit risk per ug/m3"), 0.0000033, min = 0, step = 0.0000001, max = 0.00001)),
                  column(12, numericInput("bzdaly", i18n$t("DALYs per case"), 13.7, min = 0, step = 0.1, max = 50)),
                ),
              ),
            ),
            div(
              class = "grid-linear mt-32",
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  p(i18n$t("California OEHHA 2016")), # Updated source
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Pollutant"), class = "control-label"),
                  p(i18n$t("Benzene")), # Updated pollutant
                  hidden(textInput("bzname2", "Pollutant", value = i18n$t("Benzene")))
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Outcome"), class = "control-label"),
                  p(i18n$t("Hematological")), # Updated outcome
                  hidden(textInput("bzoutcome2", "Outcome", value = i18n$t("Hematological")))
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(12, numericInput("bzanrfc", i18n$t("Reference concentration (annual ug/m3)"), 3, min = 0, step = 1, max = 1000)),
                ),
              ),
            ),
            div(
              class = "grid-linear mt-32",
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  p(i18n$t("Health Canada 2016a")), # Updated source
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Pollutant"), class = "control-label"),
                  p(i18n$t("Formaldehyde")), # Updated pollutant
                  hidden(textInput("fmname2", "Pollutant", value = i18n$t("Formaldehyde")))
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Outcome"), class = "control-label"),
                  p(i18n$t("Respiratory (asthma)")), # Updated outcome
                  hidden(textInput("fmoutcome2", "Outcome", value = i18n$t("Respiratory (asthma)")))
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(12, numericInput("fmanrfc", i18n$t("Reference concentration (annual ug/m3)"), 50, min = 0, step = 1, max = 1000)),
                ),
              ),
            ),
            div(
              class = "grid-linear mt-32",
              div(
                class = "parameter-box left-parameter-box",
                div(
                  class = "form-group shiny-input-container",
                  tags$label(i18n$t("Source"), class = "control-label"),
                  p(i18n$t("Health Canada 2016b")), # Updated source
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Pollutant"), class = "control-label"),
                  p(i18n$t("Acetaldehyde")), # Updated pollutant
                  hidden(textInput("acname2", "Pollutant", value = i18n$t("Acetaldehyde")))
                ),
                div(
                  class = "form-group",
                  tags$label(i18n$t("Outcome"), class = "control-label"),
                  p(i18n$t("Respiratory (histological)")), # Updated outcome
                  hidden(textInput("acoutcome2", "Outcome", value = i18n$t("Respiratory (histological)")))
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                fluidRow(
                  column(12, numericInput("acanrfc", i18n$t("Reference concentration (annual ug/m3)"), 280, min = 0, step = 1, max = 1000)),
                ),
              ),
            ),
          ), # div3
          hr(),
          h2(i18n$t("Other parameters"), id = "other"),
          p(i18n$t("This section lists the:"), class = "crf-description"),
          tags$ul(
            tags$li(i18n$t("prevalence of asthma for those aged < 20 years (default is 17.7%)")),
            tags$li(i18n$t("numbers of iterations to be used for Monte Carlo simulations (default is 10,000 for simulation iterations)")),
          ),
          fluidRow(
            column(
              12,
              p(i18n$t("You may edit and use your own values."))
            )
          ),
          createRestoreInstructions(i18n),
          fluidRow(
            column(4, actionButton("resetall3", i18n$t("Restore default values"), class = "btn-primary", tabindex = "0")),
          ),
          br(),
          useShinyjs(),
          div(
            id = "form4",
            class = "parameter-box",
            style = "margin-bottom: 32px;",
            fluidRow(
              column(6, numericInput("asprev", i18n$t("Prevalence of asthma (age < 20 years)"), 17.7, min = 1, step = 1, max = 25, width = "100%")),
              column(6, numericInput("itn", i18n$t("Iterations"), 10000, min = 1, step = 100, max = 15000, width = "100%")),
              textOutput("pasthma"),
            )
          ), # div4
        ),
        tabPanel(
          i18n$t("Valuation"),
          value = i18n$t("valuation"),
          createStepUI(i18n, stepNumber = 2, totalSteps = 4, progressMax = 4),
          p(i18n$t("Review the economic valuation estimates. The default values provided below are Health Canada-endorsed economic valuation estimates to support the health impact assessment of air pollution. Change them as needed for your scenario by entering your desired values.")),
          # actionLink("link_instruction5", "Click here to view details about the 'CRFs' tab under the 'Instructions'."),
          # br(),
          tags$h3(HTML(i18n$t("About valuation estimates"))),
          p(i18n$t("These values are economic welfare values. AQBAT provides economic valuation estimates that consider the potential welfare impacts associated with treatment costs, lost productivity, pain and suffering, and the impacts of increased mortality risk. The page displays corresponding economic values associated with health risks identified by AQBAT."), class = ""),
          HTML(i18n$t("Currency year ranges from 2010 to 2023. To learn more about the sources included in this page, please refer to the <a id='references-link' class='internal-link' data-target='references' href='javascript:void(0)'>references</a>.")),
          br(),
          br(),
          # Discount rate
          # actionLink("link_instruction3", "Click here to view details about the 'Valuation' tab under the 'Instructions'."),
          # hr(),
          p(i18n$t("Click \"Restore default values\" to restore the values of each cell back to its preset (the values that show up automatically when the website first loads). Default values are restored in this tab only.")),
          useShinyjs(),
          div(
            id = "formval",
            style = "margin-top: 20px;",
            fluidRow(
              column(3, actionButton("resetval", i18n$t("Restore default values"), class = "btn-primary")),
              column(3, numericInput("curr", i18n$t("Currency year"), 2016, min = 2010, step = 1, max = 2023)),
              column(3, numericInput("baseyr", i18n$t("Base year"), 2016, min = 1990, step = 1, max = 2023)),
              column(3, numericInput("discountrate", i18n$t("Discount rate (%)"), 0, min = 0, step = 1, max = 30)),
              textOutput("curryear")
            ),
            div(
              class = "grid-discrete",
              h3(strong(i18n$t("Mortality valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Chestnut and DeCivita 2009")),
                  ),
                ),
                column(12, textInput("vslyr", i18n$t("Source year"), value = "2007")),
                # column(3, textInput("vslfm", i18n$t("Estimate form"), c("discrete"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Discrete")),
                    hidden(selectInput("vslfm", NULL, c(i18n$t("discrete")), selected = i18n$t("discrete")))
                  ),
                ),
              ),
              div(
                class = "parameter-box center-parameter-box",
                column(12, numericInput("vsl", i18n$t("Central ($million)"), 6.5, min = 1, step = 0.5, max = 100)),
                column(12, numericInput("lvsl", i18n$t("Low ($million)"), 3.5, min = 0.5, step = 0.5, max = 100)),
                column(12, numericInput("uvsl", i18n$t("High ($million)"), 9.5, min = 7, step = 0.5, max = 100)),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("pcvsl", i18n$t("Probability central"), 0.5, min = 0.1, step = 0.1, max = 1)),
                column(12, numericInput("plvsl", i18n$t("Probability low"), 0.25, min = 0.1, step = 0.1, max = 0.9)),
              ),
            ),
            # Acute Respiratory Symptom Days$
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Acute respiratory symptom days valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("vsl2yr", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("vsl2fm", i18n$t("Estimate form"), c("normal"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("vsl2fm", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl2", i18n$t("Mean ($)"), 13, min = 1, step = 0.5, max = 100)),
                column(12, numericInput("lvsl2", i18n$t("Standard error"), 7, min = 0, step = 0.5, max = 100))
              ),
            ),
            # Adult Chronic Bronchitis Cases$
            div(
              class = "grid-discrete",
              h3(strong(i18n$t("Adult chronic bronchitis cases valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Krupnick and Cropper 1992; Viscusi et al. 1991")),
                  ),
                ),
                column(12, textInput("vsl3yr", i18n$t("Source year"), value = "1996")),
                # column(3, selectInput("vsl3fm", i18n$t("Estimate form"), c("discrete"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Discrete")),
                    hidden(selectInput("vsl3fm", NULL, c(i18n$t("discrete")), selected = i18n$t("discrete")))
                  )
                ),
              ),
              div(
                class = "parameter-box center-parameter-box",
                column(12, numericInput("vsl3", i18n$t("Central ($1000)"), 266, min = 1, step = 0.5, max = 1000)),
                column(12, numericInput("lvsl3", i18n$t("Low ($1000)"), 175, min = 0.5, step = 0.5, max = 1000)),
                column(12, numericInput("uvsl3", i18n$t("High ($1000)"), 465, min = 7, step = 0.5, max = 1000)),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("pcvsl3", i18n$t("Probability central"), 0.34, min = 0.1, step = 0.1, max = 1)),
                column(12, numericInput("plvsl3", i18n$t("Probability low"), 0.33, min = 0.1, step = 0.1, max = 0.9)),
              ),
            ),
            # Asthma Symptom Days $
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Asthma symptom days valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl4b", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("sourcevsl4c", i18n$t("Estimate form"), c("triangular"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Triangular")),
                    hidden(selectInput("sourcevsl4c", NULL, c(i18n$t("triangular")), selected = i18n$t("triangular")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl4", i18n$t("Likely ($)"), 28, min = 1, step = 0.5, max = 100)),
                column(12, numericInput("lvsl4", "Min ($)", 7, min = 0.5, step = 0.5, max = 100)),
                column(12, numericInput("uvsl4", "Max ($)", 120, min = 7, step = 0.5, max = 300)),
              ),
            ),
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Cardiac emergency room visits valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl5b", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("sourcevsl5c", i18n$t("Estimate form"), c("normal"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("sourcevsl5c", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl5", i18n$t("Mean ($)"), 4400, min = 1, step = 0.5, max = 10000)),
                column(12, numericInput("lvsl5", i18n$t("Standard error"), 590, min = 0.5, step = 0.5, max = 10000)),
              ),
            ),
            # hr(),
            # Child Acute Bronchitis Episodes$
            div(
              class = "grid-discrete",
              h3(strong(i18n$t("Child acute bronchitis episodes valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Krupnick and Cropper 1989")),
                  ),
                ),
                column(12, textInput("sourcevsl6b", i18n$t("Source year"), value = "1996")),
                # column(3, selectInput("sourcevsl6c", i18n$t("Estimate form"), c("discrete"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Discrete")),
                    hidden(selectInput("sourcevsl6c", NULL, c(i18n$t("discrete")), selected = i18n$t("discrete")))
                  )
                ),
              ),
              div(
                class = "parameter-box center-parameter-box",
                column(12, numericInput("vsl6", i18n$t("Central ($)"), 310, min = 1, step = 0.5, max = 1000)),
                column(12, numericInput("lvsl6", i18n$t("Low ($)"), 150, min = 0.5, step = 0.5, max = 1000)),
                column(12, numericInput("uvsl6", i18n$t("High ($)"), 460, min = 7, step = 0.5, max = 1000)),
              ),
              div(
                class = "parameter-box center-parameter-box",
                column(12, numericInput("pcvsl6", i18n$t("Probability central"), 0.34, min = 0.1, step = 0.1, max = 1)),
                column(12, numericInput("plvsl6", i18n$t("Probability low"), 0.33, min = 0.1, step = 0.1, max = 0.9))
              ),
            ),
            # Elderly Cardiac Hospital Admissions $
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Elderly cardiac hospital admissions valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl7b", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("sourcevsl7c", i18n$t("Estimate form"), c("normal"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("sourcevsl7c", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl7", i18n$t("Mean ($)"), 5200, min = 1, step = 5, max = 10000)),
                column(12, numericInput("lvsl7", i18n$t("Standard error"), 610, min = 5, step = 5, max = 10000))
              )
            ),
            # hr(),
            # Minor Restricted Activity Days$
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Minor restricted activity days valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl8b", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("sourcevsl8c", i18n$t("Estimate form"), c("normal"))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("sourcevsl8c", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl8", i18n$t("Mean ($)"), 22, min = 1, step = 5, max = 1000)),
                column(12, numericInput("lvsl8", i18n$t("Standard error"), 9, min = 5, step = 5, max = 1000))
              ),
            ),
            # hr(),
            # Respiratory Emergency Room Visits$
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Respiratory emergency room visits valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl9b", i18n$t("Source year"), value = "1997")),
                # column(3, selectInput("sourcevsl9c", i18n$t("Estimate form"), c(i18n$t("normal")))),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("sourcevsl9c", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl9", i18n$t("Mean ($)"), 2000, min = 10, step = 5, max = 10000)),
                column(12, numericInput("lvsl9", i18n$t("Standard error"), 210, min = 5, step = 5, max = 10000))
              ),
            ),
            # hr(),
            div(
              class = "grid-normal",
              h3(strong(i18n$t("Restricted activity days valuation")), class = "grid-heading"),
              div(
                class = "parameter-box left-parameter-box",
                column(
                  12,
                  div(
                    class = "",
                    tags$label(i18n$t("Source"), class = "control-label"),
                    p(i18n$t("Stieb et al. 2002")),
                  ),
                ),
                column(12, textInput("sourcevsl10b", i18n$t("Source year"), value = "1997")),
                column(
                  12,
                  div(
                    class = "form-group shiny-input-container",
                    tags$label(i18n$t("Estimate form"), class = "control-label"),
                    p(i18n$t("Normal")),
                    hidden(selectInput("sourcevsl10c", NULL, c(i18n$t("normal")), selected = i18n$t("normal")))
                  )
                ),
              ),
              div(
                class = "parameter-box right-parameter-box",
                column(12, numericInput("vsl10", i18n$t("Mean ($)"), 48, min = 1, step = 5, max = 1000)),
                column(12, numericInput("lvsl10", i18n$t("Standard error"), 18, min = 5, step = 5, max = 1000))
              ),
            ), # div4
          )
        ),
        tabPanel(
          i18n$t("Pollutant data upload"),
          value = i18n$t("pollutant-data-upload"),
          createStepUI(i18n, stepNumber = 3, totalSteps = 4, progressMax = 4),
          p(i18n$t("Upload your pollutant data by clicking \"Upload\" to browse your file system and select files. A preview of your upload will appear below.")),
          p(i18n$t("Your uploaded data is temporary and will not be saved for future visits or stored on your device.")),
          p(i18n$t("Only CSV files are supported. UTF-8 encoding is recommended.")),
          div(
            tags$section(
              id = "dataUploadedInfo",
              style = "margin-bottom: 0",
              role = "alert",
              class = "alert alert-info hidden",
              tags$h3(id = "dataUploadedHeader", i18n$t("Sample data has been uploaded")), # Header for dynamic content
              p(id = "dataUploadedInstruction", i18n$t('To upload new data, please click \"Clear all data and restart\".')) # Instruction text
            )
          ),
          fileInput("pollutants", label = "", accept = c(".csv", ".tsv"), multiple = TRUE, width = "350px"),
          textOutput("message1"),
          h3(i18n$t("Preview of the uploaded data"), id = "data-preview", class = "hidden"),
          tableOutput("dataPreviewTable"),
          p(i18n$t("If you have previously uploaded data (including the sample data), please clear all data and restart the app before uploading new pollutant data. You will return to the \"CRFs\" tab of the website. Data in all tabs in and results will be reset to defaults.")),
          createClearDataButton("upload", i18n),
          h3(i18n$t("Instructions for new users")),
          p(i18n$t("Pollutant concentrations are annual averages of daily values (24 hours), except for O3 and summer O3 (May to September), which use daily 1-hour maximum averages for the annual and summer periods, respectively. CO includes both annual daily averages and daily 1-hour maximum averages."), class = ""),
          p(i18n$t("Before using AQBAT, you need to prepare your pollutant data so you can upload it onto the tool. We provide a sample file you can use to help structure your data for upload. It includes sample air quality data for PM2.5, O3, and NO2 for 2016. These data were derived from multiple national sources and mapped to 293 census divisions in Canada. We use data from the sample file if you do not input your own data."), class = ""),
          if (lang == "fr") {
            downloadButton("xsample_fr", i18n$t("Download sample input file"), class = "btn-primary")
          } else {
            downloadButton("xsample_en", i18n$t("Download sample input file"), class = "btn-primary")
          },
          br(),
          br(),
          p(i18n$t("Please ensure that the formatting of your data matches the sample data provided. The following variables are included in the sample data:")),
          tags$div(
            tags$ul(
              tags$li(i18n$t("year (2001-2063)")),
              tags$li(i18n$t("scenario")),
              tags$li(i18n$t("census division unique identifier (CDUID)")),
              tags$li(i18n$t("fine particulate matter (pm25_1 and pm25_2)")),
              tags$li(i18n$t("nitrogen dioxide (no2_1 and no2_2)")),
              tags$li(i18n$t("ozone (o3_1 and o3_2)")),
              tags$li(i18n$t("summer ozone (summero3_1 and summero3_2)")),
              tags$li(i18n$t("24-hour carbon monoxide (co24h_1 and co24h_2)")),
              tags$li(i18n$t("1-hour carbon monoxide (co1h_1 and co1h_2)")),
              tags$li(i18n$t("sulfur dioxide (so2_1 and so2_2)")),
              tags$li(i18n$t("benzene (bz_1 and bz_2)")),
              tags$li(i18n$t("1,3-butadiene (bt_1 and bt_2)")),
              tags$li(i18n$t("acetaldehyde (ac_1 and ac_2)")),
              tags$li(i18n$t("formaldehyde (fm_1 and fm_2)"))
            )
          ),
          p(i18n$t("The \"year\", \"scenario\", and \"CDUID\" columns are mandatory, as well as a minimum of one type of pollutant (status quo and counterfactual concentrations). Both the \"year\" and \"scenario\" columns need to be numeric variables. There cannot be repetitive scenario years and numbers in combination. For example, if the year is 2016, each scenario must have a different number.")),
          p(i18n$t("AQBAT uses the difference between status quo and counterfactual concentrations to estimate health benefits or damages from air quality changes. In the sample file:")),
          tags$div(
            tags$ul(
              tags$li(i18n$t("columns with \"_2\" in the header are status quo concentrations (for example, \"pm25_2\")")),
              tags$li(i18n$t("columns with \"_1\" in the header are counterfactual concentrations, like natural background concentrations (for example, \"pm25_1\")"))
            )
          ),
          tags$script(HTML("
          // Listen for Shiny messages to enable/disable input
          Shiny.addCustomMessageHandler('toggleFileInput', function(enable) {
            if (enable) {
              $('#pollutants').parent().removeClass('disabled');
              $('#pollutants').prop('disabled', false);
            } else {
              $('#pollutants').parent().addClass('disabled');
              $('#pollutants').prop('disabled', true);
            }
          });
        ")),
        ),
        tabPanel(
          i18n$t("Results"),
          value = i18n$t("results"),
          createStepUI(i18n, stepNumber = 4, totalSteps = 4, progressMax = 4),
          p(i18n$t("View and download the results of your scenario. Details on the methodology for how AQBAT estimates human health effects and the associated economic value are provided in the AQBAT appendix.")),
          downloadButton("download_aqbat_appendix_en", i18n$t("Download AQBAT appendix"), class = "btn-primary", style = "display: none;"),
          downloadButton("download_aqbat_appendix_fr", i18n$t("Télécharger l'annexe d'OEBQA"), class = "btn-primary", style = "display: none;"),
          tags$nav(
            h3(i18n$t("On this page")),
            tags$ul(
              tags$li(tags$a(
                i18n$t("Overall results"),
                href = "#overall",
              )),
              # tags$li(tags$a(
              #   i18n$t("Maps"),
              #   href = "#maps",
              #   class = "scroll-link"
              # )),
              tags$li(tags$a(
                i18n$t("Population-weighted exposure"),
                href = "#population-weighted",
              )),
              tags$li(tags$a(
                i18n$t("Cause-specific mortality"),
                href = "#cause-specific",
              )),
              tags$li(tags$a(
                i18n$t("Non-linear shape constrained health impact function (SCHIF)"),
                href = "#schif",
              )),
              tags$li(tags$a(
                i18n$t("Toxics"),
                href = "#toxics-results",
              )),
              tags$li(tags$a(
                i18n$t("Input parameters"),
                href = "#input-params",
              )),
              tags$li(tags$a(
                i18n$t("Baseline data"),
                href = "#baseline",
              ))
            )
          ),
          h3("Instructions"),
          p(i18n$t("You must specify a currency year in the \"Valuation\" tab and import pollutant data in the \"Pollutant data upload\" tab to get results. The default results based on sample pollutant data will be generated when no pollutant data is uploaded.")),
          p(i18n$t("Click \"Clear all data and restart\" to restart the app for a new scenario. You will return to the \"CRFs\" tab of the website. Data in all tabs and results will be reset to defaults.")),
          br(),
          div(
            tags$section(
              id = "sampleDataUploadedInfo",
              style = "margin-bottom: 0",
              class = "alert alert-info hidden",
              h3(i18n$t("Sample data has been uploaded")),
              p(i18n$t('To upload your own data, please click "Clear all data and restart", and then upload your data at the "Pollutant data upload" tab before visiting this page')),
            ),
          ),
          br(),
          createClearDataButton("results", i18n),

          # OVERALL RESULTS
          tags$section(
            class = "section",
            h2(i18n$t("Overall results"), id = "overall"),
            p(i18n$t("Results at census division, provincial and territorial levels along with national summaries. Please note that the table below is just a preview of the output. To see the full results, please select \"Download overall results\".")),
            p(i18n$t("\"L95CI\" refers to the lower 95% confidence interval and \"U95CI\" refers to the upper 95% confidence interval.")),
            p(i18n$t("\"CDUID\" refers to the census division unique identifier.")),
            tableOutput("outputcd"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputcd"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d2", i18n$t("Download overall results"), class = "download-button btn-primary"),
          ),
          # MAPS
          # tags$section(
          #   class = "section",
          #   h2(i18n$t("Maps"), id="maps"),
          #   p(i18n$t("Interactive and static maps of baseline health outcome rates, status quo pollutant concentrations and selected results.")),
          #   value = i18n$t("Maps"),
          #   uiOutput("xmap")
          # ),
          # POPULATION-WEIGHTED EXPOSURE
          tags$section(
            class = "section",
            h2(i18n$t("Population-weighted exposure"), id = "population-weighted"),
            p(i18n$t("An estimate of population-weighted average air pollutant exposure nationally and by province. Please note that the table below is just a preview of the output. To see the full results, please select \"Download population-weighted exposure results\".")),
            tableOutput("outputexposure"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputexposure"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d5", i18n$t("Download population-weighted exposure results"), class = "download-button btn-primary")
          ),
          # CAUSE-SPECIFIC MORTALITY
          tags$section(
            class = "section",
            h2(i18n$t("Cause-specific mortality"), id = "cause-specific"),
            p(i18n$t("The results for four cause-specific chronic exposure type mortalities, including chronic exposure cerebrovascular mortality, chronic exposure COPD mortality, chronic exposure ischemic heart disease mortality, and chronic exposure lung cancer mortality. Please note that the table below is just a preview of the output. To see the full results, please select \"Download cause-specific mortality results\".")),
            p(i18n$t("\"L95CI\" refers to the lower 95% confidence interval and \"U95CI\" refers to the upper 95% confidence interval.")),
            tableOutput("outputmort"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputmort"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d3", i18n$t("Download cause-specific mortality results"), class = "download-button btn-primary")
          ),
          # NON-LINEAR SCHIF
          tags$section(
            class = "section",
            h2(i18n$t("Non-linear shape constrained health impact function (SCHIF)"), id = "schif"),
            p(i18n$t("The results for mortality related to long term exposure to PM2.5. Please note that the table below is just a preview of the output. To see the full results, please select \"Download non-linear shape constrained health impact function results\".")),
            p(i18n$t("\"L95CI\" refers to the lower 95% confidence interval and \"U95CI\" refers to the upper 95% confidence interval.")),
            tableOutput("outputschif"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputschif"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d4", i18n$t("Download non-linear shape constrained health impact function results"), class = "download-button btn-primary")
          ),
          # TOXICS
          tags$section(
            class = "section",
            h2(i18n$t("Toxics"), id = "toxics-results"),
            p(
              i18n$t("Results for cancer and non-cancer outcomes of air toxics."),
              i18n$t("Please note that the table below is just a preview of the output. To see the full results, please select \"Download toxics results\". If you would like to view non-cancer outcomes, please click on the \"Non-cancer\" sheet within the downloaded table.")
            ),
            tableOutput("outputtox"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputtox"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d6", i18n$t("Download toxics results"), class = "download-button btn-primary")
          ),
          # INPUT PARAMETERS
          tags$section(
            class = "section",
            h2(i18n$t("Input parameters"), id = "input-params"),
            p(i18n$t("A list of all inputs defining your modelled scenario. Please note that the table below is just a preview of the output. To see the full results, please select \"Download input parameters\".")),
            tableOutput("allinputa"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "allinputa"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("allinputb", i18n$t("Download input parameters"), class = "download-button btn-primary")
          ),
          # BASELINE DATA
          tags$section(
            class = "section",
            h2(i18n$t("Baseline data"), id = "baseline"),
            p(i18n$t("Annual baseline rates for health outcomes (per million). Please note that the table below is just a preview of the output. To see the full results, including population sizes, please select \"Download baseline data\".")),
            tableOutput("outputbaserate"),
            createErrorMessage("An error occurred. Please refresh the page and start again.", i18n, "outputbaserate"),
            createLoader(loadingMessage = i18n$t("Loading...")),
            downloadButton("d7", i18n$t("Download baseline data"), class = "download-button btn-primary")
          ),
        ),
        tabPanel(
          i18n$t("References"),
          value = i18n$t("references"),
          h2(i18n$t("References")),
          p(i18n$t("These selected references are cited as key sources within the \"CRFs\" and \"Valuation\" tabs. Select any of the links for more information on that specific reference. Please note that selected links will open in the same window and you will not be able to return to your scenario. If you wish to avoid this, please open links in a new window or tab.")),
          tags$nav(
            h3(i18n$t("On this page")),
            tags$ul(
              tags$li(tags$a(
                i18n$t("PM2.5 CRFs"),
                href = "#PM25-CRFs",
              )),
              tags$li(tags$a(
                i18n$t("Other CRFs"),
                href = "#others",
              )),
              tags$li(tags$a(
                i18n$t("Valuation estimates"),
                href = "#value",
              ))
            )
          ),
          h2(i18n$t("PM2.5 CRFs"), id = "PM25-CRFs"),
          lapply(1:nrow(pmRefs), function(i) {
            tags$div(
              htmlTemplate("templates/reference.html",
                heading = pmRefs$heading[i],
                description = pmRefs$description[i],
                authors = pmRefs$authors[i],
                source = pmRefs$source[i],
                articleTitle = pmRefs$articleTitle[i],
                publication = pmRefs$publication[i],
                lang = pmRefs$lang[i],
              )
            )
          }),
          hr(),
          h2(i18n$t("Other CRFs"), id = "others"),
          lapply(1:nrow(otherRefs), function(i) {
            tags$div(
              htmlTemplate("templates/reference.html",
                heading = otherRefs$heading[i],
                description = otherRefs$description[i],
                authors = otherRefs$authors[i],
                source = otherRefs$source[i],
                articleTitle = otherRefs$articleTitle[i],
                publication = otherRefs$publication[i],
                lang = otherRefs$lang[i],
              )
            )
          }),
          hr(),
          h2(i18n$t("Valuation estimates"), id = "value"),
          lapply(1:nrow(valuationRefs), function(i) {
            tags$div(
              htmlTemplate("templates/reference.html",
                heading = valuationRefs$heading[i],
                description = valuationRefs$description[i],
                authors = valuationRefs$authors[i],
                source = valuationRefs$source[i],
                articleTitle = valuationRefs$articleTitle[i],
                publication = valuationRefs$publication[i],
                lang = valuationRefs$lang[i],
              )
            )
          }),
        ),
        # tabPanel ("Reset/Restart" ),
      )
    ),
    # tags$script(src = "scripts/wet-boew-4.min.js", type = "text/javascript"),
    tags$script(src = "scripts/wet-boew.min.js", type = "text/javascript"),
    tags$script(src = "scripts/wet-table.js", type = "text/javascript"),
    # Disconnect button for testing purposes only
    # actionButton("disconnect_button", "Disconnect")
  )
}

one_en <- data.frame(
  pm25_1 = 0, pm25_2 = 0, no2_1 = 0, no2_2 = 0, o3_1 = 0, o3_2 = 0, summero3_1 = 0, summero3_2 = 0, co24h_1 = 0, co24h_2 = 0, co1h_1 = 0, co1h_2 = 0, so2_1 = 0, so2_2 = 0,
  bz_1 = 0, bz_2 = 0, bt_1 = 0, bt_2 = 0, ac_1 = 0, ac_2 = 0, fm_1 = 0, fm_2 = 0
) # created to prevent from missing pollutants

one_fr <- data.frame(
  pm25_1 = 0, pm25_2 = 0, no2_1 = 0, no2_2 = 0, o3_1 = 0, o3_2 = 0, o3été_1 = 0, o3été_2 = 0, co24h_1 = 0, co24h_2 = 0, co1h_1 = 0, co1h_2 = 0, so2_1 = 0, so2_2 = 0,
  bz_1 = 0, bz_2 = 0, bt_1 = 0, bt_2 = 0, ac_1 = 0, ac_2 = 0, fm_1 = 0, fm_2 = 0
) # French version

# one is set in ui(request) from lang query param

vec1pm25 <- c(
  0.1120230722, 0.1683667772, 0.201510133, 0.2267714622, 0.2490801878, 0.2690574301, 0.352166474, 0.4163465221, 0.4718658925, 0.5254892055, 0.5796142538, 0.6348618817,
  0.691894507, 0.7525630977, 0.8171849, 0.8898155472, 0.9689240925, 1.057540536, 1.15707359, 1.27498034, 1.422468386, 1.613682589, 1.878871223, 2.324654231, 2.481371125,
  2.66935021, 2.939735753, 3.389512654, 4.620879968
) # created for generic pm2.5 distribution for acute exposure outcomes

vec1o3 <- c(
  0.2135244762, 0.331884567777778, 0.391444402888889, 0.432893127111111, 0.467033740666667, 0.495721992444444, 0.599479089333333, 0.670597302888889, 0.727438446666667, 0.776108593333333,
  0.819373663111111, 0.859439958222222, 0.898550111777778, 0.936879763333333, 0.976348239555556, 1.01611881111111, 1.05762638933333, 1.10206049288889, 1.15035982511111, 1.20358225133333,
  1.26289691666667, 1.33554251466667, 1.42811724555556, 1.57234806133333, 1.61499739844444, 1.668636638, 1.74122497466667, 1.853845932, 2.16893748622222
)
vec1summo3 <- c(
  0.376516319736842, 0.45105507, 0.49536504368421, 0.527736467368421, 0.553289268684211, 0.573614237894737, 0.649734429736842, 0.702327316315789, 0.746810478947368, 0.787305396842105,
  0.825251507894737, 0.862470737894737, 0.898761276578947, 0.935480942631579, 0.972350427631579, 1.01011500684211, 1.04965354105263, 1.09160978421053, 1.13637089868421, 1.185864905,
  1.24145879421053, 1.30700516526316, 1.39203717263158, 1.51840426, 1.55577614868421, 1.60072884657895, 1.65966918657895, 1.75372727105263, 1.93060025815789
)

vec1no2 <- c(
  0.169039672449799, 0.247124084899598, 0.285565956787149, 0.314330078795181, 0.336871566746988, 0.357510622811245, 0.436872073253012, 0.499899030843374, 0.556408673654618,
  0.608153579116466, 0.659616285220884, 0.711214317831325, 0.764306606184739, 0.819437905220884, 0.877003363855422, 0.938092563052209, 1.00599861767068, 1.08144307630522,
  1.16694247389558, 1.26574828273092, 1.38488501606426, 1.5337905188755, 1.73790552449799, 2.05895756626506, 2.15647441204819, 2.27865251004016, 2.44208785060241, 2.68998748353414, 3.35278422650602
)

vec1so2 <- c(
  0.0068921954, 0.0169951844, 0.0256519254, 0.0337350472, 0.0419926736, 0.0501586958, 0.0892441116, 0.1315316072, 0.1789785906, 0.2302609958, 0.286967922, 0.3500533572, 0.4178793088,
  0.4929184876, 0.5782213038, 0.6741119916, 0.7856981788, 0.9222729318, 1.0873211392, 1.2885969106, 1.5451189528, 1.8830357828, 2.408933142, 3.344307476, 3.67675938, 4.074731584, 4.675021286,
  5.705459832, 9.614169818
)
vec1co1h <- c(
  0.23550355, 0.30852819, 0.347670603333333, 0.37214733, 0.393854233333333, 0.409910833333333, 0.48342429, 0.531770036666667, 0.571559723333333, 0.610645363333333, 0.658685636666667,
  0.702629313333333, 0.74309495, 0.790087326666667, 0.839050303333333, 0.890868456666667, 0.947101593333333, 1.02500615333333, 1.10453976333333, 1.19070822333333, 1.31603799333333,
  1.48812207333333, 1.71639497333333, 2.14147464666667, 2.27217633333333, 2.43562323333333, 2.67746116333333, 3.10652595666667, 4.5309478
)
vec1co24h <- c(
  0.26615169, 0.346409475, 0.384747425, 0.41276701, 0.435574255, 0.455594255, 0.53602103, 0.59577212, 0.647760475, 0.69681239, 0.740163225, 0.782575075, 0.826796165, 0.869215755, 0.91167906,
  0.95884006, 1.006934315, 1.06202978, 1.12123045, 1.19298636, 1.280778195, 1.400211095, 1.565069705, 1.85282357, 1.94408044, 2.06349945, 2.225716155, 2.503231905, 3.30253671
)

wtnum <- c(0.005, 0.01, 0.01, 0.01, 0.01, 0.03, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.01, 0.01, 0.01, 0.01, 0.005) # weighted numbers


# server
server <- function(input, output, session) {
  # Pass URL query string to ui(request) so ?lang=fr works for French
  enableBookmarking(store = "url")

  # Session language: from hidden input (set when this page was built), else URL, else cache. No globals.
  get_session_lang <- function() {
    if (!is.null(input$session_lang) && input$session_lang %in% c("en", "fr")) {
      session$userData$lang <- input$session_lang
      return(input$session_lang)
    }
    url_search <- session$clientData$url_search
    if (!is.null(url_search) && nzchar(url_search)) {
      query <- parseQueryString(url_search)
      if (!is.null(query$lang) && query$lang %in% c("en", "fr")) {
        session$userData$lang <- query$lang
        return(query$lang)
      }
    }
    if (!is.null(session$userData$lang) && session$userData$lang %in% c("en", "fr")) {
      return(session$userData$lang)
    }
    session$userData$lang <- "en"
    "en"
  }

  # Session-scoped data (cross-tab fix): use _en/_fr so other tabs cannot overwrite
  get_session_xprov <- function() if (get_session_lang() == "en") xprov_en else xprov_fr
  get_session_geocode <- function() if (get_session_lang() == "en") geocode_en else geocode_fr
  get_session_three <- function() if (get_session_lang() == "en") three_en else three_fr
  get_session_one <- function() if (get_session_lang() == "en") one_en else one_fr
  get_session_cpi <- function() if (get_session_lang() == "en") cpi_en else cpi_fr

  # Translate for this session without changing global i18n (cross-tab fix)
  get_session_t <- function(key) {
    old <- i18n$get_translation_language()
    on.exit(i18n$set_translation_language(old))
    i18n$set_translation_language(get_session_lang())
    i18n$t(key)
  }

  # understand whether the user uploaded data, or we uploaded sample data

  uploadStatus <- reactiveVal("none")

  observeEvent(input$disconnect_button, {
    session$close()
  })

  # Define the download handler
  output$download_aqbat_appendix_en <- downloadHandler(
    filename = function() {
      "aqbat-appendix.pdf"
    },
    content = function(file) {
      file.copy("www/AQBAT-appendix-en-v3.pdf", file)
    }
  )

  output$download_aqbat_appendix_fr <- downloadHandler(
    filename = function() {
      "annexe-oebqa.pdf"
    },
    content = function(file) {
      file.copy("www/AQBAT-appendix-fr-v3.pdf", file)
    }
  )


  observeEvent(input$pollutants, {
    req(input$pollutants)

    # Initial setup for alert visibility
    shinyjs::runjs('document.getElementById("dataUploadedInfo").className = "alert";')
    shinyjs::removeClass(selector = "#dataUploadedInfo", class = "hidden")
    # disable file input
    shinyjs::disable("pollutants")
    session$sendCustomMessage("toggleFileInput", FALSE)

    # Check if the uploaded file is a valid CSV
    if (!(input$pollutants$size > 0 && tools::file_ext(input$pollutants$name) == "csv")) {
      shinyjs::addClass(selector = "#dataUploadedInfo", class = "alert-danger")

      # Session language for error messages (cross-tab fix)
      error_header_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("Error: Invalid file type.")))
      error_instruction_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("Please restart the application and upload a valid CSV file.")))

      # Set innerHTML using JavaScript with translated error messages
      shinyjs::runjs(sprintf('
      document.getElementById("dataUploadedHeader").innerHTML = "%s";
      document.getElementById("dataUploadedInstruction").innerHTML = "%s";
    ', error_header_message, error_instruction_message))

      shinyjs::addClass(selector = ".download-button", class = "hidden")
      return() # Prevent further reactivity
    }

    # Check for UTF-8 encoding
    is_utf8 <- TRUE
    tryCatch(
      {
        # Check first 100KB of the file
        con <- file(input$pollutants$datapath, "rb")
        bytes <- readBin(con, "raw", n = 100000)
        close(con)
        # Try to convert to UTF-8
        test_conv <- iconv(list(bytes), from = "UTF-8", to = "UTF-8")
        if (any(is.na(test_conv))) {
          is_utf8 <- FALSE
        }
      },
      error = function(e) {
        is_utf8 <- FALSE
      }
    )

    if (!is_utf8) {
      shinyjs::removeClass(selector = "#dataUploadedInfo", class = "alert-success")
      shinyjs::addClass(selector = "#dataUploadedInfo", class = "alert-danger")

      # Session language for error messages (cross-tab fix)
      error_header_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("Error: Invalid file encoding.")))
      error_instruction_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("Please ensure your CSV file is UTF-8 encoded. If you are using Excel, save as 'CSV UTF-8 (comma delimited)'.")))

      # Set innerHTML using JavaScript with translated error messages
      shinyjs::runjs(sprintf('
      document.getElementById("dataUploadedHeader").innerHTML = "%s";
      document.getElementById("dataUploadedInstruction").innerHTML = "%s";
    ', error_header_message, error_instruction_message))

      shinyjs::addClass(selector = ".download-button", class = "hidden")
      return() # Prevent further reactivity
    }

    # If all checks pass, show success message
    shinyjs::removeClass(selector = ".download-button", class = "hidden")
    shinyjs::addClass(selector = "#dataUploadedInfo", class = "alert-success")
    shinyjs::removeClass(selector = "#data-preview", class = "hidden")
    # Session language so message matches this tab (cross-tab fix)
    header_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("Your data has been uploaded")))
    instruction_message <- gsub("\\\\", "\\\\\\\\", gsub('"', '\\\\"', get_session_t("To upload new data, please click 'Clear all data and restart'.")))

    # Set innerHTML using JavaScript with translated messages
    shinyjs::runjs(sprintf('
      document.getElementById("dataUploadedHeader").innerHTML = "%s";
      document.getElementById("dataUploadedInstruction").innerHTML = "%s";
    ', header_message, instruction_message))

    uploadStatus("user-data")
  })

  session$allowReconnect(TRUE)

  # Remove hidden class after initialization to ensure a flicker-free experience
  shinyjs::removeClass(selector = "body", class = "hidden")
  shinyjs::addClass(selector = "span.btn", class = "btn-primary")

  observeEvent(input$internal_link_clicked, {
    # Programmatically update the tabset panel based on the clicked element's data-target attribute
    updateTabsetPanel(session, "alltabpanel", selected = input$internal_link_clicked)
  })

  session$sendCustomMessage("bindInternalLinks", list())

  canproresults1a_endpoint <- reactive({
    if (get_session_lang() == "en") canproresults1a()$endpoint else canproresults1a()$paramètre
  })

  canprovsummary_endpoint <- reactive({
    if (get_session_lang() == "en") canprovsummary()$endpoint else canprovsummary()$paramètre
  })

  Canprowexposure1_Acetaldehyde <- reactive({
    if (get_session_lang() == "en") Canprowexposure1()$Acetaldehyde else Canprowexposure1()$Acétaldéhyde
  })

  Canprowexposure1_Benzene <- reactive({
    if (get_session_lang() == "en") Canprowexposure1()$Benzene else Canprowexposure1()$Benzène
  })

  Canprowexposure1_Formaldehyde <- reactive({
    if (get_session_lang() == "en") Canprowexposure1()$Formaldehyde else Canprowexposure1()$Formaldéhyde
  })

  cpi_year <- reactive({
    if (get_session_lang() == "en") cpi_en$year else cpi_fr$année
  })

  four_age25plus <- reactive({
    if (get_session_lang() == "en") four()$age25plus else four()$ans25plus
  })

  four_age30plus <- reactive({
    if (get_session_lang() == "en") four()$age30plus else four()$ans30plus
  })

  four_Mort_cerebro <- reactive({
    if (get_session_lang() == "en") four()$Mort_cerebro else four()$Mort_cérébro
  })

  four_Mort_chronic <- reactive({
    if (get_session_lang() == "en") four()$Mort_chronic else four()$Mort_chronique
  })

  four_Mort_copd <- reactive({
    if (get_session_lang() == "en") four()$Mort_copd else four()$Mort_mpoc
  })

  four_Mort_ischemic <- reactive({
    if (get_session_lang() == "en") four()$Mort_ischemic else four()$Mort_ischémique
  })

  foura_age25plus <- reactive({
    if (get_session_lang() == "en") foura()$age25plus else foura()$ans25plus
  })

  foura_age5_19 <- reactive({
    if (get_session_lang() == "en") foura()$age5_19 else foura()$ans5_19
  })

  foura_allages <- reactive({
    if (get_session_lang() == "en") foura()$allages else foura()$touslesâges
  })

  foura_scenario <- reactive({
    if (get_session_lang() == "en") foura()$scenario else foura()$scénario
  })

  foura_summero3_1 <- reactive({
    if (get_session_lang() == "en") foura()$summero3_1 else foura()$o3été_1
  })

  foura_summero3_2 <- reactive({
    if (get_session_lang() == "en") foura()$summero3_2 else foura()$o3été_2
  })

  foura_year <- reactive({
    if (get_session_lang() == "en") foura()$year else foura()$année
  })

  fouracbc_Adult_Chronic_Bronchitis_Cases <- reactive({
    if (get_session_lang() == "en") fouracbc()$Adult_Chronic_Bronchitis_Cases else fouracbc()$Cas_bronchites_chroniques_adultes
  })

  fouracbc_age25plus <- reactive({
    if (get_session_lang() == "en") fouracbc()$age25plus else fouracbc()$ans25plus
  })

  fouraem_allages <- reactive({
    if (get_session_lang() == "en") fouraem()$allages else fouraem()$touslesâges
  })

  fouraem_Mort_acute <- reactive({
    if (get_session_lang() == "en") fouraem()$Mort_acute else fouraem()$Mort_aiguë
  })

  fourarsd_Acute_Resp_Symptom_Days <- reactive({
    if (get_session_lang() == "en") fourarsd()$Acute_Resp_Symptom_Days else fourarsd()$Jours_symptômes_resp_aigus
  })

  fourarsd_age20plus <- reactive({
    if (get_session_lang() == "en") fourarsd()$age20plus else fourarsd()$ans20plus
  })

  fourarsd_age5_19 <- reactive({
    if (get_session_lang() == "en") fourarsd()$age5_19 else fourarsd()$ans5_19
  })

  fourasd_age5_19 <- reactive({
    if (get_session_lang() == "en") fourasd()$age5_19 else fourasd()$ans5_19
  })

  fourasd_Asthma_Symptom_Days <- reactive({
    if (get_session_lang() == "en") fourasd()$Asthma_Symptom_Days else fourasd()$Jours_symptômes_asthme
  })

  fourb_allages <- reactive({
    if (get_session_lang() == "en") fourb()$allages else fourb()$touslesâges
  })

  fourb_scenario <- reactive({
    if (get_session_lang() == "en") fourb()$scenario else fourb()$scénario
  })

  fourb_year <- reactive({
    if (get_session_lang() == "en") fourb()$year else fourb()$année
  })

  fourcabe_age5_19 <- reactive({
    if (get_session_lang() == "en") fourcabe()$age5_19 else fourcabe()$ans5_19
  })

  fourcabe_Child_Acute_Bronchitis <- reactive({
    if (get_session_lang() == "en") fourcabe()$Child_Acute_Bronchitis else fourcabe()$Bronchite_aiguë_enfant
  })

  fourcerv_allages <- reactive({
    if (get_session_lang() == "en") fourcerv()$allages else fourcerv()$touslesâges
  })

  fourcerv_Cardiac_Emergency_Room <- reactive({
    if (get_session_lang() == "en") fourcerv()$Cardiac_Emergency_Room else fourcerv()$Visites_urgence_problèmes_cardiaques
  })

  fourcha_allages <- reactive({
    if (get_session_lang() == "en") fourcha()$allages else fourcha()$touslesâges
  })

  fourcha_Cardiac_Hospital <- reactive({
    if (get_session_lang() == "en") fourcha()$Cardiac_Hospital else fourcha()$Hôpital_problèmes_cardiaques
  })

  fourco24aem_allages <- reactive({
    if (get_session_lang() == "en") fourco24aem()$allages else fourco24aem()$touslesâges
  })

  fourco24aem_Mort_acute <- reactive({
    if (get_session_lang() == "en") fourco24aem()$Mort_acute else fourco24aem()$Mort_aiguë
  })

  fourecha_age65plus <- reactive({
    if (get_session_lang() == "en") fourecha()$age65plus else fourecha()$ans65plus
  })

  fourecha_Elderly_Cardiac_Hospital <- reactive({
    if (get_session_lang() == "en") fourecha()$Elderly_Cardiac_Hospital else fourecha()$Hôpital_problèmes_cardiaques_ainés
  })

  fourmrad_age20plus <- reactive({
    if (get_session_lang() == "en") fourmrad()$age20plus else fourmrad()$ans20plus
  })

  fourmrad_age5_19 <- reactive({
    if (get_session_lang() == "en") fourmrad()$age5_19 else fourmrad()$ans5_19
  })

  fourmrad_Minor_Restricted_Activity <- reactive({
    if (get_session_lang() == "en") fourmrad()$Minor_Restricted_Activity else fourmrad()$Activité_restreinte_mineure
  })

  fouro3aem_allages <- reactive({
    if (get_session_lang() == "en") fouro3aem()$allages else fouro3aem()$touslesâges
  })

  fouro3aem_Mort_acute <- reactive({
    if (get_session_lang() == "en") fouro3aem()$Mort_acute else fouro3aem()$Mort_aiguë
  })

  fouro3arsd_Acute_Resp_Symptom_Days <- reactive({
    if (get_session_lang() == "en") fouro3arsd()$Acute_Resp_Symptom_Days else fouro3arsd()$Jours_symptômes_resp_aigus
  })

  fouro3arsd_age20plus <- reactive({
    if (get_session_lang() == "en") fouro3arsd()$age20plus else fouro3arsd()$ans20plus
  })

  fouro3arsd_age5_19 <- reactive({
    if (get_session_lang() == "en") fouro3arsd()$age5_19 else fouro3arsd()$ans5_19
  })

  fouro3asd_age5_19 <- reactive({
    if (get_session_lang() == "en") fouro3asd()$age5_19 else fouro3asd()$ans5_19
  })

  fouro3asd_Asthma_Symptom_Days <- reactive({
    if (get_session_lang() == "en") fouro3asd()$Asthma_Symptom_Days else fouro3asd()$Jours_symptômes_asthme
  })

  fouro3cerm_age30plus <- reactive({
    if (get_session_lang() == "en") fouro3cerm()$age30plus else fouro3cerm()$ans30plus
  })

  fouro3cerm_allages <- reactive({
    if (get_session_lang() == "en") fouro3cerm()$allages else fouro3cerm()$touslesâges
  })

  fouro3cerm_Mort_respiratory <- reactive({
    if (get_session_lang() == "en") fouro3cerm()$Mort_respiratory else fouro3cerm()$Mort_respiratoire
  })

  fouro3cerm_summero3_1 <- reactive({
    if (get_session_lang() == "en") fouro3cerm()$summero3_1 else fouro3cerm()$o3été_1
  })

  fouro3cerm_summero3_2 <- reactive({
    if (get_session_lang() == "en") fouro3cerm()$summero3_2 else fouro3cerm()$o3été_2
  })

  fouro3rerv_allages <- reactive({
    if (get_session_lang() == "en") fouro3rerv()$allages else fouro3rerv()$touslesâges
  })

  fouro3rerv_Respiratory_Emergency_Room <- reactive({
    if (get_session_lang() == "en") fouro3rerv()$Respiratory_Emergency_Room else fouro3rerv()$Visites_urgence_problèmes_respiratoires
  })

  fouro3rha_allages <- reactive({
    if (get_session_lang() == "en") fouro3rha()$allages else fouro3rha()$touslesâges
  })

  fouro3rha_Respiratory_Hospital <- reactive({
    if (get_session_lang() == "en") fouro3rha()$Respiratory_Hospital else fouro3rha()$Hôpital_problèmes_respiratoires
  })

  fourrad_age20plus <- reactive({
    if (get_session_lang() == "en") fourrad()$age20plus else fourrad()$ans20plus
  })

  fourrad_Restricted_Activity_Days <- reactive({
    if (get_session_lang() == "en") fourrad()$Restricted_Activity_Days else fourrad()$Jours_activité_restreinte
  })

  fourrerv_allages <- reactive({
    if (get_session_lang() == "en") fourrerv()$allages else fourrerv()$touslesâges
  })

  fourrerv_Respiratory_Emergency_Room <- reactive({
    if (get_session_lang() == "en") fourrerv()$Respiratory_Emergency_Room else fourrerv()$Visites_urgence_problèmes_respiratoires
  })

  fourrha_allages <- reactive({
    if (get_session_lang() == "en") fourrha()$allages else fourrha()$touslesâges
  })

  fourrha_Respiratory_Hospital <- reactive({
    if (get_session_lang() == "en") fourrha()$Respiratory_Hospital else fourrha()$Hôpital_problèmes_respiratoires
  })

  fourso2aem_allages <- reactive({
    if (get_session_lang() == "en") fourso2aem()$allages else fourso2aem()$touslesâges
  })

  fourso2aem_Mort_acute <- reactive({
    if (get_session_lang() == "en") fourso2aem()$Mort_acute else fourso2aem()$Mort_aiguë
  })

  pnamemortfinal_Geocode <- reactive({
    if (get_session_lang() == "en") pnamemortfinal()$Geocode else pnamemortfinal()$Géocode
  })

  pnameschiff_Geocode <- reactive({
    if (get_session_lang() == "en") pnameschiff()$Geocode else pnameschiff()$Géocode
  })

  pnametoxic_Geocode <- reactive({
    if (get_session_lang() == "en") pnametoxic()$Geocode else pnametoxic()$Géocode
  })

  pwexposure1_allages <- reactive({
    if (get_session_lang() == "en") pwexposure1()$allages else pwexposure1()$touslesâges
  })

  twelveb_endpoint <- reactive({
    if (get_session_lang() == "en") twelveb()$endpoint else twelveb()$paramètre
  })

  wexposure1_allages <- reactive({
    if (get_session_lang() == "en") wexposure1()$allages else wexposure1()$touslesâges
  })

  # output$xmap <- renderUI({
  #   if (is.null(input$pollutants)) {
  #     div(
  #       get_session_t("The image below is an example of output that would be generated when a dataset is uploaded to the \"Pollutant data upload\" tab. The image shows the counts for chronic exposure mortality per 100,000 for the pollutant PM2.5 in 2016 with scenario set as \"1\". The census division for 'Toronto' in Ontario is selected as an example."),
  #       br(),
  #       strong(get_session_t("Note:")),
  #       get_session_t("These are static images."),
  #       strong(get_session_t("They cannot be interacted with.")),
  #       get_session_t("Interaction, customization, and more visualization options will be presented once data has been uploaded."),
  #     # div(img(src = "Map Options.jpg", height = 250, width = 1600)),
  #       div(img(src = "map_screenshot.jpg", class="dummy-map", alt=get_session_t("A demo map showing pollutant concentration levels across Canada.") ))
  #     )
  #   } else {
  #     useShinyjs()
  #     div(
  #       id = "formmap",
  #       get_session_t("Click \"Restore default values\" to restore the values of each cell back to its preset. In this case, it will clear all map inputs. Default values are restored in this tab only."),
  #       p(actionButton("resetmap", get_session_t("Restore default values"), style = "background-color: #ec7063 ; border: none; position:absoulute; bottom: 50px;", tabindex="0")),
  #       br(),
  #       tags$section(
  #         id = "map-instructions",
  #         class = "panel panel-info",
  #         tags$header(
  #           class = "panel-heading",
  #           tags$h3(
  #             class = "panel-title",
  #             get_session_t("Instructions for maps")
  #           )
  #         ),
  #         tags$div(
  #           class = "panel-body",
  #           tags$p(
  #             get_session_t("Select year, scenario, pollutant, endpoint and metric, then click \"Generate map\"."),
  #             br(),
  #             get_session_t("To map the baseline mortality rate, set the scenario and pollutant to \"None\"."),
  #             br(),
  #             get_session_t("To map status quo pollutant concentration, select a year, scenario and pollutant, and set endpoint to \"None\".")
  #             )
  #         )
  #       ),
  # paste("<b>",fruits[input$index],"</b>")
  # fluidRow(
  #   br(),
  #   # column(2,"")
  #   column(12, 'Select year, scenario, pollutant, endpoint and metric, then click GENERATE MAP. To map baseline mortality rate, set scenario and pollutant to "none". To map status quo pollutant concentration, select a year and scenario, and set endpoint to "none".'),
  # ),
  # div(
  #   class="flex-row",
  #   # setup user inputs for maps; make subsequent selections conditional on previous selections
  #   div(class="map-options", selectInput("mdy", get_session_t("Year"), c("", 2000:2063))),
  #   div(class="map-options", selectInput("scenario", get_session_t("Scenario"), c(get_session_t("None"), 1:50))),
  #   div(class="map-options",selectInput("mdp", get_session_t("Pollutant"), c("", get_session_t("None"), get_session_t("PM2.5"), "CO 24h", "NO2", "O3", get_session_t("O3 Summer"), "SO2", get_session_t("All (CO, NO2, O3, PM2.5, SO2)"), get_session_t("Benzene"), get_session_t("1,3-Butadiene"), get_session_t("Acetaldehyde"), get_session_t("Formaldehyde"), get_session_t("All Toxics (cancer)"), get_session_t("All Toxics (non-cancer)")))),
  #   div(class="map-options",
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'PM2.5'"
  #       } else {
  #         condition = "input.mdp == 'PM2,5'"
  #       },
  #       selectInput("mde1", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Chronic Exposure Mortality")))
  #     ),
  #
  #     conditionalPanel(
  #       condition = "input.mdp == 'NO2'",
  #       selectInput("mde2", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Acute Exposure Mortality")))
  #     ),
  #
  #     conditionalPanel(
  #       condition = "input.mdp == 'CO 24h'",
  #       selectInput("mde3", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Acute Exposure Mortality")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'All (CO, NO2, O3, PM2.5, SO2)'"
  #       } else {
  #         condition = "input.mdp == 'Tous (CO, NO2, O3, PM2,5, SO2)'"
  #       },
  #       selectInput("mde4", get_session_t("Endpoint"), c(get_session_t("Total Mortality"), get_session_t("Total Valuation")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'Benzene'"
  #       } else {
  #         condition = "input.mdp == 'Benzène'"
  #       },
  #       selectInput("mde5", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Cancer"), get_session_t("Hematological")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == '1,3-Butadiene'"
  #       } else {
  #         condition = "input.mdp == '1,3-butadiène'"
  #       },
  #       selectInput("mde6", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Cancer")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'Formaldehyde'"
  #       } else {
  #         condition = "input.mdp == 'Formaldéhyde'"
  #       },
  #       selectInput("mde7", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Respiratory (asthma)")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'Acetaldehyde'"
  #       } else {
  #         condition = "input.mdp == 'Acétaldéhyde'"
  #       },
  #       selectInput("mde8", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Respiratory (histological)")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'All Toxics (cancer)'"
  #       } else {
  #         condition = "input.mdp == 'Toutes toxiques (cancer)'"
  #       },
  #       selectInput("mde9", get_session_t("Endpoint"), c(get_session_t("Cancer")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mdp == 'All Toxics (non-cancer)'"
  #       } else {
  #         condition = "input.mdp == 'Toutes toxiques (non-cancérigène)'"
  #       },
  #       selectInput("mde10", get_session_t("Endpoint"), c(get_session_t("Non-cancer")))
  #     ),

  # conditionalPanel(
  #   condition = "input.mdp == 'SO2'",
  #   selectInput("mde11", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Acute Exposure Mortality")))
  # ),

  # conditionalPanel(
  #   condition = "input.mdp == 'O3'",
  #   selectInput("mde12", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Acute Exposure Mortality")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mdp == 'O3 Summer'"
  #   } else {
  #     condition = "input.mdp == 'O3 en été'"
  #   },
  #   selectInput("mde13", get_session_t("Endpoint"), c(get_session_t("None"), get_session_t("Chronic Exposure Respiratory Mortality")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mdp == 'Benzene'"
  #   } else {
  #     condition = "input.mdp == 'Benzène'"
  #   },
  #   selectInput("mde5", get_session_t("Endpoint"), c(get_session_t("None"), "Cancer", get_session_t("Hematological")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mdp == '1,3-Butadiene'"
  #   } else {
  #     condition = "input.mdp == '1,3-butadiène'"
  #   },
  #   selectInput("mde6", get_session_t("Endpoint"), c(get_session_t("None"), "Cancer")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde7 == 'None' & input.mdp=='Formaldehyde'"
  #   } else {
  #     condition = "input.mde7 == 'Aucun' & input.mdp=='Formaldéhyde'"
  #   },
  #   selectInput("mdc6", get_session_t("Metric"), c(get_session_t("Pollutant concentration")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde3 == 'None' & input.mdp=='CO 24h'"
  #   } else {
  #     condition = "input.mde3 == 'Aucun' & input.mdp=='CO 24h'"
  #   },
  #   selectInput("mdc7", get_session_t("Metric"), c(get_session_t("Pollutant concentration")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde11 == 'None' & input.mdp=='SO2'"
  #   } else {
  #     condition = "input.mde11 == 'Aucun' & input.mdp=='SO2'"
  #   },
  #   selectInput("mdc8", get_session_t("Metric"), c(get_session_t("Pollutant concentration")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde12 == 'None' & input.mdp=='O3'"
  #   } else {
  #     condition = "input.mde12 == 'Aucun' & input.mdp=='O3'"
  #   },
  #   selectInput("mdc9", get_session_t("Metric"), c(get_session_t("Pollutant concentration")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde13 == 'None' & input.mdp=='O3 Summer'"
  #   } else {
  #     condition = "input.mde13 == 'Aucun' & input.mdp=='O3 en été'"
  #   },
  #   selectInput("mdc10", get_session_t("Metric"), c(get_session_t("Pollutant concentration")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde1 == 'Chronic Exposure Mortality' & input.mdp=='PM2.5'"
  #   } else {
  #     condition = "input.mde1 == 'Mortalité liée à une exposition chronique' & input.mdp=='PM2,5'"
  #   },
  #   selectInput("mdm1", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Per capita valuation"), get_session_t("Change in life expectancy")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde2 == 'Acute Exposure Mortality' & input.mdp=='NO2'"
  #   } else {
  #     condition = "input.mde2 == 'Mortalité liée à une exposition aiguë' & input.mdp=='NO2'"
  #   },
  #   selectInput("mdm2", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Per capita valuation")))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mdp == 'All Toxics (cancer)'"
  #   } else {
  #     condition = "input.mdp == 'Toutes toxiques (cancer)'"
  #   },
  #   selectInput("mde9", get_session_t("Endpoint"), c("Cancer"))
  # ),

  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde5 == 'Hematological' & input.mdp=='Benzene'"
  #   } else {
  #     condition = "input.mde5 == 'Hématologique' & input.mdp=='Benzène'"
  #   },
  #   selectInput("mdm6", get_session_t("Metric"), c(get_session_t("Hazard Quotient")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde6 == 'Cancer' & input.mdp=='1,3-Butadiene'"
  #   } else {
  #     condition = "input.mde6 == 'Cancer' & input.mdp=='1,3-butadiène'"
  #   },
  #   selectInput("mdm7", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("DALYs/100k")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde8 == 'Respiratory (histological)' & input.mdp=='Acetaldehyde'"
  #   } else {
  #     condition = "input.mde8 == 'Respiratoire (histologique)' & input.mdp=='Acétaldéhyde'"
  #   },
  #   selectInput("mdm8", get_session_t("Metric"), c(get_session_t("Hazard Quotient")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde7 == 'Respiratory (asthma)' & input.mdp=='Formaldehyde'"
  #   } else {
  #     condition = "input.mde7 == 'Respiratoire (asthme)' & input.mdp=='Formaldéhyde'"
  #   },
  #   selectInput("mdm9", get_session_t("Metric"), c(get_session_t("Hazard Quotient")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde9 == 'Cancer' & input.mdp=='All Toxics (cancer)'"
  #   } else {
  #     condition = "input.mde9 == 'Cancer' & input.mdp=='Toutes toxiques (cancer)'"
  #   },
  #   selectInput("mdm10", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("DALYs/100k")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde10 == 'Non-cancer' & input.mdp=='All Toxics (non-cancer)'"
  #   } else {
  #     condition = "input.mde10 == 'Non-cancérigène' & input.mdp=='Toutes toxiques (non-cancérigène)'"
  #   },
  #   selectInput("mdm11", get_session_t("Metric"), c(get_session_t("Hazard Quotient")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde13 == 'Chronic Exposure Respiratory Mortality' & input.mdp=='O3 Summer'"
  #   } else {
  #     condition = "input.mde13 == 'Mortalité respiratoire liée à une exposition chronique' & input.mdp=='O3 en été'"
  #   },
  #   selectInput("mdm12", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Per capita valuation"), get_session_t("Change in life expectancy")))
  # ),
  #
  # conditionalPanel(
  #   if (get_session_lang() == "en") {
  #     condition = "input.mde3 == 'Acute Exposure Mortality' & input.mdp=='CO 24h'"
  #   } else {
  #     condition = "input.mde3 == 'Mortalité liée à une exposition aiguë' & input.mdp=='CO 24h'"
  #   },
  #   selectInput("mdm13", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Per capita valuation")))
  # ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mde11 == 'Acute Exposure Mortality' & input.mdp=='SO2'"
  #       } else {
  #         condition = "input.mde11 == 'Mortalité liée à une exposition aiguë' & input.mdp=='SO2'"
  #       },
  #       selectInput("mdm14", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Valuation/100k")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mde12 == 'Acute Exposure Mortality' & input.mdp=='O3'"
  #       } else {
  #         condition = "input.mde12 == 'Mortalité liée à une exposition aiguë' & input.mdp=='O3'"
  #       },
  #       selectInput("mdm15", get_session_t("Metric"), c(get_session_t("Counts/100k"), get_session_t("Per capita valuation")))
  #     ),
  #
  #     conditionalPanel(
  #       if (get_session_lang() == "en") {
  #         condition = "input.mde14 == 'Non-accidental mortality' & input.mdp=='None'"
  #       } else {
  #         condition = "input.mde14 == 'Mortalité non accidentel' & input.mdp=='Aucun'"
  #       },
  #       selectInput("mdm16", get_session_t("Metric"), c(get_session_t("Baseline rate/100k")))
  #     )
  #   )
  # ),
  #       p(actionButton("genmap", get_session_t("Generate map"))),
  #       div(tmapOutput("my_tmap", width = "100%", height = "auto")),
  #       # fluidRow(
  #       #   column(6,plotOutput("myplot"))),
  #
  #       h3("Image Downloads"),
  #       div(strong(get_session_t("Download static national map"))),
  #         # column(2,strong("Download histogram"))
  #       div(downloadButton("downm", get_session_t("Download map")),
  #       ),
  #       br(),
  #       div(radioButtons("mdg", get_session_t("Select province"), selected = character(0), inline = TRUE, c(get_session_t("NL"), get_session_t("PE"), get_session_t("NS"), get_session_t("NB"), get_session_t("QC"), get_session_t("ON"), get_session_t("MB"), get_session_t("SK"), get_session_t("AB"), get_session_t("BC"), get_session_t("YK"), get_session_t("NT"), get_session_t("NU")))),
  #       div(strong(get_session_t("Download static provincial map"))),
  #       div(downloadButton("downmp", get_session_t("Download map"))),
  #     )
  #   }
  # })

  # Reset buttons
  observeEvent(input$dataUploadRestartButton, {
    # runjs("window.history.pushState({}, '', '?tab=CRFs');")
    # session$reload()
  })

  observeEvent(input$resultsRestartButton, {
    # runjs("window.history.pushState({}, '', '?tab=CRFs');")
    # session$reload()
  })

  observeEvent(input$thresholdRestartButton, {
    # runjs("window.history.pushState({}, '', '?tab=CRFs');")
    # session$reload()
  })

  observeEvent(input$resetpm25, {
    reset("formpm25")
  }) # reset to default values in the page

  observeEvent(input$resetthreshold, {
    reset("formthreshold")
  }) # reset to default values in the page

  observe({
    params <- parseQueryString(session$clientData$url_search)
    if ("tab_index" %in% names(params)) {
      updateTabsetPanel(session, "alltabpanel", selected = params$tab_index)
    }
  })


  observeEvent(input$resetother, {
    reset("form2")
  })
  observeEvent(input$resetother1, {
    reset("form3")
  })

  observeEvent(input$resetall3, {
    reset("form4")
  })

  observeEvent(input$resetval, {
    reset("formval")
  })

  observeEvent(input$resetmap, {
    reset("formmap")
  })

  observeEvent(input$link_instruction1, {
    updateTabsetPanel(session, "alltabpanel", "Instructions")
  })
  observeEvent(input$link_instruction2, {
    updateTabsetPanel(session, "alltabpanel", "Instructions")
  })
  observeEvent(input$link_instruction3, {
    updateTabsetPanel(session, "alltabpanel", "Instructions")
  })
  observeEvent(input$link_instruction5, {
    updateTabsetPanel(session, "alltabpanel", "Instructions")
  })

  # prepare all input parameters for user summary to download

  scenarioyr <- reactive({
    cbind(get_session_t("Scenario year"), unique(foura_year()))
  })

  currency <- reactive({
    cbind(get_session_t("Currency"), paste(get_session_t("currency year ="), input$curr, get_session_t(", base year for discounting ="), input$baseyr, get_session_t(", discounting rate ="), input$discountrate, "%"))
  })
  simulationitn <- reactive({
    cbind(get_session_t("Simulation iterations"), paste(get_session_t("iterations ="), input$itn))
  })
  childasprev <- reactive({
    cbind(get_session_t("Prevalence of asthma (age < 20 years)"), paste(get_session_t("asthma prevalence (%) ="), input$asprev))
  })
  pmcrf1 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic exposure mortality:"), get_session_t("reg type ="), input$rtype, get_session_t(", RR/OR ="), input$crf, "[", input$l95, ",", input$u95, "],", get_session_t("Increment (ug/m3) = "), input$incr))
  })
  pmcrf2 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Acute respiratory symptom days:"), get_session_t("reg type ="), input$pm25_rtype2, get_session_t(", RR/OR ="), input$pm25_crf2, "[", input$pm25_l95_2, ",", input$pm25_u95_2, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr2))
  })
  pmcrf3 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Adult chronic bronchitis cases:"), get_session_t("reg type ="), input$pm25_rtype3, get_session_t(", RR/OR ="), input$pm25_crf3, "[", input$pm25_l95_3, ",", input$pm25_u95_3, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr3))
  })
  pmcrf4 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Asthma symptom days:"), get_session_t("reg type ="), input$pm25_rtype4, get_session_t(", RR/OR ="), input$pm25_crf4, "[", input$pm25_l95_4, ",", input$pm25_u95_4, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr4))
  })
  pmcrf5 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Cardiac emergency room visits:"), get_session_t("reg type ="), input$pm25_rtype5, get_session_t(", RR/OR ="), input$pm25_crf5, "[", input$pm25_l95_5, ",", input$pm25_u95_5, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr5))
  })
  pmcrf6 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Cardiac hospital admissions:"), get_session_t("reg type ="), input$pm25_rtype6, get_session_t(", RR/OR ="), input$pm25_crf6, "[", input$pm25_l95_6, ",", input$pm25_u95_6, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr6))
  })
  pmcrf7 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Child acute bronchitis episodes:"), get_session_t("reg type ="), input$pm25_rtype7, get_session_t(", RR/OR ="), input$pm25_crf7, "[", input$pm25_l95_7, ",", input$pm25_u95_7, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr7))
  })
  pmcrf8 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Respiratory emergency room visits:"), get_session_t("reg type ="), input$pm25_rtype8, get_session_t(", RR/OR ="), input$pm25_crf8, "[", input$pm25_l95_8, ",", input$pm25_u95_8, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr8))
  })
  pmcrf9 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Respiratory Hospital Admissions:"), get_session_t("reg type ="), input$pm25_rtype9, get_session_t(", RR/OR ="), input$pm25_crf9, "[", input$pm25_l95_9, ",", input$pm25_u95_9, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr9))
  })
  pmcrf10 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Restricted Activity Days:"), get_session_t("reg type ="), input$pm25_rtype10, get_session_t(", RR/OR ="), input$pm25_crf10, "[", input$pm25_l95_10, ",", input$pm25_u95_10, "],", get_session_t("Increment (ug/m3) = "), input$pm25_incr10))
  })

  pmcrf11 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic Exposure Cerebrovascular Mortality:"), get_session_t("reg type ="), input$rtypecerebro, get_session_t(", shape ="), input$crfcerebro, get_session_t(", scale ="), input$scalecerebro, get_session_t(", Increment (ug/m3) = "), input$incrcerebro))
  })
  pmcrf12 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic Exposure COPD Mortality:"), get_session_t("reg type ="), input$rtypcopd, get_session_t(", shape ="), input$crfcopd, get_session_t(", scale ="), input$scalecopd, get_session_t(", Increment (ug/m3) = "), input$incrcopd))
  })
  pmcrf13 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic Exposure Ischemic Heart Disease Mortality:"), get_session_t("reg type ="), input$rtypIschem, get_session_t(", shape ="), input$crfIschem, get_session_t(", scale ="), input$scaleIschem, get_session_t(", Increment (ug/m3) = "), input$incrIschem))
  })

  pmcrf14 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic Exposure Lung Cancer Mortality:"), get_session_t("reg type ="), input$rtypelung, get_session_t(", RR/OR ="), input$crflung, "[", input$l95lung, ",", input$u95lung, "],", get_session_t("Increment (ug/m3) = "), input$incrlung))
  })

  pmcrf15 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("PM2.5 - Chronic exposure mortality:"), get_session_t("reg type = Non-linear SCHIF, thr conc = 2.4 ug/m3, Normal (mean = 0.0813, se = 0.01773, theta = 3.639, mu = 0.763, pi = 1.896)")))
  })

  o3crf1 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("O3 - Acute Exposure Mortality:"), get_session_t("reg type ="), input$o3_rtype1, get_session_t(", RR/OR ="), input$o3_crf1, "[", input$o3_l95_1, ",", input$o3_u95_1, "],", get_session_t("Increment (ppb) = "), input$o3_incr1))
  })
  o3crf2 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Chronic Exposure Respiratory Mortality:"), get_session_t("reg type ="), input$o3_rtype2, get_session_t(", RR/OR ="), input$o3_crf2, "[", input$o3_l95_2, ",", input$o3_u95_2, "],", get_session_t("Increment (ppb) = "), input$o3_incr2))
  })
  o3crf3 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Acute respiratory symptom days:"), get_session_t("reg type ="), input$o3_rtype3, get_session_t(", RR/OR ="), input$o3_crf3, "[", input$o3_l95_3, ",", input$o3_u95_3, "],", get_session_t("Increment (ppb) = "), input$o3_incr3))
  })
  o3crf4 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Asthma symptom days:"), get_session_t("reg type ="), input$o3_rtype4, get_session_t(", RR/OR ="), input$o3_crf4, "[", input$o3_l95_4, ",", input$o3_u95_4, "],", get_session_t("Increment (ppb) = "), input$o3_incr4))
  })
  o3crf5 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Minor Restricted Activity Days:"), get_session_t("reg type ="), input$o3_rtype5, get_session_t(", RR/OR ="), input$o3_crf5, "[", input$o3_l95_5, ",", input$o3_u95_5, "],", get_session_t("Increment (ppb) = "), input$o3_incr5))
  })
  o3crf6 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Respiratory emergency room visits:"), get_session_t("reg type ="), input$o3_rtype6, get_session_t(", RR/OR ="), input$o3_crf6, "[", input$o3_l95_6, ",", input$o3_u95_6, "],", get_session_t("Increment (ppb) = "), input$o3_incr6))
  })
  o3crf7 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("Summer O3 - Respiratory Hospital Admissions:"), get_session_t("reg type ="), input$o3_rtype7, get_session_t(", RR/OR ="), input$o3_crf7, "[", input$o3_l95_7, ",", input$o3_u95_7, "],", get_session_t("Increment (ppb) = "), input$o3_incr7))
  })

  no2crf <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("NO2 - Acute Exposure Mortality:"), get_session_t("reg type ="), input$no2_rtype, get_session_t(", RR/OR ="), input$no2_crf, "[", input$no2_l95, ",", input$no2_u95, "],", get_session_t("Increment (ppb) = "), input$no2_incr))
  })
  so2crf <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("SO2 - Acute Exposure Mortality:"), get_session_t("reg type ="), input$so2_rtype, get_session_t(", RR/OR ="), input$so2_crf, "[", input$so2_l95, ",", input$so2_u95, "],", get_session_t("Increment (ppb) = "), input$so2_incr))
  })
  cocrf1 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("CO (24h) - Acute Exposure Mortality:"), get_session_t("reg type ="), input$co24_rtype, get_session_t(", RR/OR ="), input$co24_crf, "[", input$co24_l95, ",", input$co24_u95, "],", get_session_t("Increment (ppm) = "), input$co24_incr))
  })
  cocrf2 <- reactive({
    cbind(get_session_t("Concentration response function"), paste(get_session_t("CO (1h) - Elderly cardiac hospital admissions:"), get_session_t("reg type ="), input$co1_rtype, get_session_t(", RR/OR ="), input$co1_crf, "[", input$co1_l95, ",", input$co1_u95, "],", get_session_t("Increment (ppm) = "), input$co1_incr))
  })


  endpointval1 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Mortality: source year ="), input$vslyr, get_session_t(", distribution form ="), input$vslfm, get_session_t("(low ="), input$lvsl, get_session_t("million, central ="), input$vsl,
      get_session_t("million, high ="), input$uvsl, get_session_t("million, low value probability ="), input$plvsl, get_session_t(", central value probability ="), input$pcvsl, ")"
    ))
  })

  endpointval2 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(get_session_t("Acute respiratory symptom days: source year ="), input$vsl2yr, get_session_t(", distribution form ="), input$vsl2fm, get_session_t("( mean ="), input$vsl2, get_session_t(", se ="), input$lvsl2, ")"))
  })

  endpointval3 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Adult Chronic Bronchitis Cases: source year ="), input$vsl3yr, get_session_t(", distribution form ="), input$vsl3fm, get_session_t("(low ="), input$lvsl3 * 1000, get_session_t(", central ="), input$vsl3 * 1000,
      get_session_t(", high ="), input$uvsl3 * 1000, get_session_t(", low value probability ="), input$pcvsl3, get_session_t(", central value probability ="), input$plvsl3, ")"
    ))
  })


  endpointval4 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Asthma symptom days: source year ="), input$sourcevsl4b, get_session_t(", distribution form ="), input$sourcevsl4c, "(min =", input$lvsl4, get_session_t(", central ="), input$vsl4,
      ", max=", input$uvsl4, ")"
    ))
  })

  endpointval5 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(get_session_t("Cardiac emergency room visits: source year ="), input$sourcevsl5b, get_session_t(", distribution form ="), input$sourcevsl5c, get_session_t("( mean ="), input$vsl5, get_session_t(", se ="), input$lvsl5, ")"))
  })


  endpointval6 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Child acute bronchitis episodes: source year ="), input$sourcevsl6b, get_session_t(", distribution form ="), input$sourcevsl6c, get_session_t("(low ="), input$lvsl6, get_session_t(", central ="), input$vsl6,
      get_session_t(", high ="), input$uvsl6, get_session_t(", low value probability ="), input$plvsl6, get_session_t(", central value probability ="), input$pcvsl6, ")"
    ))
  })

  endpointval7 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(get_session_t("Elderly cardiac hospital admissions: source year ="), input$sourcevsl7b, get_session_t(", distribution form ="), input$sourcevsl7c, get_session_t("( mean ="), input$vsl7, get_session_t(", se ="), input$lvsl7, ")"))
  })

  endpointval8 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Minor Restricted Activity Days: source year ="), input$sourcevsl8b, get_session_t(", distribution form ="), input$sourcevsl8c,
      get_session_t("( mean ="), input$vsl8, get_session_t(", se ="), input$lvsl8, ")"
    ))
  })


  endpointval9 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Respiratory emergency room visits: source year ="), input$sourcevsl9b, get_session_t(", distribution form ="), input$sourcevsl9c,
      get_session_t("( mean ="), input$vsl9, get_session_t(", se ="), input$lvsl9, ")"
    ))
  })

  endpointval10 <- reactive({
    cbind(get_session_t("endpoint valuations($)"), paste(
      get_session_t("Restricted Activity Days: source year ="), input$sourcevsl10b, get_session_t(", distribution form ="), input$sourcevsl10c,
      get_session_t("( mean ="), input$vsl10, get_session_t(", se ="), input$lvsl10, ")"
    ))
  })

  pmthreshold <- reactive({
    cbind(get_session_t("PM2.5 threshold concentration (ug/m3)"), paste(get_session_t("threshold ="), input$pmthr))
  })
  o3threshold <- reactive({
    cbind(get_session_t("O3 threshold concentration (ppb)"), paste(get_session_t("threshold ="), input$o3thr))
  })
  summero3threshold <- reactive({
    cbind(get_session_t("Summer O3 threshold concentration (ppb)"), paste(get_session_t("threshold ="), input$summero3thr))
  })
  no2threshold <- reactive({
    cbind(get_session_t("NO2 threshold concentration (ppb)"), paste(get_session_t("threshold ="), input$no2thr))
  })
  so2threshold <- reactive({
    cbind(get_session_t("SO2 threshold concentration (ppb)"), paste(get_session_t("threshold ="), input$so2thr))
  })
  cothreshold <- reactive({
    cbind(get_session_t("CO threshold concentration (ppm)"), paste(get_session_t("threshold ="), input$cothr))
  })


  inputallparameters1 <- reactive({
    as.data.frame(rbind(
      scenarioyr(), currency(), simulationitn(), childasprev(), pmcrf1(), pmcrf2(), pmcrf3(), pmcrf4(), pmcrf5(), pmcrf6(), pmcrf7(), pmcrf8(), pmcrf9(), pmcrf10(), pmcrf11(), pmcrf12(), pmcrf13(), pmcrf14(), pmcrf15(),
      o3crf1(), o3crf2(), o3crf3(), o3crf4(), o3crf5(), o3crf6(), o3crf7(), no2crf(), so2crf(), cocrf1(), cocrf2(), pmthreshold(), o3threshold(), summero3threshold(), no2threshold(), so2threshold(), cothreshold(),
      endpointval1(), endpointval2(), endpointval3(), endpointval4(), endpointval5(), endpointval6(), endpointval7(), endpointval8(), endpointval9(), endpointval10()
    ))
  })


  inputallparameters <- reactive({
    i <- inputallparameters1()
    colnames(i) <- c(get_session_t("Item"), "Description")
    i
  })

  # INPUT PARAMETERS TABLE
  output$allinputa <- renderTable({
    tryCatch(
      {
        head(inputallparameters(), 3)
      },
      error = function(e) handle_error(e, "allinputa")
    )
  })

  output$allinputb <- downloadHandler(
    filename = function() {
      get_session_t("input_parameters.xlsx")
    },
    content = function(file) {
      write_xlsx(inputallparameters(), path = file)
    }
  )

  pollutantdata0 <- reactive({
    req(input$pollutants) # Ensure files are uploaded

    # Validate file extensions
    valid_files <- input$pollutants$datapath[
      grepl("\\.csv$", input$pollutants$name, ignore.case = TRUE)
    ]

    if (length(valid_files) == 0) {
      shinyjs::addClass(selector = ".download-button", class = "hidden")
      # shinyjs::runjs('$(".download-button").addClass("hidden")',)
      shinyjs::addClass(selector = "#dataPreviewTable", class = "hidden")
      stop("No valid CSV files uploaded.")
    }

    # Read and combine the files
    tryCatch(
      {
        rbindlist(
          lapply(valid_files, fread),
          use.names = TRUE, fill = TRUE
        )
      },
      error = function(e) {
        # shinyjs::runjs('$(".download-button").addClass("hidden")',)
        shinyjs::addClass(selector = ".download-button", class = "hidden")
        shinyjs::addClass(selector = "#dataPreviewTable", class = "hidden")
        stop("Error reading files. Please ensure all files are valid CSVs.")
      }
    )
  })
  # fill=TRUE: if files have unequal length and filling with blank

  # shinyjs::removeClass(selector = "#data-preview", class = "hidden")

  xpollutantdata0 <- reactive({
    if (is.null(input$pollutants)) {
      # Simulate actions
      uploadStatus("sample-data")
      shinyjs::disable("pollutants")
      shinyjs::removeClass(selector = "#dataUploadedInfo", class = "hidden")
      shinyjs::removeClass(selector = "#dataUploadedInfo", class = "alert-success")
      shinyjs::addClass(selector = "#dataUploadedInfo", class = "alert-info")
      session$sendCustomMessage("toggleFileInput", FALSE)
      shinyjs::removeClass(selector = "#sampleDataUploadedInfo", class = "hidden")
      # Use language from hidden input (set when this session's UI was built)
      return(if (get_session_lang() == "fr") xsample1_fr else xsample1_en)
    } else {
      return(pollutantdata0()) # Avoid circular dependency
    }
  })


  pollutantdata1 <- reactive({
    one_session <- if (get_session_lang() == "fr") one_fr else one_en
    left_join(xpollutantdata0(), one_session)
  })


  # pollutantdata1 <-reactive({ merge(pollutantdata0(), one, all.x=TRUE  )})
  pollutantdata <- reactive({
    na_replace(pollutantdata1(), 0)
  })

  # preview user input file with user specified number of rows to preview
  output$dataPreviewTable <- renderTable({
    req(pollutantdata0())
    head(pollutantdata0(), n = 3)
  })


  # merge file with population and mortality by CD with user input pollutant concentration data by CD
  foura0 <- reactive({
    right_join(get_session_three(), pollutantdata(), by = c(get_session_t("year"), get_session_t("CDUID")))
  })
  xfoura <- reactive({
    left_join(foura0(), get_session_one())
  })
  foura <- reactive({
    na_replace(xfoura(), 0)
  })

  baserates <- reactive({
    foura()[c(4, 1, 2, 5:23)]
  })

  xbaserates <- reactive({
    foura()[c(4, 1, 2, 5:10)]
  })

  xxbaserates <- reactive({
    xk5 <- xbaserates()
    colnames(xk5) <- c(
      get_session_t("Year"), get_session_t("CDUID"), "Province", get_session_t("Acute Exposure Mortality"), get_session_t("Chronic Exposure Mortality"), get_session_t("Respiratory Mortality"),
      get_session_t("Cardiovascular Mortality"), get_session_t("Cerebrovascular Mortality"), get_session_t("COPD Mortality")
    )
    xk5
  })


  popdata <- reactive({
    foura()[c(4, 1, 2, 24, 25, 27, 28, 29, 33, 34)]
  })
  basedata <- reactive({
    foura()[c(4, 1, 2, 5:25, 27, 28, 29, 33, 34)]
  })


  fourb <- reactive({
    foura()
  }) # fourb:  used for population-weighted exposure and toxics calculation

  wexposure1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        allages * chg(pm25_2, pm25_1, input$pmthr), allages * chg(summero3_2, summero3_1, input$summero3thr), allages * chg(o3_2, o3_1, input$o3thr), allages * chg(no2_2, no2_1, input$no2thr),
        allages * chg(so2_2, so2_1, input$so2thr), allages * chg(co1h_2, co1h_1, input$cothr), allages * chg(co24h_2, co24h_1, input$cothr),
        allages * (bz_2 - bz_1), allages * (bt_2 - bt_1), allages * (ac_2 - ac_1), allages * (fm_2 - fm_1), allages
      ) ~ year + scenario, fourb(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        touslesâges * chg(pm25_2, pm25_1, input$pmthr), touslesâges * chg(o3été_2, o3été_1, input$summero3thr), touslesâges * chg(o3_2, o3_1, input$o3thr), touslesâges * chg(no2_2, no2_1, input$no2thr),
        touslesâges * chg(so2_2, so2_1, input$so2thr), touslesâges * chg(co1h_2, co1h_1, input$cothr), touslesâges * chg(co24h_2, co24h_1, input$cothr),
        touslesâges * (bz_2 - bz_1), touslesâges * (bt_2 - bt_1), touslesâges * (ac_2 - ac_1), touslesâges * (fm_2 - fm_1), touslesâges
      ) ~ année + scénario, fourb(), sum, na.action = NULL)
    }
  })

  Canwexposure2 <- reactive({
    cbind(
      wexposure1()[c(1, 2)], "Canada", chg7(wexposure1()$V1, wexposure1_allages()), chg7(wexposure1()$V2, wexposure1_allages()), chg7(wexposure1()$V3, wexposure1_allages()),
      chg7(wexposure1()$V4, wexposure1_allages()), chg7(wexposure1()$V5, wexposure1_allages()), chg7(wexposure1()$V6, wexposure1_allages()), chg7(wexposure1()$V7, wexposure1_allages()),
      chg7(wexposure1()$V8, wexposure1_allages()), chg7(wexposure1()$V9, wexposure1_allages()), chg7(wexposure1()$V10, wexposure1_allages()), chg7(wexposure1()$V11, wexposure1_allages())
    )
  })


  Canwexposure3 <- reactive({
    es1 <- Canwexposure2()
    colnames(es1) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", "pm25", "summer_O3", "O3", "NO2", "SO2", "CO_1h",
      "CO_24h", get_session_t("Benzene"), "butadiene.1.3", get_session_t("Acetaldehyde"), get_session_t("Formaldehyde")
    )
    es1
  })

  pwexposure1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        allages * chg(pm25_2, pm25_1, input$pmthr), allages * chg(summero3_2, summero3_1, input$summero3thr), allages * chg(o3_2, o3_1, input$o3thr), allages * chg(no2_2, no2_1, input$no2thr), allages * chg(so2_2, so2_1, input$so2thr),
        allages * chg(co1h_2, co1h_1, input$cothr), allages * chg(co24h_2, co24h_1, input$cothr),
        allages * (bz_2 - bz_1), allages * (bt_2 - bt_1), allages * (ac_2 - ac_1), allages * (fm_2 - fm_1), allages
      ) ~ year + scenario + province, fourb(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        touslesâges * chg(pm25_2, pm25_1, input$pmthr), touslesâges * chg(o3été_2, o3été_1, input$summero3thr), touslesâges * chg(o3_2, o3_1, input$o3thr), touslesâges * chg(no2_2, no2_1, input$no2thr), touslesâges * chg(so2_2, so2_1, input$so2thr),
        touslesâges * chg(co1h_2, co1h_1, input$cothr), touslesâges * chg(co24h_2, co24h_1, input$cothr),
        touslesâges * (bz_2 - bz_1), touslesâges * (bt_2 - bt_1), touslesâges * (ac_2 - ac_1), touslesâges * (fm_2 - fm_1), touslesâges
      ) ~ année + scénario + province, fourb(), sum, na.action = NULL)
    }
  })

  pwexposure2 <- reactive({
    cbind(
      pwexposure1()[c(1, 2, 3)], chg7(pwexposure1()$V1, pwexposure1_allages()), chg7(pwexposure1()$V2, pwexposure1_allages()), chg7(pwexposure1()$V3, pwexposure1_allages()),
      chg7(pwexposure1()$V4, pwexposure1_allages()), chg7(pwexposure1()$V5, pwexposure1_allages()), chg7(pwexposure1()$V6, pwexposure1_allages()), chg7(pwexposure1()$V7, pwexposure1_allages()), chg7(pwexposure1()$V8, pwexposure1_allages()), chg7(pwexposure1()$V9, pwexposure1_allages()),
      chg7(pwexposure1()$V10, pwexposure1_allages()), chg7(pwexposure1()$V11, pwexposure1_allages())
    )
  })

  pwexposure3 <- reactive({
    es2 <- pwexposure2()
    colnames(es2) <- c(get_session_t("year"), get_session_t("scenario"), "region", "pm25", "summer_O3", "O3", "NO2", "SO2", "CO_1h", "CO_24h", get_session_t("Benzene"), "butadiene.1.3", get_session_t("Acetaldehyde"), get_session_t("Formaldehyde"))
    es2
  })


  xCanprowexposure1 <- reactive({
    rbind(Canwexposure3(), pwexposure3())
  })

  Canprowexposure1 <- reactive({
    if (get_session_lang() == "en") {
      xCanprowexposure1()[with(xCanprowexposure1(), order(year, scenario)), ]
    } else {
      xCanprowexposure1()[with(xCanprowexposure1(), order(année, scénario)), ]
    }
  })

  xxCanprowexposure1 <- reactive({
    xes1 <- Canprowexposure1()
    colnames(xes1) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Region"), get_session_t("PM2.5"), get_session_t("Summer O3"), "O3", "NO2",
      "SO2", "CO 1h", "CO 24h", get_session_t("Benzene"), get_session_t("1,3-Butadiene"), get_session_t("Acetaldehyde"), get_session_t("Formaldehyde")
    )
    xes1
  })

  xxxCanprowexposure1 <- reactive({
    if (get_session_lang() == "en") {
      cbind(xxCanprowexposure1()[c(1, 2, 3)], as.data.frame(sapply((xxCanprowexposure1()[c(4:14)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xxCanprowexposure1()[c(1, 2, 3)], as.data.frame(sapply((xxCanprowexposure1()[c(4:14)]), format_numbers2, simplify = FALSE)))
    }
  })

  # POPULATION-WEIGHTED EXPOSURE TABLE
  output$outputexposure <- renderTable({
    tryCatch(
      {
        head(xxpnameexposure(), 3)
      },
      error = function(e) handle_error(e, "outputexposure")
    )
  })

  pnameexposure <- reactive({
    merge_data(xxxCanprowexposure1(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  xpnameexposure <- reactive({
    pnameexposure()[c(1, 2, 4:6), c(2, 3, 16, 4:8, 11, 13, 14)]
  })

  xxpnameexposure <- reactive({
    xxphh2 <- xpnameexposure()
    colnames(xxphh2) <- c(
      get_session_t("Year"), get_session_t("Scenario"), "Province", get_session_t("PM2.5"), get_session_t("Summer O3"), "O3", "NO2", "SO2",
      get_session_t("Benzene"), get_session_t("Acetaldehyde"), get_session_t("Formaldehyde")
    )
    xxphh2
  })

  x1Canprowexposure1 <- reactive({
    merge_data(Canprowexposure1(), get_session_xprov(), by.x = "region", by.y = "Province")
  })
  x2Canprowexposure1 <- reactive({
    x1Canprowexposure1()[, c(2, 3, 16, 4:14)]
  })

  x3Canprowexposure1 <- reactive({
    x3expo <- x2Canprowexposure1()
    colnames(x3expo) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Region"), get_session_t("PM2.5"), get_session_t("Summer O3"), "O3", "NO2", "SO2", "CO 1h",
      "CO 24h", get_session_t("Benzene"), get_session_t("1,3-Butadiene"), get_session_t("Acetaldehyde"), get_session_t("Formaldehyde")
    )
    x3expo
  })

  x4Canprowexposure1 <- reactive({
    if (get_session_lang() == "en") {
      x3Canprowexposure1()[with(x3Canprowexposure1(), order(Year, Scenario, Region)), ]
    } else {
      x3Canprowexposure1()[with(x3Canprowexposure1(), order(Année, Scénario, Région)), ]
    }
  })


  output$d5 <- downloadHandler(
    filename = function() {
      get_session_t("Weighted_exposure.xlsx")
    },
    content = function(file) {
      write_xlsx(x4Canprowexposure1(), path = file)
    }
  )
  # Monte Carlo simulations with number of iterations specified by user to calculate mean, 95%CI of attributable outcomes and associated valuation

  # PM2.5 chronic exposure mortality
  set.seed(100)
  bm <- reactive({
    rnorm(input$itn, beta(input$rtype, input$crf, input$incr, get_session_t("log-linear")), se(input$rtype, input$u95, input$l95, input$incr, get_session_t("log-linear")))
  }) # n=10,000

  set.seed(100)
  vm <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiy <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  }) # define currency year-CPI

  cpiy1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  }) # source year-CPI

  cpiadj <- reactive({
    (cpiy()$cpi / cpiy1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  }) # formula User Guide p187
  rtype <- reactive({
    input$rtype
  })
  four <- reactive({
    cbind(foura(), rtype(), cpiadj())
  })

  five <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * af(four()$rtype, beta(four()$rtype, input$crf, input$incr, get_session_t("log-linear")), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  six <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * af(four()$rtype, quantile(bm(), 0.025), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  seven <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * af(four()$rtype, quantile(bm(), 0.975), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })


  eight <- reactive({
    five() * mean(vm()) * four()$cpiadj * 1000000
  }) # $unit = million
  # vml <- reactive({
  #   (quantile((af(four()$rtype, bm(), 1) * vm()), 0.025)) / af(four()$rtype, quantile(bm(), 0.025), 1)
  # }) # Number=n*293
  # xvml <- reactive({
  #   vml() * four()$cpiadj * 1000000
  # })
  vml <- reactive({
    quantile(vm(), 0.025)
  })
  vmu <- reactive({
    quantile(vm(), 0.975)
  })

  # vmu <- reactive({
  #   (quantile((af(four()$rtype, bm(), 1) * vm()), 0.975)) / af(four()$rtype, quantile(bm(), 0.975), 1)
  # })
  # xvmu <- reactive({
  #   vmu() * four()$cpiadj * 1000000
  # })

  nine <- reactive({
    six() * vml() * four()$cpiadj * 1000000
  })
  ten <- reactive({
    seven() * vmu() * four()$cpiadj * 1000000
  })
  tena <- reactive({
    100000 * five() / foura_allages()
  })
  pctxsmort <- reactive({
    af(four()$rtype, beta(four()$rtype, input$crf, input$incr, get_session_t("log-linear")), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  }) # pctXS is a function of CRF*concentration change
  pctxsmortl95 <- reactive({
    af(four()$rtype, quantile(bm(), 0.025), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  pctxsmortu95 <- reactive({
    af(four()$rtype, quantile(bm(), 0.975), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })

  leyr <- reactive({
    lifeyr(pctxsmort())
  }) # life expectancy change is function of pctXS
  leyrl95 <- reactive({
    lifeyr(pctxsmortl95())
  })
  leyru95 <- reactive({
    lifeyr(pctxsmortu95())
  })


  # PM2.5 SCHIF- chronic exposure mortality
  set.seed(100)
  bmschif <- reactive({
    rnorm(input$itn, 0.0813, 0.01773)
  })

  tfc1 <- reactive({
    log((chg6(four()$pm25_2) + 3.639) / 3.639)
  }) # chg6 is for pm2.5 threshold as 2.4 ug/m3
  tfc2 <- reactive({
    1 + exp(-(chg6(four()$pm25_2) - 0.763) / 1.896)
  })
  tfc3 <- reactive({
    tfc1() / tfc2()
  })

  tsq1 <- reactive({
    log((chg6(four()$pm25_1) + 3.639) / 3.639)
  })
  tsq2 <- reactive({
    1 + exp(-(chg6(four()$pm25_1) - 0.763) / 1.896)
  })
  tsq3 <- reactive({
    tsq1() / tsq2()
  })

  diffschif <- reactive({
    chrochg(tfc3(), tsq3())
  })

  pctxsschif <- reactive({
    (exp(0.0813 * diffschif()) - 1) / exp(0.0813 * diffschif())
  })
  pctxsschifl95 <- reactive({
    (exp(quantile(bmschif(), 0.025) * diffschif()) - 1) / exp(quantile(bmschif(), 0.025) * diffschif())
  })
  pctxsschifu95 <- reactive({
    (exp(quantile(bmschif(), 0.975) * diffschif()) - 1) / exp(quantile(bmschif(), 0.025) * diffschif())
  })

  fiveschif <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * pctxsschif()
  })
  sixschif <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * pctxsschifl95()
  })
  sevenschif <- reactive({
    (four_age25plus() / 1000000) * four_Mort_chronic() * pctxsschifu95()
  })

  eightschif <- reactive({
    fiveschif() * mean(vm()) * four()$cpiadj * 1000000
  })
  nineschif <- reactive({
    sixschif() * vml() * four()$cpiadj * 1000000
  })
  tenschif <- reactive({
    sevenschif() * vmu() * four()$cpiadj * 1000000
  })
  tenaschif <- reactive({
    100000 * fiveschif() / foura_age25plus()
  })

  leyrschif <- reactive({
    lifeyr(pctxsschif())
  })
  leyrschifl95 <- reactive({
    lifeyr(pctxsschifl95())
  })
  leyrschifu95 <- reactive({
    lifeyr(pctxsschifu95())
  })

  eleven_mortschif <- reactive({
    cbind(
      foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("CD"), get_session_t("PM2.5"), get_session_t("Chronic Exposure Mortality"),
      fiveschif(), sixschif(), sevenschif(), eightschif(), nineschif(), tenschif(), tenaschif(), pctxsschif(), pctxsschifl95(),
      pctxsschifu95(), leyrschif(), leyrschifl95(), leyrschifu95(), get_session_t("mortality"), four_age25plus()
    )
  })


  # prepare SCHIF results at CD level
  cdmortschif0 <- reactive({
    s1 <- eleven_mortschif()
    colnames(s1) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "region", "geotype", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p", "life_year", "L95CI_le", "U95CI_le", "mort", "pop"
    )
    s1
  })

  cdmortschif1 <- reactive({
    if (get_session_lang() == "en") {
      cdmortschif0()[with(cdmortschif0(), order(year, scenario, region, pollutant)), ]
    } else {
      cdmortschif0()[with(cdmortschif0(), order(année, scénario, region, polluant)), ]
    }
  })


  # aggregate non-linear SCHIF at nationally

  Canaggschif1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ year + scenario + pollutant + endpoint, cdmortschif1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ année + scénario + polluant + paramètre, cdmortschif1(), sum, na.action = NULL)
    }
  })

  Canaggschif2 <- reactive({
    cbind(
      Canaggschif1()[c(1:10)], (Canaggschif1()$counts * 100000 / Canaggschif1()$pop), chg7(Canaggschif1()$counts, Canaggschif1()$V7),
      chg7(Canaggschif1()$L95CI_counts, Canaggschif1()$V8), chg7(Canaggschif1()$U95CI_counts, Canaggschif1()$V9)
    )
  })


  Canaggschif4 <- reactive({
    cbind("Canada", Canaggschif2())
  })
  Canaggschif5 <- reactive({
    Canaggschif4()[c(2, 3, 1, 4:15)]
  })

  Canaggschif6 <- reactive({
    s2 <- Canaggschif5()
    colnames(s2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p"
    )
    s2
  })

  Canaggschifle1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + scenario + pollutant + endpoint, cdmortschif1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + scénario + polluant + paramètre, cdmortschif1(), sum, na.action = NULL)
    }
  })

  Canaggschifle2 <- reactive({
    if (get_session_lang() == "en") {
      cbind.data.frame(
        Canaggschifle1()$year, Canaggschifle1()$scenario, "Canada", Canaggschifle1()$pollutant, Canaggschifle1()$endpoint,
        as.data.frame(chg7(Canaggschifle1()$V1, Canaggschifle1()$pop)), as.data.frame(chg7(Canaggschifle1()$V2, Canaggschifle1()$pop)), as.data.frame(chg7(Canaggschifle1()$V3, Canaggschifle1()$pop))
      )
    } else {
      cbind.data.frame(
        Canaggschifle1()$année, Canaggschifle1()$scénario, "Canada", Canaggschifle1()$polluant, Canaggschifle1()$paramètre,
        as.data.frame(chg7(Canaggschifle1()$V1, Canaggschifle1()$pop)), as.data.frame(chg7(Canaggschifle1()$V2, Canaggschifle1()$pop)), as.data.frame(chg7(Canaggschifle1()$V3, Canaggschifle1()$pop))
      )
    }
  })

  Canaggschifle3 <- reactive({
    as.data.frame(Canaggschifle2())
  })


  Canaggschifle4 <- reactive({
    s3 <- Canaggschifle3()
    colnames(s3) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"),
      "life_year", "L95CI_le", "U95CI_le"
    )
    s3
  })

  Canaggschifle5 <- reactive({
    if (get_session_lang() == "en") {
      cbind(as.integer(Canaggschifle4()$year), as.integer(Canaggschifle4()$scenario), Canaggschifle4()[c(3:8)])
    } else {
      cbind(as.integer(Canaggschifle4()$année), as.integer(Canaggschifle4()$scénario), Canaggschifle4()[c(3:8)])
    }
  })

  Canaggschifle6 <- reactive({
    s4 <- Canaggschifle5()
    colnames(s4) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"),
      "life_year", "L95CI_le", "U95CI_le"
    )
    s4
  })

  Canaggschiffinal1 <- reactive({
    left_join(Canaggschif6(), Canaggschifle6(), by = c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint")))
  })


  # aggregated SCHIF mortality by province
  paggschif1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ year + scenario + region + pollutant + endpoint, cdmortschif1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ année + scénario + region + polluant + paramètre, cdmortschif1(), sum, na.action = NULL)
    }
  })

  paggschif3 <- reactive({
    cbind(
      paggschif1()[c(1:11)], chg7(paggschif1()$counts * 100000, paggschif1()$pop), chg7(paggschif1()$counts, paggschif1()$V7),
      chg7(paggschif1()$L95CI_counts, paggschif1()$V8), chg7(paggschif1()$U95CI_counts, paggschif1()$V9)
    )
  })


  paggschif4 <- reactive({
    ps1 <- paggschif3()
    colnames(ps1) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val",
      "counts_per_100K", "proportional_change", "L95CI_p", "U95CI_p"
    )
    ps1
  })

  Proaggschifle1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + +scenario + region + pollutant + endpoint, cdmortschif1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + +scénario + region + polluant + paramètre, cdmortschif1(), sum, na.action = NULL)
    }
  })

  Proaggschifle2 <- reactive({
    cbind(
      Proaggschifle1()[c(1:5)], chg7(Proaggschifle1()$V1, Proaggschifle1()$pop), chg7(Proaggschifle1()$V2, Proaggschifle1()$pop),
      chg7(Proaggschifle1()$V3, Proaggschifle1()$pop)
    )
  })

  Proaggschifle3 <- reactive({
    ps2 <- Proaggschifle2()
    colnames(ps2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"),
      "life_year", "L95CI_le", "U95CI_le"
    )
    ps2
  })

  Proaggschiffinal1 <- reactive({
    left_join(paggschif4(), Proaggschifle3(), by = c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint")))
  })


  # rbind provincial and national results
  CanProaggschiffinal1 <- reactive({
    rbind(Proaggschiffinal1(), Canaggschiffinal1())
  })

  CanProaggschiffinal2 <- reactive({
    merge(CanProaggschiffinal1(), get_session_geocode(), by.x = "region", by.y = "Name", all.x = TRUE)
  })

  CanProaggschiffinal3 <- reactive({
    CanProaggschiffinal2()[c(2, 3, 19, 20, 1, 4:18)]
  })

  CanProaggschiffinal4 <- reactive({
    if (get_session_lang() == "en") {
      CanProaggschiffinal3()[with(CanProaggschiffinal3(), order(CanProaggschiffinal3()$year, CanProaggschiffinal3()$scenario, CanProaggschiffinal3()$geocode)), ]
    } else {
      CanProaggschiffinal3()[with(CanProaggschiffinal3(), order(CanProaggschiffinal3()$année, CanProaggschiffinal3()$scénario, CanProaggschiffinal3()$geocode)), ]
    }
  })

  cdmortschif2 <- reactive({
    cdmortschif1()[c(1:20)]
  })

  cdmortschif3 <- reactive({
    if (get_session_lang() == "en") {
      cdmortschif2()[with(cdmortschif2(), order(cdmortschif2()$year, cdmortschif2()$scenario, cdmortschif2()$geocode)), ]
    } else {
      cdmortschif2()[with(cdmortschif2(), order(cdmortschif2()$année, cdmortschif2()$scénario, cdmortschif2()$geocode)), ]
    }
  })

  allschiffinal1 <- reactive({
    rbind(cdmortschif3(), CanProaggschiffinal4())
  })


  allschiffinal3i <- reactive({
    fs1 <- allschiffinal1()
    colnames(fs1) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "region", "geotype", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p", "life_expectancy_chg", "L95CI_le", "U95CI_le"
    )
    fs1
  })

  allschiffinal3 <- reactive({
    if (get_session_lang() == "en") {
      allschiffinal3i()[with(allschiffinal3i(), order(year, scenario, geocode, geotype, region, pollutant, endpoint)), ]
    } else {
      allschiffinal3i()[with(allschiffinal3i(), order(année, scénario, geocode, geotype, region, polluant, paramètre)), ]
    }
  })

  xallschiffinal3 <- reactive({
    xxhh5 <- allschiffinal3()
    colnames(xxhh5) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Region"), get_session_t("Type of Geography"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"),
      get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Counts per 100,000"),
      get_session_t("Proportional Change"), get_session_t("L95CI Proportional Change"), get_session_t("U95CI Proportional Change"),
      get_session_t("Life Expectancy Change"), get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    xxhh5
  })

  xxallschiffinal3 <- reactive({
    if (get_session_lang() == "en") {
      cbind(xallschiffinal3()[c(1:7)], as.data.frame(sapply((xallschiffinal3()[c(8:14)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xallschiffinal3()[c(1:7)], as.data.frame(sapply((xallschiffinal3()[c(8:14)]), format_numbers2, simplify = FALSE)))
    }
  })

  # SCHIFF TABLE
  output$outputschif <- renderTable({
    tryCatch(
      {
        head(xxpnameschiff(), 3)
      },
      error = function(e) handle_error(e, "outputschif")
    )
  })

  pnameschiff <- reactive({
    merge_data(xxallschiffinal3(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  xpnameschiff <- reactive({
    cbind(pnameschiff()[c(2, 3)], as.integer(pnameschiff_Geocode()), pnameschiff()[c(16, 6:13)])
  })

  xxpnameschiff <- reactive({
    xkphh2 <- xpnameschiff()
    colnames(xkphh2) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"),
      get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"), get_session_t("L95CI Valuation"),
      get_session_t("U95CI Valuation")
    )
    xkphh2
  })

  x1xallschiffinal3 <- reactive({
    merge_data(xallschiffinal3(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  x2xallschiffinal3 <- reactive({
    x1xallschiffinal3()[, c(2, 3, 4, 5, 22, 6:20)]
  })

  x3xallschiffinal3 <- reactive({
    x3schif <- x2xallschiffinal3()
    colnames(x3schif) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Type of Geography"), get_session_t("Region"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"),
      get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Counts per 100,000"), get_session_t("Proportional Change"),
      get_session_t("L95CI Proportional Change"), get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"),
      get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    x3schif
  })

  x4xallschiffinal3 <- reactive({
    if (get_session_lang() == "en") {
      x3xallschiffinal3()[with(x3xallschiffinal3(), order(Year, Scenario, Geocode, Region)), ]
    } else {
      x3xallschiffinal3()[with(x3xallschiffinal3(), order(Année, Scénario, Géocode, Région)), ]
    }
  })

  output$d4 <- downloadHandler(
    filename = function() {
      get_session_t("Mortality_SCHIF.xlsx")
    },
    content = function(file) {
      write_xlsx(x4xallschiffinal3(), path = file)
    }
  )


  # PM2.5 Chronic Exposure Cerebrovascular Mortality
  set.seed(100)
  bmcerebro <- reactive({
    rgamma(input$itn, input$crfcerebro, 1 / chg5(input$scalecerebro, input$incrcerebro))
  })

  set.seed(100)
  vmcerebro <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1cerebro <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjcerebro <- reactive({
    (cpiy()$cpi / cpiy1cerebro()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })
  # rtypecerebro<-reactive({input$rtype})
  # fourcerebro<-reactive({cbind(foura(),rtypecerebro(),cpiadjcerebro())})

  fivecerebro <- reactive({
    (four_age25plus() / 1000000) * four_Mort_cerebro() * af2(input$crfcerebro, chg5(input$scalecerebro, input$incrcerebro), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sixcerebro <- reactive({
    (four_age25plus() / 1000000) * four_Mort_cerebro() * af3(qgamma(0.025, input$crfcerebro, rate = 1 / chg5(input$scalecerebro, input$incrcerebro)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sevencerebro <- reactive({
    (four_age25plus() / 1000000) * four_Mort_cerebro() * af3(qgamma(0.975, input$crfcerebro, rate = 1 / chg5(input$scalecerebro, input$incrcerebro)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })


  eightcerebro <- reactive({
    fivecerebro() * mean(vm()) * cpiadjcerebro() * 1000000
  })

  vmlcerebro <- reactive({
    quantile(vm(), 0.025)
  })
  vmucerebro <- reactive({
    quantile(vm(), 0.975)
  })
  # vmlcerebro <- reactive({
  #   (quantile(af3(bmcerebro(), 1) * vmcerebro(), 0.025)) / af3(quantile(bmcerebro(), 0.025), 1)
  # })
  #
  # vmucerebro <- reactive({
  #   (quantile(af3(bmcerebro(), 1) * vmcerebro(), 0.975)) / af3(quantile(bmcerebro(), 0.975), 1)
  # })

  ninecerebro <- reactive({
    sixcerebro() * vmlcerebro() * cpiadjcerebro() * 1000000
  })

  tencerebro <- reactive({
    sevencerebro() * vmucerebro() * cpiadjcerebro() * 1000000
  })

  tenacerebro <- reactive({
    100000 * fivecerebro() / foura_age25plus()
  })

  pctxsmortcerebro <- reactive({
    af2(input$crfcerebro, chg5(input$scalecerebro, input$incrcerebro), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortl95cerebro <- reactive({
    af3(qgamma(0.025, input$crfcerebro, rate = 1 / chg5(input$scalecerebro, input$incrcerebro)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortu95cerebro <- reactive({
    af3(qgamma(0.975, input$crfcerebro, rate = 1 / chg5(input$scalecerebro, input$incrcerebro)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })

  leyrcerebro <- reactive({
    lifeyr3(pctxsmortcerebro())
  })
  leyrl95cerebro <- reactive({
    lifeyr3(pctxsmortl95cerebro())
  })
  leyru95cerebro <- reactive({
    lifeyr3(pctxsmortu95cerebro())
  })


  eleven_xmort1 <- reactive({
    cbind(
      foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Cerebrovascular"), fivecerebro(), sixcerebro(), sevencerebro(),
      eightcerebro(), ninecerebro(), tencerebro(), tenacerebro(), pctxsmortcerebro(), pctxsmortl95cerebro(), pctxsmortu95cerebro(),
      leyrcerebro(), leyrl95cerebro(), leyru95cerebro(), get_session_t("mortality"), four_age25plus()
    )
  })

  # PM2.5 Chronic Exposure COPD Mortality
  set.seed(100)
  bmcopd <- reactive({
    rgamma(input$itn, input$crfcopd, 1 / chg5(input$scalecopd, input$incrcopd))
  })

  set.seed(100)
  vmcopd <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1copd <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjcopd <- reactive({
    (cpiy()$cpi / cpiy1copd()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  fivecopd <- reactive({
    (four_age25plus() / 1000000) * four_Mort_copd() * af2(input$crfcopd, chg5(input$scalecopd, input$incrcopd), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sixcopd <- reactive({
    (four_age25plus() / 1000000) * four_Mort_copd() * af3(qgamma(0.025, input$crfcopd, rate = 1 / chg5(input$scalecopd, input$incrcopd)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sevencopd <- reactive({
    (four_age25plus() / 1000000) * four_Mort_copd() * af3(qgamma(0.975, input$crfcopd, rate = 1 / chg5(input$scalecopd, input$incrcopd)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })


  eightcopd <- reactive({
    fivecopd() * mean(vm()) * cpiadjcopd() * 1000000
  })
  vmlcopd <- reactive({
    quantile(vm(), 0.025)
  })
  vmucopd <- reactive({
    quantile(vm(), 0.975)
  })

  # vmlcopd <- reactive({
  #   (quantile(af3(bmcopd(), 1) * vmcopd(), 0.025)) / af3(quantile(bmcopd(), 0.025), 1)
  # })
  #
  # vmucopd <- reactive({
  #   (quantile(af3(bmcopd(), 1) * vmcopd(), 0.975)) / af3(quantile(bmcopd(), 0.975), 1)
  # })

  ninecopd <- reactive({
    sixcopd() * vmlcopd() * cpiadjcopd() * 1000000
  })
  tencopd <- reactive({
    sevencopd() * vmucopd() * cpiadjcopd() * 1000000
  })
  tenacopd <- reactive({
    100000 * fivecopd() / foura_age25plus()
  })

  pctxsmortcopd <- reactive({
    af2(input$crfcopd, chg5(input$scalecopd, input$incrcopd), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortl95copd <- reactive({
    af3(qgamma(0.025, input$crfcopd, rate = 1 / chg5(input$scalecopd, input$incrcopd)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortu95copd <- reactive({
    af3(qgamma(0.975, input$crfcopd, rate = 1 / chg5(input$scalecopd, input$incrcopd)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })

  leyrcopd <- reactive({
    lifeyr4(pctxsmortcopd())
  })
  leyrl95copd <- reactive({
    lifeyr4(pctxsmortl95copd())
  })
  leyru95copd <- reactive({
    lifeyr4(pctxsmortu95copd())
  })

  eleven_xmort2 <- reactive({
    cbind(
      foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("COPD"), fivecopd(), sixcopd(), sevencopd(),
      eightcopd(), ninecopd(), tencopd(), tenacopd(), pctxsmortcopd(), pctxsmortl95copd(), pctxsmortu95copd(),
      leyrcopd(), leyrl95copd(), leyru95copd(), get_session_t("mortality"), four_age25plus()
    )
  })

  # PM2.5 Chronic Exposure Ischemic Heart Disease Mortality
  set.seed(100)
  bmischemic <- reactive({
    rgamma(input$itn, input$crfIschem, 1 / chg5(input$scaleIschem, input$incrIschem))
  })

  set.seed(100)
  vmischemic <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1ischemic <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjischemic <- reactive({
    (cpiy()$cpi / cpiy1ischemic()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  fiveischemic <- reactive({
    (four_age25plus() / 1000000) * four_Mort_ischemic() * af2(input$crfIschem, chg5(input$scaleIschem, input$incrIschem), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sixischemic <- reactive({
    (four_age25plus() / 1000000) * four_Mort_ischemic() * af3(qgamma(0.025, input$crfIschem, rate = 1 / chg5(input$scaleIschem, input$incrIschem)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  sevenischemic <- reactive({
    (four_age25plus() / 1000000) * four_Mort_ischemic() * af3(qgamma(0.975, input$crfIschem, rate = 1 / chg5(input$scaleIschem, input$incrIschem)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })


  eightischemic <- reactive({
    fiveischemic() * mean(vm()) * cpiadjischemic() * 1000000
  })
  vmlischemic <- reactive({
    quantile(vm(), 0.025)
  })
  vmuischemic <- reactive({
    quantile(vm(), 0.975)
  })

  # vmlischemic <- reactive({
  #   (quantile(af3(bmischemic(), 1) * vmischemic(), 0.025)) / af3(quantile(bmischemic(), 0.025), 1)
  # })
  #
  # vmuischemic <- reactive({
  #   (quantile(af3(bmischemic(), 1) * vmischemic(), 0.975)) / af3(quantile(bmischemic(), 0.975), 1)
  # })

  nineischemic <- reactive({
    sixischemic() * vmlischemic() * cpiadjischemic() * 1000000
  })
  tenischemic <- reactive({
    sevenischemic() * vmuischemic() * cpiadjischemic() * 1000000
  })
  tenaischemic <- reactive({
    100000 * fiveischemic() / foura_age25plus()
  })

  pctxsmortischemic <- reactive({
    af2(input$crfIschem, chg5(input$scaleIschem, input$incrIschem), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortl95ischemic <- reactive({
    af3(qgamma(0.025, input$crfIschem, rate = 1 / chg5(input$scaleIschem, input$incrIschem)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })
  pctxsmortu95ischemic <- reactive({
    af3(qgamma(0.975, input$crfIschem, rate = 1 / chg5(input$scaleIschem, input$incrIschem)), chg(four()$pm25_2, four()$pm25_1, input$pmthr))
  })

  leyrischemic <- reactive({
    lifeyr5(pctxsmortischemic())
  })
  leyrl95ischemic <- reactive({
    lifeyr5(pctxsmortl95ischemic())
  })
  leyru95ischemic <- reactive({
    lifeyr5(pctxsmortu95ischemic())
  })

  eleven_xmort3 <- reactive({
    cbind(
      foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Ischemic Heart Disease"), fiveischemic(), sixischemic(), sevenischemic(),
      eightischemic(), nineischemic(), tenischemic(), tenaischemic(), pctxsmortischemic(), pctxsmortl95ischemic(), pctxsmortu95ischemic(),
      leyrischemic(), leyrl95ischemic(), leyru95ischemic(), get_session_t("mortality"), four_age25plus()
    )
  })

  # PM2.5 Chronic Exposure Lung Cancer Mortality
  set.seed(100)
  bmlung <- reactive({
    rnorm(input$itn, beta(input$rtypelung, input$crflung, input$incrlung, get_session_t("log-linear")), se(input$rtypelung, input$u95lung, input$l95lung, input$incrlung, get_session_t("log-linear")))
  }) # n=10,000

  set.seed(100)
  vmlung <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1lung <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjlung <- reactive({
    (cpiy()$cpi / cpiy1lung()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  Mortlung <- reactive({
    if (get_session_lang() == "en") {
      (four()$Mort_lung)
    } else {
      (four()$Mort_poumon)
    }
  })

  rtypelung <- reactive({
    input$rtypelung
  })
  fourlung <- reactive({
    cbind(foura(), rtypelung(), cpiadjlung())
  })

  fivelung <- reactive({
    (four_age25plus() / 1000000) * Mortlung() * af(fourlung()$rtypelung, beta(fourlung()$rtypelung, input$crflung, input$incrlung), chg(fourlung()$pm25_2, fourlung()$pm25_1, input$pmthr))
  })
  sixlung <- reactive({
    (four_age25plus() / 1000000) * Mortlung() * af(fourlung()$rtypelung, quantile(bmlung(), 0.025), chg(fourlung()$pm25_2, fourlung()$pm25_1, input$pmthr))
  })
  sevenlung <- reactive({
    (four_age25plus() / 1000000) * Mortlung() * af(fourlung()$rtypelung, quantile(bmlung(), 0.975), chg(fourlung()$pm25_2, fourlung()$pm25_1, input$pmthr))
  })


  eightlung <- reactive({
    fivelung() * mean(vm()) * cpiadjlung() * 1000000
  })

  vmllung <- reactive({
    quantile(vm(), 0.025)
  })
  vmulung <- reactive({
    quantile(vm(), 0.975)
  })

  # vmllung <- reactive({
  #   (quantile(af3(bmlung(), 1) * vmlung(), 0.025)) / af3(quantile(bmlung(), 0.025), 1)
  # })
  #
  # vmulung <- reactive({
  #   (quantile(af3(bmlung(), 1) * vmlung(), 0.975)) / af3(quantile(bmlung(), 0.975), 1)
  # })

  ninelung <- reactive({
    sixlung() * vmllung() * cpiadjlung() * 1000000
  })
  tenlung <- reactive({
    sevenlung() * vmulung() * cpiadjlung() * 1000000
  })
  tenalung <- reactive({
    100000 * fivelung() / foura_age25plus()
  })
  pctxsmortlung <- reactive({
    af(fourlung()$rtypelung, beta(fourlung()$rtypelung, input$crflung, input$incrlung, get_session_t("log-linear")), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  pctxsmortl95lung <- reactive({
    af(fourlung()$rtypelung, quantile(bmlung(), 0.025), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  pctxsmortu95lung <- reactive({
    af(fourlung()$rtypelung, quantile(bmlung(), 0.975), chg(four()$pm25_2, four()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })

  leyrlung <- reactive({
    lifeyr6(pctxsmortlung())
  })
  leyrl95lung <- reactive({
    lifeyr6(pctxsmortl95lung())
  })
  leyru95lung <- reactive({
    lifeyr6(pctxsmortu95lung())
  })

  eleven_xmort4 <- reactive({
    cbind(
      foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Lung Cancer"), fivelung(), sixlung(), sevenlung(),
      eightlung(), ninelung(), tenlung(), tenalung(), pctxsmortlung(), pctxsmortl95lung(), pctxsmortu95lung(),
      leyrlung(), leyrl95lung(), leyru95lung(), get_session_t("mortality"), four_age25plus()
    )
  })

  allx4mort1 <- reactive({
    rbind(eleven_xmort1(), setNames(eleven_xmort2(), names(eleven_xmort1())))
  })
  allx4mort2 <- reactive({
    rbind(allx4mort1(), setNames(eleven_xmort3(), names(allx4mort1())))
  })
  allx4mort3 <- reactive({
    rbind(allx4mort2(), setNames(eleven_xmort4(), names(allx4mort2())))
  })


  xmortreqvars0 <- reactive({
    na_replace(allx4mort3()[c(1:21)], 0)
  })
  xmortreqvars <- reactive({
    cbind(xmortreqvars0()[c(1, 2, 3)], get_session_t("CD"), xmortreqvars0()[c(4:21)])
  })


  rxmort0 <- reactive({
    mm <- xmortreqvars()
    colnames(mm) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "geotype", "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K", "proportional_change", "L95CI_p",
      "U95CI_p", "life_year", "L95CI_le", "U95CI_le", "mort", "pop"
    )
    mm
  })

  rxmort1 <- reactive({
    if (get_session_lang() == "en") {
      rxmort0()[with(rxmort0(), order(rxmort0()$year, rxmort0()$scenario, rxmort0()$geocode, rxmort0()$pollutant)), ]
    } else {
      rxmort0()[with(rxmort0(), order(rxmort0()$année, rxmort0()$scénario, rxmort0()$geocode, rxmort0()$polluant)), ]
    }
  })

  Canaggxmort1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ year + scenario + pollutant + endpoint, rxmort1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ année + scénario + polluant + paramètre, rxmort1(), sum, na.action = NULL)
    }
  })

  Canaggxmort3 <- reactive({
    cbind(
      Canaggxmort1()[c(1:10)], chg7(Canaggxmort1()$counts * 100000, Canaggxmort1()$pop), chg7(Canaggxmort1()$counts, Canaggxmort1()$V7),
      chg7(Canaggxmort1()$L95CI_counts, Canaggxmort1()$V8), chg7(Canaggxmort1()$U95CI_counts, Canaggxmort1()$V9)
    )
  })

  Canaggxmort4 <- reactive({
    cbind("Canada", Canaggxmort3())
  })
  Canaggxmort5 <- reactive({
    Canaggxmort4()[, c(2, 3, 1, 4:15)]
  })
  Canaggxmort6 <- reactive({
    k2 <- Canaggxmort5()
    colnames(k2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p"
    )
    k2
  })

  Canaggxmortle1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + scenario + pollutant + endpoint, rxmort1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + scénario + polluant + paramètre, rxmort1(), sum, na.action = NULL)
    }
  })

  Canaggxmortle2 <- reactive({
    if (get_session_lang() == "en") {
      cbind.data.frame(
        Canaggxmortle1()$year, Canaggxmortle1()$scenario, "Canada", Canaggxmortle1()$pollutant, Canaggxmortle1()$endpoint,
        as.data.frame(chg7(Canaggxmortle1()$V1, Canaggxmortle1()$pop)), as.data.frame(chg7(Canaggxmortle1()$V2, Canaggxmortle1()$pop)), as.data.frame(chg7(Canaggxmortle1()$V3, Canaggxmortle1()$pop))
      )
    } else {
      cbind.data.frame(
        Canaggxmortle1()$année, Canaggxmortle1()$scénario, "Canada", Canaggxmortle1()$polluant, Canaggxmortle1()$paramètre,
        as.data.frame(chg7(Canaggxmortle1()$V1, Canaggxmortle1()$pop)), as.data.frame(chg7(Canaggxmortle1()$V2, Canaggxmortle1()$pop)), as.data.frame(chg7(Canaggxmortle1()$V3, Canaggxmortle1()$pop))
      )
    }
  })

  Canaggxmortle3 <- reactive({
    le2 <- Canaggxmortle2()
    colnames(le2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "life_year",
      "L95CI_le", "U95CI_le"
    )
    le2
  })


  Canaggxmortfinal1 <- reactive({
    left_join(Canaggxmort6(), Canaggxmortle3(), by = c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint")))
  })


  # aggregated cause-specific mortality by province
  paggxmort1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ year + scenario + region + pollutant + endpoint, rxmort1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(
        counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change),
        chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p), pop
      ) ~ année + scénario + region + polluant + paramètre, rxmort1(), sum, na.action = NULL)
    }
  })

  paggxmort3 <- reactive({
    cbind(
      paggxmort1()[c(1:11)], chg7(paggxmort1()$counts * 100000, paggxmort1()$pop), chg7(paggxmort1()$counts, paggxmort1()$V7),
      chg7(paggxmort1()$L95CI_counts, paggxmort1()$V8), chg7(paggxmort1()$U95CI_counts, paggxmort1()$V9)
    )
  })


  # paggxmort3 <- reactive({na_replace(paggxmort2(),0) })
  paggxmort4 <- reactive({
    ac2 <- paggxmort3()
    colnames(ac2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p"
    )
    ac2
  })


  # Proaggxmortle1<- reactive({subset(twelveb(),twelveb_endpoint()=='Mortality'|twelveb_endpoint()=='Chronic Exposure Respiratory Mortality', select=c(1:5,16:20)) })
  Proaggxmortle1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + scenario + region + pollutant + endpoint, rxmort1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + scénario + region + polluant + paramètre, rxmort1(), sum, na.action = NULL)
    }
  })

  Proaggxmortle2 <- reactive({
    cbind(Proaggxmortle1()[c(1:5)], chg7(Proaggxmortle1()$V1, Proaggxmortle1()$pop), chg7(Proaggxmortle1()$V2, Proaggxmortle1()$pop), chg7(Proaggxmortle1()$V3, Proaggxmortle1()$pop))
  })
  Proaggxmortle3 <- reactive({
    le3 <- Proaggxmortle2()
    colnames(le3) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "life_year",
      "L95CI_le", "U95CI_le"
    )
    le3
  })


  Proaggxmortfinal1 <- reactive({
    left_join(paggxmort4(), Proaggxmortle3(), by = c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint")))
  })

  CanProaggxmortfinal1 <- reactive({
    rbind(Proaggxmortfinal1(), Canaggxmortfinal1())
  })

  CanProaggxmortfinal2 <- reactive({
    merge(CanProaggxmortfinal1(), get_session_geocode(), by.x = "region", by.y = "Name", all.x = TRUE)
  })

  CanProaggxmortfinal3 <- reactive({
    CanProaggxmortfinal2()[c(2, 3, 19, 20, 1, 4:18)]
  })

  CanProaggxmortfinal4 <- reactive({
    if (get_session_lang() == "en") {
      CanProaggxmortfinal3()[with(CanProaggxmortfinal3(), order(
        CanProaggxmortfinal3()$year, CanProaggxmortfinal3()$scenario,
        CanProaggxmortfinal3()$geocode, CanProaggxmortfinal3()$pollutant
      )), ]
    } else {
      CanProaggxmortfinal3()[with(CanProaggxmortfinal3(), order(
        CanProaggxmortfinal3()$année, CanProaggxmortfinal3()$scénario,
        CanProaggxmortfinal3()$geocode, CanProaggxmortfinal3()$polluant
      )), ]
    }
  })

  cdxmortfinal0 <- reactive({
    rxmort1()[c(1:20)]
  })

  cdxmortfinal1 <- reactive({
    if (get_session_lang() == "en") {
      cdxmortfinal0()[with(cdxmortfinal0(), order(cdxmortfinal0()$year, cdxmortfinal0()$scenario, cdxmortfinal0()$geocode, cdxmortfinal0()$pollutant)), ]
    } else {
      cdxmortfinal0()[with(cdxmortfinal0(), order(cdxmortfinal0()$année, cdxmortfinal0()$scénario, cdxmortfinal0()$geocode, cdxmortfinal0()$polluant)), ]
    }
  })


  allxmortfinal1 <- reactive({
    rbind(cdxmortfinal1(), CanProaggxmortfinal4())
  })


  allxmortfinal3i <- reactive({
    hh5 <- allxmortfinal1()
    colnames(hh5) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "geotype", "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K", "proportional_change", "L95CI_p",
      "U95CI_p", "life_expectancy_chg", "L95CI_le", "U95CI_le"
    )
    hh5
  })

  allxmortfinal3 <- reactive({
    if (get_session_lang() == "en") {
      allxmortfinal3i()[with(allxmortfinal3i(), order(year, scenario, geocode, geotype, region, pollutant, endpoint)), ]
    } else {
      allxmortfinal3i()[with(allxmortfinal3i(), order(année, scénario, geocode, geotype, region, polluant, paramètre)), ]
    }
  })

  # CASE SPECIFIC MORTALITY TABLE
  output$outputmort <- renderTable({
    tryCatch(
      {
        head(xxpnamemortfinal(), 3)
      },
      error = function(e) handle_error(e, "outputmort")
    )
  })

  xxallxmortfinal3 <- reactive({
    if (get_session_lang() == "en") {
      cbind(xallxmortfinal3()[c(1:7)], as.data.frame(sapply((xallxmortfinal3()[c(8:13)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xallxmortfinal3()[c(1:7)], as.data.frame(sapply((xallxmortfinal3()[c(8:13)]), format_numbers2, simplify = FALSE)))
    }
  })

  pnamemortfinal <- reactive({
    merge_data(xxallxmortfinal3(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  xpnamemortfinal <- reactive({
    cbind(pnamemortfinal()[c(2, 3)], as.integer(pnamemortfinal_Geocode()), pnamemortfinal()[c(15, 6, 7, 8:13)])
  })

  xxpnamemortfinal <- reactive({
    pphh2 <- xpnamemortfinal()
    colnames(pphh2) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"),
      get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"), get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation")
    )
    pphh2
  })

  xallxmortfinal3 <- reactive({
    xhh5 <- allxmortfinal3()
    colnames(xhh5) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Type of Geography"), get_session_t("Region"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"),
      get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Counts per 100,000"), get_session_t("Proportional Change"),
      get_session_t("L95CI Proportional Change"), get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"),
      get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    xhh5
  })

  x1xallxmortfinal3 <- reactive({
    merge_data(xallxmortfinal3(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  x2xallxmortfinal3 <- reactive({
    x1xallxmortfinal3()[, c(2, 3, 4, 5, 22, 6:20)]
  })

  x3xallxmortfinal3 <- reactive({
    x3mort <- x2xallxmortfinal3()
    colnames(x3mort) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Type of Geography"), get_session_t("Region"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"),
      get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Counts per 100,000"), get_session_t("Proportional Change"),
      get_session_t("L95CI Proportional Change"), get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"),
      get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    x3mort
  })

  x4xallxmortfinal3 <- reactive({
    if (get_session_lang() == "en") {
      x3xallxmortfinal3()[with(x3xallxmortfinal3(), order(Year, Scenario, Geocode)), ]
    } else {
      x3xallxmortfinal3()[with(x3xallxmortfinal3(), order(Année, Scénario, Géocode)), ]
    }
  })


  output$d3 <- downloadHandler(
    filename = function() {
      get_session_t("Mortality_Results.xlsx")
    },
    content = function(file) {
      write_xlsx(x4xallxmortfinal3(), path = file)
    }
  )

  # PM2.5 Adult Chronic Bronchitis Cases
  set.seed(100)
  bacbc <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype3, input$pm25_crf3, input$pm25_incr3, get_session_t("log-linear")), se(input$pm25_rtype3, input$pm25_u95_3, input$pm25_l95_3, input$pm25_incr3, get_session_t("log-linear"))))
  }) # chg2 function not allowed distribution values below 0 to ensure lower 95%CI not below 0


  set.seed(100)
  vacbc <- reactive({
    valdist(input$vsl3fm, input$itn, input$vsl3, input$lvsl3, input$uvsl3, input$pcvsl3, input$plvsl3, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiy1acbc <- reactive({
    get_session_cpi()[which(cpi_year() == input$vsl3yr), ]
  })
  cpiadjacbc <- reactive({
    (cpiy()$cpi / cpiy1acbc()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })
  rtypeacbc <- reactive({
    input$pm25_rtype3
  })
  fouracbc <- reactive({
    cbind(foura(), rtypeacbc(), cpiadjacbc())
  })
  fiveacbc <- reactive({
    (fouracbc_age25plus() / 1000000) * fouracbc_Adult_Chronic_Bronchitis_Cases() * af(fouracbc()$rtypeacbc, beta(input$pm25_rtype3, input$pm25_crf3, input$pm25_incr3, get_session_t("log-linear")), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  sixacbc <- reactive({
    (fouracbc_age25plus() / 1000000) * fouracbc_Adult_Chronic_Bronchitis_Cases() * af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.025), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  sevenacbc <- reactive({
    (fouracbc_age25plus() / 1000000) * fouracbc_Adult_Chronic_Bronchitis_Cases() * af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.975), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  eightacbc <- reactive({
    fiveacbc() * mean(vacbc()) * fouracbc()$cpiadjacbc * 1000
  })

  # vacbcl <- reactive({
  #   chg3(af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.025), 1), quantile((af(fouracbc()$rtypeacbc, bacbc(), 1) * vacbc()), 0.025))
  # }) # chg3 function does not allow denominator to be 0, if 0, then the entire value is 0
  # xvacbcl <- reactive({
  #   vacbcl() * fouracbc()$cpiadjacbc
  # })

  # vacbcu <- reactive({
  #   (quantile((af(fouracbc()$rtypeacbc, bacbc(), 1) * vacbc()), 0.975)) / af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.975), 1)
  # })
  # xvacbcu <- reactive({
  #   vacbcu() * fouracbc()$cpiadjacbc * 1000
  # })

  vacbcl <- reactive({
    quantile(vacbc(), 0.025)
  })
  vacbcu <- reactive({
    quantile(vacbc(), 0.975)
  })

  nineacbc <- reactive({
    sixacbc() * vacbcl() * fouracbc()$cpiadjacbc * 1000
  })
  tenacbc <- reactive({
    sevenacbc() * vacbcu() * fouracbc()$cpiadjacbc * 1000
  })
  tenaacbc <- reactive({
    100000 * fiveacbc() / foura_age25plus()
  })
  pctxsacbc <- reactive({
    af(fouracbc()$rtypeacbc, beta(input$pm25_rtype3, input$pm25_crf3, input$pm25_incr3, get_session_t("log-linear")), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  pctxsacbcl95 <- reactive({
    af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.025), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })
  pctxsacbcu95 <- reactive({
    af(fouracbc()$rtypeacbc, quantile(bacbc(), 0.975), chg(fouracbc()$pm25_2, fouracbc()$pm25_1, input$pmthr), get_session_t("log-linear"))
  })


  # pm2.5 concentration change for acute exposure outcomes
  pmregchg <- reactive({
    foura()$pm25_2 - foura()$pm25_1
  })
  pmsq29 <- reactive({
    thr1(foura()$pm25_1, vec1pm25, input$pmthr)
  })
  pmfc29 <- reactive({
    thr1(foura()$pm25_2, vec1pm25, input$pmthr)
  })
  pmthrchg <- reactive({
    pmfc29() - pmsq29()
  })


  # o3 concentration change
  o3regchg <- reactive({
    foura()$o3_2 - foura()$o3_1
  })
  o3sq29 <- reactive({
    thr1(foura()$o3_1, vec1o3, input$o3thr)
  })
  o3fc29 <- reactive({
    thr1(foura()$o3_2, vec1o3, input$o3thr)
  })
  o3thrchg <- reactive({
    o3fc29() - o3sq29()
  })


  # summer o3 concentration change
  summo3regchg <- reactive({
    foura_summero3_2() - foura_summero3_1()
  })
  summo3sq29 <- reactive({
    thr1(foura_summero3_1(), vec1summo3, input$summero3thr)
  })
  summo3fc29 <- reactive({
    thr1(foura_summero3_2(), vec1summo3, input$summero3thr)
  })
  summo3thrchg <- reactive({
    summo3fc29() - summo3sq29()
  })


  # no2 concentration change
  no2regchg <- reactive({
    foura()$no2_2 - foura()$no2_1
  })
  no2sq29 <- reactive({
    thr1(foura()$no2_1, vec1no2, input$no2thr)
  })
  no2fc29 <- reactive({
    thr1(foura()$no2_2, vec1no2, input$no2thr)
  })
  no2thrchg <- reactive({
    no2fc29() - no2sq29()
  })


  # so2 concentration change
  so2regchg <- reactive({
    foura()$so2_2 - foura()$so2_1
  })
  so2sq29 <- reactive({
    thr1(foura()$so2_1, vec1so2, input$so2thr)
  })
  so2fc29 <- reactive({
    thr1(foura()$so2_2, vec1so2, input$so2thr)
  })
  so2thrchg <- reactive({
    so2fc29() - so2sq29()
  })


  # co1h concentration change
  co1regchg <- reactive({
    foura()$co1h_2 - foura()$co1h_1
  })
  co1hsq29 <- reactive({
    thr1(foura()$co1h_1, vec1co1h, input$cothr)
  })
  co1hfc29 <- reactive({
    thr1(foura()$co1h_2, vec1co1h, input$cothr)
  })
  co1thrchg <- reactive({
    co1hfc29() - co1hsq29()
  })


  # co24h concentration change
  co24regchg <- reactive({
    foura()$co24h_2 - foura()$co24h_1
  })
  co24hsq29 <- reactive({
    thr1(foura()$co24h_1, vec1co24h, input$cothr)
  })
  co24hfc29 <- reactive({
    thr1(foura()$co24h_2, vec1co24h, input$cothr)
  })
  co24thrchg <- reactive({
    co24hfc29() - co24hsq29()
  })


  # PM2.5 Cardiac Emergency Room Visits
  set.seed(100)
  bcerv <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype5, input$pm25_crf5, input$pm25_incr5, get_session_t("log-linear")), se(input$pm25_rtype5, input$pm25_u95_5, input$pm25_l95_5, input$pm25_incr5, get_session_t("log-linear"))))
  })

  set.seed(100)
  vcerv <- reactive({
    chg2(valdist(input$sourcevsl5c, input$itn, input$vsl5, input$lvsl5, input$uvsl5, input$pcvsl5, input$plvsl5, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1cerv <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl5b), ]
  })

  cpiadjcerv <- reactive({
    (cpiy()$cpi / cpiy1cerv()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypecerv <- reactive({
    input$pm25_rtype5
  })
  fourcerv <- reactive({
    cbind(foura(), rtypecerv(), cpiadjcerv())
  })

  # for threshold>0 and linear crf
  xpctxscard1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype5, input$pm25_crf5, input$pm25_incr5, get_session_t("log-linear")), afb)
  })
  xpctxscard2a <- reactive({
    thr2(xpctxscard1a(), wtnum)
  }) # thr2 function for concentration change at each distribution point multiple weight number
  ypctxscard1 <- reactive({
    thr3(xpctxscard2a())
  }) # thr3 function is sum of weighted pctxs at each distribution point

  xpctxscard1b <- reactive({
    outer(pmthrchg(), quantile(bcerv(), 0.025), afb)
  })
  xpctxscard2b <- reactive({
    thr2(xpctxscard1b(), wtnum)
  })
  ypctxscard2 <- reactive({
    thr3(xpctxscard2b())
  })

  xpctxscard1c <- reactive({
    outer(pmthrchg(), quantile(bcerv(), 0.975), afb)
  })
  xpctxscard2c <- reactive({
    thr2(xpctxscard1c(), wtnum)
  })
  ypctxscard3 <- reactive({
    thr3(xpctxscard2c())
  })

  # for threshold>0 and log-linear crf
  xpctxscard1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype5, input$pm25_crf5, input$pm25_incr5, get_session_t("log-linear")), afa)
  })
  xpctxscard2d <- reactive({
    thr2(xpctxscard1d(), wtnum)
  })
  zpctxscard1 <- reactive({
    thr3(xpctxscard2d())
  })

  xpctxscard1e <- reactive({
    outer(pmthrchg(), quantile(bcerv(), 0.025), afa)
  })
  xpctxscard2e <- reactive({
    thr2(xpctxscard1e(), wtnum)
  })
  zpctxscard2 <- reactive({
    thr3(xpctxscard2e())
  })

  xpctxscard1f <- reactive({
    outer(pmthrchg(), quantile(bcerv(), 0.975), afa)
  })
  xpctxscard2f <- reactive({
    thr2(xpctxscard1f(), wtnum)
  })
  zpctxscard3 <- reactive({
    thr3(xpctxscard2f())
  })

  pctxscard <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype5 == get_session_t("linear")) {
      xpctxscard1 <- outer(pmregchg(), beta(input$pm25_rtype5, input$pm25_crf5, input$pm25_incr5, get_session_t("log-linear")), afb)
      xpctxscard2 <- outer(pmregchg(), quantile(bcerv(), 0.025), afb)
      xpctxscard3 <- outer(pmregchg(), quantile(bcerv(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype5 == get_session_t("log-linear")) {
      xpctxscard1 <- outer(pmregchg(), beta(input$pm25_rtype5, input$pm25_crf5, input$pm25_incr5, get_session_t("log-linear")), afa)
      xpctxscard2 <- outer(pmregchg(), quantile(bcerv(), 0.025), afa)
      xpctxscard3 <- outer(pmregchg(), quantile(bcerv(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype5 == get_session_t("linear")) {
      xpctxscard1 <- ypctxscard1()
      xpctxscard2 <- ypctxscard2()
      xpctxscard3 <- ypctxscard3()
    } else if (input$pmthr > 0 & input$pm25_rtype5 == get_session_t("log-linear")) {
      xpctxscard1 <- zpctxscard1()
      xpctxscard2 <- zpctxscard2()
      xpctxscard3 <- zpctxscard3()
    }
    return(list(cardaa = xpctxscard1, cardbb = xpctxscard2, cardcc = xpctxscard3))
  })


  fivecerv <- reactive({
    (fourcerv_allages() / 1000000) * fourcerv_Cardiac_Emergency_Room() * pctxscard()$cardaa
  })
  sixcerv <- reactive({
    (fourcerv_allages() / 1000000) * fourcerv_Cardiac_Emergency_Room() * pctxscard()$cardbb
  })
  sevencerv <- reactive({
    (fourcerv_allages() / 1000000) * fourcerv_Cardiac_Emergency_Room() * pctxscard()$cardcc
  })

  eightcerv <- reactive({
    fivecerv() * mean(vcerv()) * fourcerv()$cpiadjcerv
  })
  # vcervl <- reactive({
  #   chg3(af(fourcerv()$rtypecerv, quantile(bcerv(), 0.025), 1), quantile((af(fourcerv()$rtypecerv, bcerv(), 1) * vcerv()), 0.025))
  # })
  # xvcervl <- reactive({
  #   vcervl() * fourcerv()$cpiadjcerv
  # })
  #
  # vcervu <- reactive({
  #   (quantile((af(fourcerv()$rtypecerv, bcerv(), 1) * vcerv()), 0.975)) / af(fourcerv()$rtypecerv, quantile(bcerv(), 0.975), 1)
  # })
  # xvcervu <- reactive({
  #   vcervu() * fourcerv()$cpiadjcerv
  # })

  vcervl <- reactive({
    quantile(vcerv(), 0.025)
  })
  vcervu <- reactive({
    quantile(vcerv(), 0.975)
  })

  ninecerv <- reactive({
    sixcerv() * vcervl() * fourcerv()$cpiadjcerv
  })
  tencerv <- reactive({
    sevencerv() * vcervu() * fourcerv()$cpiadjcerv
  })
  tenacerv <- reactive({
    100000 * fivecerv() / fourcerv_allages()
  })

  pctxscerv <- reactive({
    pctxscard()$cardaa
  })
  pctxscervl95 <- reactive({
    pctxscard()$cardbb
  })
  pctxscervu95 <- reactive({
    pctxscard()$cardcc
  })

  # PM2.5 Cardiac Hospital Admissions
  set.seed(100)
  bcha <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype6, input$pm25_crf6, input$pm25_incr6, get_session_t("log-linear")), se(input$pm25_rtype6, input$pm25_u95_6, input$pm25_l95_6, input$pm25_incr6, get_session_t("log-linear"))))
  })
  rtypecha <- reactive({
    input$pm25_rtype6
  })
  fourcha <- reactive({
    cbind(foura(), rtypecha(), 0)
  })

  # for threshold>0 and linear crf
  xpctxscha1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype6, input$pm25_crf6, input$pm25_incr6, get_session_t("log-linear")), afb)
  })
  xpctxscha2a <- reactive({
    thr2(xpctxscha1a(), wtnum)
  })
  ypctxscha1 <- reactive({
    thr3(xpctxscha2a())
  })


  xpctxscha1b <- reactive({
    outer(pmthrchg(), quantile(bcha(), 0.025), afb)
  })
  xpctxscha2b <- reactive({
    thr2(xpctxscha1b(), wtnum)
  })
  ypctxscha2 <- reactive({
    thr3(xpctxscha2b())
  })

  xpctxscha1c <- reactive({
    outer(pmthrchg(), quantile(bcha(), 0.975), afb)
  })
  xpctxscha2c <- reactive({
    thr2(xpctxscha1c(), wtnum)
  })
  ypctxscha3 <- reactive({
    thr3(xpctxscha2c())
  })

  # for threshold>0 and log-linear crf
  xpctxscha1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype6, input$pm25_crf6, input$pm25_incr6, get_session_t("log-linear")), afa)
  })
  xpctxscha2d <- reactive({
    thr2(xpctxscha1d(), wtnum)
  })
  zpctxscha1 <- reactive({
    thr3(xpctxscha2d())
  })

  xpctxscha1e <- reactive({
    outer(pmthrchg(), quantile(bcha(), 0.025), afa)
  })
  xpctxscha2e <- reactive({
    thr2(xpctxscha1e(), wtnum)
  })
  zpctxscha2 <- reactive({
    thr3(xpctxscha2e())
  })

  xpctxscha1f <- reactive({
    outer(pmthrchg(), quantile(bcha(), 0.975), afa)
  })
  xpctxscha2f <- reactive({
    thr2(xpctxscha1f(), wtnum)
  })
  zpctxscha3 <- reactive({
    thr3(xpctxscha2f())
  })

  xxpctxscha <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype6 == get_session_t("linear")) {
      xpctxscha1 <- outer(pmregchg(), beta(input$pm25_rtype6, input$pm25_crf6, input$pm25_incr6, get_session_t("log-linear")), afb)
      xpctxscha2 <- outer(pmregchg(), quantile(bcha(), 0.025), afb)
      xpctxscha3 <- outer(pmregchg(), quantile(bcha(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype6 == get_session_t("log-linear")) {
      xpctxscha1 <- outer(pmregchg(), beta(input$pm25_rtype6, input$pm25_crf6, input$pm25_incr6, get_session_t("log-linear")), afa)
      xpctxscha2 <- outer(pmregchg(), quantile(bcha(), 0.025), afa)
      xpctxscha3 <- outer(pmregchg(), quantile(bcha(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype6 == get_session_t("linear")) {
      xpctxscha1 <- ypctxscha1()
      xpctxscha2 <- ypctxscha2()
      xpctxscha3 <- ypctxscha3()
    } else if (input$pmthr > 0 & input$pm25_rtype6 == get_session_t("log-linear")) {
      xpctxscha1 <- zpctxscha1()
      xpctxscha2 <- zpctxscha2()
      xpctxscha3 <- zpctxscha3()
    }
    return(list(chaaa = xpctxscha1, chabb = xpctxscha2, chacc = xpctxscha3))
  })

  fivecha <- reactive({
    (fourcha_allages() / 1000000) * fourcha_Cardiac_Hospital() * xxpctxscha()$chaaa
  })
  sixcha <- reactive({
    (fourcha_allages() / 1000000) * fourcha_Cardiac_Hospital() * xxpctxscha()$chabb
  })
  sevencha <- reactive({
    (fourcha_allages() / 1000000) * fourcha_Cardiac_Hospital() * xxpctxscha()$chacc
  })
  tenacha <- reactive({
    100000 * fivecha() / fourcha_allages()
  })

  pctxscha <- reactive({
    xxpctxscha()$chaaa
  })
  pctxschal95 <- reactive({
    xxpctxscha()$chabb
  })
  pctxschau95 <- reactive({
    xxpctxscha()$chacc
  })

  # PM2.5  Asthma Symptom Days
  set.seed(100)
  basd <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype4, input$pm25_crf4, input$pm25_incr4, get_session_t("log-linear")), se(input$pm25_rtype4, input$pm25_u95_4, input$pm25_l95_4, input$pm25_incr4, get_session_t("log-linear"))))
  })

  set.seed(100)
  vasd <- reactive({
    valdist(input$sourcevsl4c, input$itn, as.double(input$vsl4), as.double(input$lvsl4), as.double(input$uvsl4), input$pcvsl4, input$plvsl4, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1asd <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl4b), ]
  })

  cpiadjasd <- reactive({
    (cpiy()$cpi / cpiy1asd()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeasd <- reactive({
    input$pm25_rtype4
  })
  fourasd <- reactive({
    cbind(foura(), rtypeasd(), cpiadjasd())
  })

  # for threshold>0 and linear crf
  xpctxsasd1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype4, input$pm25_crf4, input$pm25_incr4, get_session_t("log-linear")), afb)
  })
  xpctxsasd2a <- reactive({
    thr2(xpctxsasd1a(), wtnum)
  })
  ypctxsasd1 <- reactive({
    thr3(xpctxsasd2a())
  })


  xpctxsasd1b <- reactive({
    outer(pmthrchg(), quantile(basd(), 0.025), afb)
  })
  xpctxsasd2b <- reactive({
    thr2(xpctxsasd1b(), wtnum)
  })
  ypctxsasd2 <- reactive({
    thr3(xpctxsasd2b())
  })

  xpctxsasd1c <- reactive({
    outer(pmthrchg(), quantile(basd(), 0.975), afb)
  })
  xpctxsasd2c <- reactive({
    thr2(xpctxsasd1c(), wtnum)
  })
  ypctxsasd3 <- reactive({
    thr3(xpctxsasd2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsasd1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype4, input$pm25_crf4, input$pm25_incr4, get_session_t("log-linear")), afa)
  })
  xpctxsasd2d <- reactive({
    thr2(xpctxsasd1d(), wtnum)
  })
  zpctxsasd1 <- reactive({
    thr3(xpctxsasd2d())
  })

  xpctxsasd1e <- reactive({
    outer(pmthrchg(), quantile(basd(), 0.025), afa)
  })
  xpctxsasd2e <- reactive({
    thr2(xpctxsasd1e(), wtnum)
  })
  zpctxsasd2 <- reactive({
    thr3(xpctxsasd2e())
  })

  xpctxsasd1f <- reactive({
    outer(pmthrchg(), quantile(basd(), 0.975), afa)
  })
  xpctxsasd2f <- reactive({
    thr2(xpctxsasd1f(), wtnum)
  })
  zpctxsasd3 <- reactive({
    thr3(xpctxsasd2f())
  })

  xxpctxsasd <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype4 == get_session_t("linear")) {
      xpctxsasd1 <- outer(pmregchg(), beta(input$pm25_rtype4, input$pm25_crf4, input$pm25_incr4, get_session_t("log-linear")), afb)
      xpctxsasd2 <- outer(pmregchg(), quantile(basd(), 0.025), afb)
      xpctxsasd3 <- outer(pmregchg(), quantile(basd(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype4 == get_session_t("log-linear")) {
      xpctxsasd1 <- outer(pmregchg(), beta(input$pm25_rtype4, input$pm25_crf4, input$pm25_incr4, get_session_t("log-linear")), afa)
      xpctxsasd2 <- outer(pmregchg(), quantile(basd(), 0.025), afa)
      xpctxsasd3 <- outer(pmregchg(), quantile(basd(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype4 == get_session_t("linear")) {
      xpctxsasd1 <- ypctxsasd1()
      xpctxsasd2 <- ypctxsasd2()
      xpctxsasd3 <- ypctxsasd3()
    } else if (input$pmthr > 0 & input$pm25_rtype4 == get_session_t("log-linear")) {
      xpctxsasd1 <- zpctxsasd1()
      xpctxsasd2 <- zpctxsasd2()
      xpctxsasd3 <- zpctxsasd3()
    }
    return(list(asdaa = xpctxsasd1, asdbb = xpctxsasd2, asdcc = xpctxsasd3))
  })

  fiveasd <- reactive({
    (fourasd_age5_19() / 1000000) * (input$asprev / 100) * fourasd_Asthma_Symptom_Days() * xxpctxsasd()$asdaa
  })
  sixasd <- reactive({
    (fourasd_age5_19() / 1000000) * (input$asprev / 100) * fourasd_Asthma_Symptom_Days() * xxpctxsasd()$asdbb
  })
  sevenasd <- reactive({
    (fourasd_age5_19() / 1000000) * (input$asprev / 100) * fourasd_Asthma_Symptom_Days() * xxpctxsasd()$asdcc
  })

  eightasd <- reactive({
    fiveasd() * mean(vasd()) * fourasd()$cpiadjasd
  })
  # eighta_asd <- reactive({squarese(sixasd(),fiveasd(),sevenasd())})
  nineasd <- reactive({
    sixasd() * quantile(vasd(), 0.025) * fourasd()$cpiadjasd
  })
  tenasd <- reactive({
    sevenasd() * quantile(vasd(), 0.975) * fourasd()$cpiadjasd
  })
  tenaasd <- reactive({
    100000 * fiveasd() / (foura_age5_19() * (input$asprev / 100))
  })

  pctxsasd <- reactive({
    xxpctxsasd()$asdaa
  })
  pctxsasdl95 <- reactive({
    xxpctxsasd()$asdbb
  })
  pctxsasdu95 <- reactive({
    xxpctxsasd()$asdcc
  })

  # PM2.5  Child Acute Bronchitis Episodes
  set.seed(100)
  bcabe <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype7, input$pm25_crf7, input$pm25_incr7, get_session_t("log-linear")), se(input$pm25_rtype7, input$pm25_u95_7, input$pm25_l95_7, input$pm25_incr7, get_session_t("log-linear"))))
  })
  # vcabe<-reactive({sample(c(input$vsl6,input$lvsl6,input$uvsl6), size = input$itn, replace = TRUE, prob = c(input$pcvsl6,input$plvsl6,(1-input$pcvsl6-input$plvsl6)))})
  set.seed(100)
  vcabe <- reactive({
    valdist(input$sourcevsl6c, input$itn, input$vsl6, input$lvsl6, input$uvsl6, input$pcvsl6, input$plvsl6, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiy1cabe <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl6b), ]
  })

  cpiadjcabe <- reactive({
    (cpiy()$cpi / cpiy1cabe()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })
  rtypecabe <- reactive({
    input$pm25_rtype7
  })
  fourcabe <- reactive({
    cbind(foura(), rtypecabe(), cpiadjcabe())
  })


  # for threshold>0 and linear crf
  xpctxscabe1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype7, input$pm25_crf7, input$pm25_incr7, get_session_t("log-linear")), afb)
  })
  xpctxscabe2a <- reactive({
    thr2(xpctxscabe1a(), wtnum)
  })
  ypctxscabe1 <- reactive({
    thr3(xpctxscabe2a())
  })


  xpctxscabe1b <- reactive({
    outer(pmthrchg(), quantile(bcabe(), 0.025), afb)
  })
  xpctxscabe2b <- reactive({
    thr2(xpctxscabe1b(), wtnum)
  })
  ypctxscabe2 <- reactive({
    thr3(xpctxscabe2b())
  })

  xpctxscabe1c <- reactive({
    outer(pmthrchg(), quantile(bcabe(), 0.975), afb)
  })
  xpctxscabe2c <- reactive({
    thr2(xpctxscabe1c(), wtnum)
  })
  ypctxscabe3 <- reactive({
    thr3(xpctxscabe2c())
  })

  # for threshold>0 and log-linear crf
  xpctxscabe1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype7, input$pm25_crf7, input$pm25_incr7, get_session_t("log-linear")), afa)
  })
  xpctxscabe2d <- reactive({
    thr2(xpctxscabe1d(), wtnum)
  })
  zpctxscabe1 <- reactive({
    thr3(xpctxscabe2d())
  })

  xpctxscabe1e <- reactive({
    outer(pmthrchg(), quantile(bcabe(), 0.025), afa)
  })
  xpctxscabe2e <- reactive({
    thr2(xpctxscabe1e(), wtnum)
  })
  zpctxscabe2 <- reactive({
    thr3(xpctxscabe2e())
  })

  xpctxscabe1f <- reactive({
    outer(pmthrchg(), quantile(bcabe(), 0.975), afa)
  })
  xpctxscabe2f <- reactive({
    thr2(xpctxscabe1f(), wtnum)
  })
  zpctxscabe3 <- reactive({
    thr3(xpctxscabe2f())
  })

  xxpctxscabe <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype7 == get_session_t("linear")) {
      xpctxscabe1 <- outer(pmregchg(), beta(input$pm25_rtype7, input$pm25_crf7, input$pm25_incr7, get_session_t("log-linear")), afb)
      xpctxscabe2 <- outer(pmregchg(), quantile(bcabe(), 0.025), afb)
      xpctxscabe3 <- outer(pmregchg(), quantile(bcabe(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype7 == get_session_t("log-linear")) {
      xpctxscabe1 <- outer(pmregchg(), beta(input$pm25_rtype7, input$pm25_crf7, input$pm25_incr7, get_session_t("log-linear")), afa)
      xpctxscabe2 <- outer(pmregchg(), quantile(bcabe(), 0.025), afa)
      xpctxscabe3 <- outer(pmregchg(), quantile(bcabe(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype7 == get_session_t("linear")) {
      xpctxscabe1 <- ypctxscabe1()
      xpctxscabe2 <- ypctxscabe2()
      xpctxscabe3 <- ypctxscabe3()
    } else if (input$pmthr > 0 & input$pm25_rtype7 == get_session_t("log-linear")) {
      xpctxscabe1 <- zpctxscabe1()
      xpctxscabe2 <- zpctxscabe2()
      xpctxscabe3 <- zpctxscabe3()
    }
    return(list(cabeaa = xpctxscabe1, cabebb = xpctxscabe2, cabecc = xpctxscabe3))
  })


  fivecabe <- reactive({
    (fourcabe_age5_19() / 1000000) * fourcabe_Child_Acute_Bronchitis() * xxpctxscabe()$cabeaa
  })
  sixcabe <- reactive({
    (fourcabe_age5_19() / 1000000) * fourcabe_Child_Acute_Bronchitis() * xxpctxscabe()$cabebb
  })
  sevencabe <- reactive({
    (fourcabe_age5_19() / 1000000) * fourcabe_Child_Acute_Bronchitis() * xxpctxscabe()$cabecc
  })

  eightcabe <- reactive({
    fivecabe() * mean(vcabe()) * fourcabe()$cpiadjcabe
  })

  # vcabel <- reactive({
  #   chg3(af(fourcabe()$rtypecabe, quantile(bcabe(), 0.025), 1), quantile((af(fourcabe()$rtypecabe, bcabe(), 1) * vcabe()), 0.025))
  # })
  # xvcabel <- reactive({
  #   vcabel() * fourcabe()$cpiadjcabe
  # })
  #
  # vcabeu <- reactive({
  #   (quantile((af(fourcabe()$rtypecabe, bcabe(), 1) * vcabe()), 0.975)) / af(fourcabe()$rtypecabe, quantile(bcabe(), 0.975), 1)
  # })
  # xvcabeu <- reactive({
  #   vcabeu() * fourcabe()$cpiadjcabe
  # })
  vcabel <- reactive({
    quantile(vcabe(), 0.025)
  })
  vcabeu <- reactive({
    quantile(vcabe(), 0.975)
  })

  ninecabe <- reactive({
    sixcabe() * vcabel() * fourcabe()$cpiadjcabe
  })
  tencabe <- reactive({
    sevencabe() * vcabeu() * fourcabe()$cpiadjcabe
  })
  tenacabe <- reactive({
    100000 * fivecabe() / foura_age5_19()
  })

  pctxscabe <- reactive({
    xxpctxscabe()$cabeaa
  })
  pctxscabel95 <- reactive({
    xxpctxscabe()$cabebb
  })
  pctxscabeu95 <- reactive({
    xxpctxscabe()$cabecc
  })


  # PM2.5  Respiratory Emergency Room Visits
  set.seed(100)
  brerv <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype8, input$pm25_crf8, input$pm25_incr8, get_session_t("log-linear")), se(input$pm25_rtype8, input$pm25_u95_8, input$pm25_l95_8, input$pm25_incr8, get_session_t("log-linear"))))
  })

  set.seed(100)
  vrerv <- reactive({
    chg2(valdist(input$sourcevsl8c, input$itn, input$vsl9, input$lvsl9, input$uvsl9, input$pcvsl9, input$plvsl9, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1rerv <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl9b), ]
  })

  cpiadjrerv <- reactive({
    (cpiy()$cpi / cpiy1rerv()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypererv <- reactive({
    input$pm25_rtype8
  })
  fourrerv <- reactive({
    cbind(foura(), rtypererv(), cpiadjrerv())
  })


  # for threshold>0 and linear crf
  xpctexcess1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype8, input$pm25_crf8, input$pm25_incr8, get_session_t("log-linear")), afb)
  })
  xpctexcess2a <- reactive({
    thr2(xpctexcess1a(), wtnum)
  })
  ypctexcess1 <- reactive({
    thr3(xpctexcess2a())
  })

  xpctexcess1b <- reactive({
    outer(pmthrchg(), quantile(brerv(), 0.025), afb)
  })
  xpctexcess2b <- reactive({
    thr2(xpctexcess1b(), wtnum)
  })
  ypctexcess2 <- reactive({
    thr3(xpctexcess2b())
  })

  xpctexcess1c <- reactive({
    outer(pmthrchg(), quantile(brerv(), 0.975), afb)
  })
  xpctexcess2c <- reactive({
    thr2(xpctexcess1c(), wtnum)
  })
  ypctexcess3 <- reactive({
    thr3(xpctexcess2c())
  })

  # for threshold>0 and log-linear crf
  xpctexcess1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype8, input$pm25_crf8, input$pm25_incr8, get_session_t("log-linear")), afa)
  })
  xpctexcess2d <- reactive({
    thr2(xpctexcess1d(), wtnum)
  })
  zpctexcess1 <- reactive({
    thr3(xpctexcess2d())
  })

  xpctexcess1e <- reactive({
    outer(pmthrchg(), quantile(brerv(), 0.025), afa)
  })
  xpctexcess2e <- reactive({
    thr2(xpctexcess1e(), wtnum)
  })
  zpctexcess2 <- reactive({
    thr3(xpctexcess2e())
  })

  xpctexcess1f <- reactive({
    outer(pmthrchg(), quantile(brerv(), 0.975), afa)
  })
  xpctexcess2f <- reactive({
    thr2(xpctexcess1f(), wtnum)
  })
  zpctexcess3 <- reactive({
    thr3(xpctexcess2f())
  })

  pctexcessrerv <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype8 == get_session_t("linear")) {
      xpctexcess1 <- outer(pmregchg(), beta(input$pm25_rtype8, input$pm25_crf8, input$pm25_incr8, get_session_t("log-linear")), afb)
      xpctexcess2 <- outer(pmregchg(), quantile(brerv(), 0.025), afb)
      xpctexcess3 <- outer(pmregchg(), quantile(brerv(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype8 == get_session_t("log-linear")) {
      xpctexcess1 <- outer(pmregchg(), beta(input$pm25_rtype8, input$pm25_crf8, input$pm25_incr8, get_session_t("log-linear")), afa)
      xpctexcess2 <- outer(pmregchg(), quantile(brerv(), 0.025), afa)
      xpctexcess3 <- outer(pmregchg(), quantile(brerv(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype8 == get_session_t("linear")) {
      xpctexcess1 <- ypctexcess1()
      xpctexcess2 <- ypctexcess2()
      xpctexcess3 <- ypctexcess3()
    } else if (input$pmthr > 0 & input$pm25_rtype8 == get_session_t("log-linear")) {
      xpctexcess1 <- zpctexcess1()
      xpctexcess2 <- zpctexcess2()
      xpctexcess3 <- zpctexcess3()
    }
    return(list(aa = xpctexcess1, bb = xpctexcess2, cc = xpctexcess3))
  })

  output$d18 <- downloadHandler(
    filename = function() {
      "testfile2.xlsx"
    },
    content = function(file) {
      write_xlsx(xwpctexcess(), path = file)
    }
  )

  fivererv <- reactive({
    (fourrerv_allages() / 1000000) * fourrerv_Respiratory_Emergency_Room() * pctexcessrerv()$aa
  })
  sixrerv <- reactive({
    (fourrerv_allages() / 1000000) * fourrerv_Respiratory_Emergency_Room() * pctexcessrerv()$bb
  })
  sevenrerv <- reactive({
    (fourrerv_allages() / 1000000) * fourrerv_Respiratory_Emergency_Room() * pctexcessrerv()$cc
  })

  eightrerv <- reactive({
    fivererv() * mean(vrerv()) * fourrerv()$cpiadjrerv
  })

  # vrervl <- reactive({
  #   chg3(af(fourrerv()$rtypererv, quantile(brerv(), 0.025), 1), quantile((af(fourrerv()$rtypererv, brerv(), 1) * vrerv()), 0.025))
  # })
  #
  # vrervu <- reactive({
  #   (quantile((af(fourrerv()$rtypererv, brerv(), 1) * vrerv()), 0.975)) / af(fourrerv()$rtypererv, quantile(brerv(), 0.975), 1)
  # })
  vrervl <- reactive({
    quantile(vrerv(), 0.025)
  })
  vrervu <- reactive({
    quantile(vrerv(), 0.975)
  })

  ninererv <- reactive({
    sixrerv() * vrervl() * fourrerv()$cpiadjrerv
  })
  tenrerv <- reactive({
    sevenrerv() * vrervu() * fourrerv()$cpiadjrerv
  })
  tenarerv <- reactive({
    100000 * fivererv() / fourrerv_allages()
  })

  pctxsrerv <- reactive({
    pctexcessrerv()$aa
  })
  pctxsrervl95 <- reactive({
    pctexcessrerv()$bb
  })
  pctxsrervu95 <- reactive({
    pctexcessrerv()$cc
  })


  # PM2.5  Respiratory Hospital Admissions
  set.seed(100)
  brha <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype9, input$pm25_crf9, input$pm25_incr9, get_session_t("log-linear")), se(input$pm25_rtype9, input$pm25_u95_9, input$pm25_l95_9, input$pm25_incr9, get_session_t("log-linear"))))
  })

  rtyperha <- reactive({
    input$pm25_rtype9
  })
  fourrha <- reactive({
    cbind(foura(), rtyperha(), 0)
  })

  xpctxsrha1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype9, input$pm25_crf9, input$pm25_incr9, get_session_t("log-linear")), afb)
  })
  xpctxsrha2a <- reactive({
    thr2(xpctxsrha1a(), wtnum)
  })
  ypctxsrha1 <- reactive({
    thr3(xpctxsrha2a())
  })


  xpctxsrha1b <- reactive({
    outer(pmthrchg(), quantile(brha(), 0.025), afb)
  })
  xpctxsrha2b <- reactive({
    thr2(xpctxsrha1b(), wtnum)
  })
  ypctxsrha2 <- reactive({
    thr3(xpctxsrha2b())
  })

  xpctxsrha1c <- reactive({
    outer(pmthrchg(), quantile(brha(), 0.975), afb)
  })
  xpctxsrha2c <- reactive({
    thr2(xpctxsrha1c(), wtnum)
  })
  ypctxsrha3 <- reactive({
    thr3(xpctxsrha2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsrha1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype9, input$pm25_crf9, input$pm25_incr9, get_session_t("log-linear")), afa)
  })
  xpctxsrha2d <- reactive({
    thr2(xpctxsrha1d(), wtnum)
  })
  zpctxsrha1 <- reactive({
    thr3(xpctxsrha2d())
  })

  xpctxsrha1e <- reactive({
    outer(pmthrchg(), quantile(brha(), 0.025), afa)
  })
  xpctxsrha2e <- reactive({
    thr2(xpctxsrha1e(), wtnum)
  })
  zpctxsrha2 <- reactive({
    thr3(xpctxsrha2e())
  })

  xpctxsrha1f <- reactive({
    outer(pmthrchg(), quantile(brha(), 0.975), afa)
  })
  xpctxsrha2f <- reactive({
    thr2(xpctxsrha1f(), wtnum)
  })
  zpctxsrha3 <- reactive({
    thr3(xpctxsrha2f())
  })

  xxpctxsrha <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype9 == get_session_t("linear")) {
      xpctxsrha1 <- outer(pmregchg(), beta(input$pm25_rtype9, input$pm25_crf9, input$pm25_incr9, get_session_t("log-linear")), afb)
      xpctxsrha2 <- outer(pmregchg(), quantile(brha(), 0.025), afb)
      xpctxsrha3 <- outer(pmregchg(), quantile(brha(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype9 == get_session_t("log-linear")) {
      xpctxsrha1 <- outer(pmregchg(), beta(input$pm25_rtype9, input$pm25_crf9, input$pm25_incr9, get_session_t("log-linear")), afa)
      xpctxsrha2 <- outer(pmregchg(), quantile(brha(), 0.025), afa)
      xpctxsrha3 <- outer(pmregchg(), quantile(brha(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype9 == get_session_t("linear")) {
      xpctxsrha1 <- ypctxsrha1()
      xpctxsrha2 <- ypctxsrha2()
      xpctxsrha3 <- ypctxsrha3()
    } else if (input$pmthr > 0 & input$pm25_rtype9 == get_session_t("log-linear")) {
      xpctxsrha1 <- zpctxsrha1()
      xpctxsrha2 <- zpctxsrha2()
      xpctxsrha3 <- zpctxsrha3()
    }
    return(list(rhaaa = xpctxsrha1, rhabb = xpctxsrha2, rhacc = xpctxsrha3))
  })

  fiverha <- reactive({
    (fourrha_allages() / 1000000) * fourrha_Respiratory_Hospital() * xxpctxsrha()$rhaaa
  })
  sixrha <- reactive({
    (fourrha_allages() / 1000000) * fourrha_Respiratory_Hospital() * xxpctxsrha()$rhabb
  })
  sevenrha <- reactive({
    (fourrha_allages() / 1000000) * fourrha_Respiratory_Hospital() * xxpctxsrha()$rhacc
  })

  tenarha <- reactive({
    100000 * fiverha() / fourrha_allages()
  })

  pctxsrha <- reactive({
    xxpctxsrha()$rhaaa
  })
  pctxsrhal95 <- reactive({
    xxpctxsrha()$rhabb
  })
  pctxsrhau95 <- reactive({
    xxpctxsrha()$rhacc
  })


  # PM2.5 Restricted Activity Days
  set.seed(100)
  brad <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype10, input$pm25_crf10, input$pm25_incr10, get_session_t("log-linear")), se(input$pm25_rtype10, input$pm25_u95_10, input$pm25_l95_10, input$pm25_incr10, get_session_t("log-linear"))))
  })

  set.seed(100)
  vrad <- reactive({
    chg2(valdist(input$sourcevsl10c, input$itn, input$vsl10, input$lvsl10, input$uvsl10, input$pcvsl10, input$plvsl10, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1rad <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl10b), ]
  })

  cpiadjrad <- reactive({
    (cpiy()$cpi / cpiy1rad()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })
  rtyperad <- reactive({
    input$pm25_rtype10
  })
  fourrad <- reactive({
    cbind(foura(), rtyperad(), cpiadjrad())
  })

  # for threshold>0 and linear crf
  xpctxsrad1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype10, input$pm25_crf10, input$pm25_incr10, get_session_t("log-linear")), afb)
  })
  xpctxsrad2a <- reactive({
    thr2(xpctxsrad1a(), wtnum)
  })
  ypctxsrad1 <- reactive({
    thr3(xpctxsrad2a())
  })


  xpctxsrad1b <- reactive({
    outer(pmthrchg(), quantile(brad(), 0.025), afb)
  })
  xpctxsrad2b <- reactive({
    thr2(xpctxsrad1b(), wtnum)
  })
  ypctxsrad2 <- reactive({
    thr3(xpctxsrad2b())
  })

  xpctxsrad1c <- reactive({
    outer(pmthrchg(), quantile(brad(), 0.975), afb)
  })
  xpctxsrad2c <- reactive({
    thr2(xpctxsrad1c(), wtnum)
  })
  ypctxsrad3 <- reactive({
    thr3(xpctxsrad2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsrad1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype10, input$pm25_crf10, input$pm25_incr10, get_session_t("log-linear")), afa)
  })
  xpctxsrad2d <- reactive({
    thr2(xpctxsrad1d(), wtnum)
  })
  zpctxsrad1 <- reactive({
    thr3(xpctxsrad2d())
  })

  xpctxsrad1e <- reactive({
    outer(pmthrchg(), quantile(brad(), 0.025), afa)
  })
  xpctxsrad2e <- reactive({
    thr2(xpctxsrad1e(), wtnum)
  })
  zpctxsrad2 <- reactive({
    thr3(xpctxsrad2e())
  })

  xpctxsrad1f <- reactive({
    outer(pmthrchg(), quantile(brad(), 0.975), afa)
  })
  xpctxsrad2f <- reactive({
    thr2(xpctxsrad1f(), wtnum)
  })
  zpctxsrad3 <- reactive({
    thr3(xpctxsrad2f())
  })

  xxpctxsrad <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype10 == get_session_t("linear")) {
      xpctxsrad1 <- outer(pmregchg(), beta(input$pm25_rtype10, input$pm25_crf10, input$pm25_incr10, get_session_t("log-linear")), afb)
      xpctxsrad2 <- outer(pmregchg(), quantile(brad(), 0.025), afb)
      xpctxsrad3 <- outer(pmregchg(), quantile(brad(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype10 == get_session_t("log-linear")) {
      xpctxsrad1 <- outer(pmregchg(), beta(input$pm25_rtype10, input$pm25_crf10, input$pm25_incr10, get_session_t("log-linear")), afa)
      xpctxsrad2 <- outer(pmregchg(), quantile(brad(), 0.025), afa)
      xpctxsrad3 <- outer(pmregchg(), quantile(brad(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype10 == get_session_t("linear")) {
      xpctxsrad1 <- ypctxsrad1()
      xpctxsrad2 <- ypctxsrad2()
      xpctxsrad3 <- ypctxsrad3()
    } else if (input$pmthr > 0 & input$pm25_rtype10 == get_session_t("log-linear")) {
      xpctxsrad1 <- zpctxsrad1()
      xpctxsrad2 <- zpctxsrad2()
      xpctxsrad3 <- zpctxsrad3()
    }
    return(list(radaa = xpctxsrad1, radbb = xpctxsrad2, radcc = xpctxsrad3))
  })


  fiverad <- reactive({
    if (get_session_lang() == "en") {
      (popnonasthma(input$asprev, fourrad()$age5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radaa
    } else {
      (popnonasthma(input$asprev, fourrad()$ans5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radaa
    }
  })

  sixrad <- reactive({
    if (get_session_lang() == "en") {
      (popnonasthma(input$asprev, fourrad()$age5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radbb
    } else {
      (popnonasthma(input$asprev, fourrad()$ans5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radbb
    }
  })

  sevenrad <- reactive({
    if (get_session_lang() == "en") {
      (popnonasthma(input$asprev, fourrad()$age5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radcc
    } else {
      (popnonasthma(input$asprev, fourrad()$ans5_19, fourrad_age20plus()) / 1000000) * fourrad_Restricted_Activity_Days() * xxpctxsrad()$radcc
    }
  })


  eightrad <- reactive({
    fiverad() * input$vsl10 * fourrad()$cpiadjrad
  })

  # vradl <- reactive({
  #   chg3(af(fourrad()$rtyperad, quantile(brad(), 0.025), 1), quantile((af(fourrad()$rtyperad, brad(), 1) * vrad()), 0.025))
  # })
  #
  # vradu <- reactive({
  #   (quantile((af(fourrad()$rtyperad, brad(), 1) * vrad()), 0.975)) / af(fourrad()$rtyperad, quantile(brad(), 0.975), 1)
  # })

  vradl <- reactive({
    quantile(vrad(), 0.025)
  })
  vradu <- reactive({
    quantile(vrad(), 0.975)
  })

  ninerad <- reactive({
    sixrad() * vradl() * fourrad()$cpiadjrad
  })
  tenrad <- reactive({
    sevenrad() * vradu() * fourrad()$cpiadjrad
  })

  tenarad <- reactive({
    if (get_session_lang() == "en") {
      100000 * fiverad() / popnonasthma(input$asprev, fourrad()$age5_19, fourrad_age20plus())
    } else {
      100000 * fiverad() / popnonasthma(input$asprev, fourrad()$ans5_19, fourrad_age20plus())
    }
  })

  pctxsrad <- reactive({
    xxpctxsrad()$radaa
  })
  pctxsradl95 <- reactive({
    xxpctxsrad()$radbb
  })
  pctxsradu95 <- reactive({
    xxpctxsrad()$radcc
  })


  # PM2.5 Acute Respiratory Symptom Days
  set.seed(100)
  barsd <- reactive({
    chg2(rnorm(input$itn, beta(input$pm25_rtype2, input$pm25_crf2, input$pm25_incr2, get_session_t("log-linear")), se(input$pm25_rtype2, input$pm25_u95_2, input$pm25_l95_2, input$pm25_incr2, get_session_t("log-linear"))))
  })

  set.seed(100)
  varsd <- reactive({
    chg2(valdist(input$vsl2fm, input$itn, input$vsl2, input$lvsl2, input$uvsl2, input$pcvsl2, input$plvsl2, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1arsd <- reactive({
    get_session_cpi()[which(cpi_year() == input$vsl2yr), ]
  })

  cpiadjarsd <- reactive({
    (cpiy()$cpi / cpiy1arsd()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypearsd <- reactive({
    input$pm25_rtype2
  })
  fourarsd <- reactive({
    cbind(foura(), rtypearsd(), cpiadjarsd())
  })

  # for threshold>0 and linear crf
  xpctxsarsd1a <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype2, input$pm25_crf2, input$pm25_incr2, get_session_t("log-linear")), afb)
  })
  xpctxsarsd2a <- reactive({
    thr2(xpctxsarsd1a(), wtnum)
  })
  ypctxsarsd1 <- reactive({
    thr3(xpctxsarsd2a())
  })


  xpctxsarsd1b <- reactive({
    outer(pmthrchg(), quantile(barsd(), 0.025), afb)
  })
  xpctxsarsd2b <- reactive({
    thr2(xpctxsarsd1b(), wtnum)
  })
  ypctxsarsd2 <- reactive({
    thr3(xpctxsarsd2b())
  })

  xpctxsarsd1c <- reactive({
    outer(pmthrchg(), quantile(barsd(), 0.975), afb)
  })
  xpctxsarsd2c <- reactive({
    thr2(xpctxsarsd1c(), wtnum)
  })
  ypctxsarsd3 <- reactive({
    thr3(xpctxsarsd2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsarsd1d <- reactive({
    outer(pmthrchg(), beta(input$pm25_rtype2, input$pm25_crf2, input$pm25_incr2, get_session_t("log-linear")), afa)
  })
  xpctxsarsd2d <- reactive({
    thr2(xpctxsarsd1d(), wtnum)
  })
  zpctxsarsd1 <- reactive({
    thr3(xpctxsarsd2d())
  })

  xpctxsarsd1e <- reactive({
    outer(pmthrchg(), quantile(barsd(), 0.025), afa)
  })
  xpctxsarsd2e <- reactive({
    thr2(xpctxsarsd1e(), wtnum)
  })
  zpctxsarsd2 <- reactive({
    thr3(xpctxsarsd2e())
  })

  xpctxsarsd1f <- reactive({
    outer(pmthrchg(), quantile(barsd(), 0.975), afa)
  })
  xpctxsarsd2f <- reactive({
    thr2(xpctxsarsd1f(), wtnum)
  })
  zpctxsarsd3 <- reactive({
    thr3(xpctxsarsd2f())
  })

  xxpctxsarsd <- reactive({
    if (input$pmthr == 0 & input$pm25_rtype2 == get_session_t("linear")) {
      xpctxsarsd1 <- outer(pmregchg(), beta(input$pm25_rtype2, input$pm25_crf2, input$pm25_incr2, get_session_t("log-linear")), afb)
      xpctxsarsd2 <- outer(pmregchg(), quantile(barsd(), 0.025), afb)
      xpctxsarsd3 <- outer(pmregchg(), quantile(barsd(), 0.975), afb)
    } else if (input$pmthr == 0 & input$pm25_rtype2 == get_session_t("log-linear")) {
      xpctxsarsd1 <- outer(pmregchg(), beta(input$pm25_rtype2, input$pm25_crf2, input$pm25_incr2, get_session_t("log-linear")), afa)
      xpctxsarsd2 <- outer(pmregchg(), quantile(barsd(), 0.025), afa)
      xpctxsarsd3 <- outer(pmregchg(), quantile(barsd(), 0.975), afa)
    } else if (input$pmthr > 0 & input$pm25_rtype2 == get_session_t("linear")) {
      xpctxsarsd1 <- ypctxsarsd1()
      xpctxsarsd2 <- ypctxsarsd2()
      xpctxsarsd3 <- ypctxsarsd3()
    } else if (input$pmthr > 0 & input$pm25_rtype2 == get_session_t("log-linear")) {
      xpctxsarsd1 <- zpctxsarsd1()
      xpctxsarsd2 <- zpctxsarsd2()
      xpctxsarsd3 <- zpctxsarsd3()
    }
    return(list(arsdaa = xpctxsarsd1, arsdbb = xpctxsarsd2, arsdcc = xpctxsarsd3))
  })


  fivearsd <- reactive({
    (popnonasthma(input$asprev, fourarsd_age5_19(), fourarsd_age20plus()) / 1000000) * fourarsd_Acute_Resp_Symptom_Days() * xxpctxsarsd()$arsdaa
  })
  xfivearsd <- reactive({
    fivearsd() - fiverad()
  })

  sixarsd <- reactive({
    (popnonasthma(input$asprev, fourarsd_age5_19(), fourarsd_age20plus()) / 1000000) * fourarsd_Acute_Resp_Symptom_Days() * xxpctxsarsd()$arsdbb
  })
  xsixarsd <- reactive({
    sixarsd() - sixrad()
  })


  sevenarsd <- reactive({
    (popnonasthma(input$asprev, fourarsd_age5_19(), fourarsd_age20plus()) / 1000000) * fourarsd_Acute_Resp_Symptom_Days() * xxpctxsarsd()$arsdcc
  })
  xsevenarsd <- reactive({
    sevenarsd() - sevenrad()
  })

  eightarsd <- reactive({
    xfivearsd() * input$vsl2 * fourarsd()$cpiadjarsd
  })

  # varsdl <- reactive({
  #   chg3(af(fourarsd()$rtypearsd, quantile(barsd(), 0.025), 1), quantile((af(fourarsd()$rtypearsd, barsd(), 1) * varsd()), 0.025))
  # })
  # xvarsdl <- reactive({
  #   varsdl() * fourarsd()$cpiadjarsd
  # })
  #
  # varsdu <- reactive({
  #   (quantile((af(fourarsd()$rtypearsd, barsd(), 1) * varsd()), 0.975)) / af(fourarsd()$rtypearsd, quantile(barsd(), 0.975), 1)
  # })
  # xvarsdu <- reactive({
  #   varsdu() * fourarsd()$cpiadjarsd
  # })

  varsdl <- reactive({
    quantile(varsd(), 0.025)
  })
  varsdu <- reactive({
    quantile(varsd(), 0.975)
  })

  ninearsd <- reactive({
    xsixarsd() * varsdl() * fourarsd()$cpiadjarsd
  })
  tenarsd <- reactive({
    xsevenarsd() * varsdu() * fourarsd()$cpiadjarsd
  })
  tenaarsd <- reactive({
    100000 * fivearsd() / popnonasthma(input$asprev, fourarsd_age5_19(), fourarsd_age20plus())
  })

  pctxsarsd <- reactive({
    xxpctxsarsd()$arsdaa
  })
  pctxsarsdl95 <- reactive({
    xxpctxsarsd()$arsdbb
  })
  pctxsarsdu95 <- reactive({
    xxpctxsarsd()$arsdcc
  })

  # NO2 Acute Exposure Mortality
  set.seed(100)
  baem <- reactive({
    rnorm(input$itn, beta(input$no2_rtype, input$no2_crf, input$no2_incr, get_session_t("log-linear")), se(input$no2_rtype, input$no2_u95, input$no2_l95, input$no2_incr, get_session_t("log-linear")))
  })

  set.seed(100)
  vaem <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiyaem <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  })
  cpiyaem1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjaem <- reactive({
    (cpiyaem()$cpi / cpiyaem1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeaem <- reactive({
    input$no2_rtype
  })
  fouraem <- reactive({
    cbind(foura(), rtypeaem(), cpiadjaem())
  })

  # for threshold>0 and linear crf
  xpctxsno2aem1a <- reactive({
    outer(no2thrchg(), beta(input$no2_rtype, input$no2_crf, input$no2_incr, get_session_t("log-linear")), afb)
  })
  xpctxsno2aem2a <- reactive({
    thr2(xpctxsno2aem1a(), wtnum)
  })
  ypctxsno2aem1 <- reactive({
    thr3(xpctxsno2aem2a())
  })


  xpctxsno2aem1b <- reactive({
    outer(no2thrchg(), quantile(baem(), 0.025), afb)
  })
  xpctxsno2aem2b <- reactive({
    thr2(xpctxsno2aem1b(), wtnum)
  })
  ypctxsno2aem2 <- reactive({
    thr3(xpctxsno2aem2b())
  })

  xpctxsno2aem1c <- reactive({
    outer(no2thrchg(), quantile(baem(), 0.975), afb)
  })
  xpctxsno2aem2c <- reactive({
    thr2(xpctxsno2aem1c(), wtnum)
  })
  ypctxsno2aem3 <- reactive({
    thr3(xpctxsno2aem2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsno2aem1d <- reactive({
    outer(no2thrchg(), beta(input$no2_rtype, input$no2_crf, input$no2_incr, get_session_t("log-linear")), afa)
  })
  xpctxsno2aem2d <- reactive({
    thr2(xpctxsno2aem1d(), wtnum)
  })
  zpctxsno2aem1 <- reactive({
    thr3(xpctxsno2aem2d())
  })

  xpctxsno2aem1e <- reactive({
    outer(no2thrchg(), quantile(baem(), 0.025), afa)
  })
  xpctxsno2aem2e <- reactive({
    thr2(xpctxsno2aem1e(), wtnum)
  })
  zpctxsno2aem2 <- reactive({
    thr3(xpctxsno2aem2e())
  })

  xpctxsno2aem1f <- reactive({
    outer(no2thrchg(), quantile(baem(), 0.975), afa)
  })
  xpctxsno2aem2f <- reactive({
    thr2(xpctxsno2aem1f(), wtnum)
  })
  zpctxsno2aem3 <- reactive({
    thr3(xpctxsno2aem2f())
  })

  xxpctxsno2aem <- reactive({
    if (input$no2thr == 0 & input$no2_rtype == get_session_t("linear")) {
      xpctxsno2aem1 <- outer(no2regchg(), beta(input$no2_rtype, input$no2_crf, input$no2_incr, get_session_t("log-linear")), afb)
      xpctxsno2aem2 <- outer(no2regchg(), quantile(baem(), 0.025), afb)
      xpctxsno2aem3 <- outer(no2regchg(), quantile(baem(), 0.975), afb)
    } else if (input$no2thr == 0 & input$no2_rtype == get_session_t("log-linear")) {
      xpctxsno2aem1 <- outer(no2regchg(), beta(input$no2_rtype, input$no2_crf, input$no2_incr, get_session_t("log-linear")), afa)
      xpctxsno2aem2 <- outer(no2regchg(), quantile(baem(), 0.025), afa)
      xpctxsno2aem3 <- outer(no2regchg(), quantile(baem(), 0.975), afa)
    } else if (input$no2thr > 0 & input$no2_rtype == get_session_t("linear")) {
      xpctxsno2aem1 <- ypctxsno2aem1()
      xpctxsno2aem2 <- ypctxsno2aem2()
      xpctxsno2aem3 <- ypctxsno2aem3()
    } else if (input$no2thr > 0 & input$no2_rtype == get_session_t("log-linear")) {
      xpctxsno2aem1 <- zpctxsno2aem1()
      xpctxsno2aem2 <- zpctxsno2aem2()
      xpctxsno2aem3 <- zpctxsno2aem3()
    }
    return(list(no2aemaa = xpctxsno2aem1, no2aembb = xpctxsno2aem2, no2aemcc = xpctxsno2aem3))
  })


  fiveaem <- reactive({
    (fouraem_allages() / 1000000) * fouraem_Mort_acute() * xxpctxsno2aem()$no2aemaa
  })
  sixaem <- reactive({
    (fouraem_allages() / 1000000) * fouraem_Mort_acute() * xxpctxsno2aem()$no2aembb
  })
  sevenaem <- reactive({
    (fouraem_allages() / 1000000) * fouraem_Mort_acute() * xxpctxsno2aem()$no2aemcc
  })


  eightaem <- reactive({
    fiveaem() * mean(vm()) * fouraem()$cpiadjaem * 1000000
  })
  # vaeml <- reactive({
  #   (quantile((af(fouraem()$rtypeaem, baem(), 1) * vaem()), 0.025)) / af(fouraem()$rtypeaem, quantile(baem(), 0.025), 1)
  # })
  # xvaeml <- reactive({vaeml()*fouraem()$cpiadj*1000000})
  #
  # vaemu <- reactive({
  #   (quantile((af(fouraem()$rtypeaem, baem(), 1) * vaem()), 0.975)) / af(fouraem()$rtypeaem, quantile(baem(), 0.975), 1)
  # })

  vaeml <- reactive({
    quantile(vm(), 0.025)
  })

  vaemu <- reactive({
    quantile(vm(), 0.975)
  })

  nineaem <- reactive({
    sixaem() * vaeml() * fouraem()$cpiadjaem * 1000000
  })
  tenaem <- reactive({
    sevenaem() * vaemu() * fouraem()$cpiadjaem * 1000000
  })
  tenaaem <- reactive({
    100000 * fiveaem() / fouraem_allages()
  })

  pctxsno2aem <- reactive({
    xxpctxsno2aem()$no2aemaa
  })
  pctxsno2aeml95 <- reactive({
    xxpctxsno2aem()$no2aembb
  })
  pctxsno2aemu95 <- reactive({
    xxpctxsno2aem()$no2aemcc
  })


  # O3 Acute Exposure Mortality
  set.seed(100)
  bo3aem <- reactive({
    rnorm(input$itn, beta(input$o3_rtype1, input$o3_crf1, input$o3_incr1, get_session_t("log-linear")), se(input$o3_rtype1, input$o3_u95_1, input$o3_l95_1, input$o3_incr1, get_session_t("log-linear")))
  })

  set.seed(100)
  vo3aem <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiyo3aem <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  })
  cpiyo3aem1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjo3aem <- reactive({
    (cpiyo3aem()$cpi / cpiyo3aem1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeo3aem <- reactive({
    input$o3_rtype1
  })
  fouro3aem <- reactive({
    cbind(foura(), rtypeo3aem(), cpiadjo3aem())
  })

  # for threshold>0 and linear crf
  xpctxso3aem1a <- reactive({
    outer(o3thrchg(), beta(input$o3_rtype1, input$o3_crf1, input$o3_incr1, get_session_t("log-linear")), afb)
  })
  xpctxso3aem2a <- reactive({
    thr2(xpctxso3aem1a(), wtnum)
  })
  ypctxso3aem1 <- reactive({
    thr3(xpctxso3aem2a())
  })


  xpctxso3aem1b <- reactive({
    outer(o3thrchg(), quantile(bo3aem(), 0.025), afb)
  })
  xpctxso3aem2b <- reactive({
    thr2(xpctxso3aem1b(), wtnum)
  })
  ypctxso3aem2 <- reactive({
    thr3(xpctxso3aem2b())
  })

  xpctxso3aem1c <- reactive({
    outer(o3thrchg(), quantile(bo3aem(), 0.975), afb)
  })
  xpctxso3aem2c <- reactive({
    thr2(xpctxso3aem1c(), wtnum)
  })
  ypctxso3aem3 <- reactive({
    thr3(xpctxso3aem2c())
  })

  # for threshold>0 and log-linear crf
  xpctxso3aem1d <- reactive({
    outer(o3thrchg(), beta(input$o3_rtype1, input$o3_crf1, input$o3_incr1, get_session_t("log-linear")), afa)
  })
  xpctxso3aem2d <- reactive({
    thr2(xpctxso3aem1d(), wtnum)
  })
  zpctxso3aem1 <- reactive({
    thr3(xpctxso3aem2d())
  })

  xpctxso3aem1e <- reactive({
    outer(o3thrchg(), quantile(bo3aem(), 0.025), afa)
  })
  xpctxso3aem2e <- reactive({
    thr2(xpctxso3aem1e(), wtnum)
  })
  zpctxso3aem2 <- reactive({
    thr3(xpctxso3aem2e())
  })

  xpctxso3aem1f <- reactive({
    outer(o3thrchg(), quantile(bo3aem(), 0.975), afa)
  })
  xpctxso3aem2f <- reactive({
    thr2(xpctxso3aem1f(), wtnum)
  })
  zpctxso3aem3 <- reactive({
    thr3(xpctxso3aem2f())
  })

  xxpctxso3aem <- reactive({
    if (input$o3thr == 0 & input$o3_rtype1 == get_session_t("linear")) {
      xpctxso3aem1 <- outer(o3regchg(), beta(input$o3_rtype1, input$o3_crf1, input$o3_incr1, get_session_t("log-linear")), afb)
      xpctxso3aem2 <- outer(o3regchg(), quantile(bo3aem(), 0.025), afb)
      xpctxso3aem3 <- outer(o3regchg(), quantile(bo3aem(), 0.975), afb)
    } else if (input$o3thr == 0 & input$o3_rtype1 == get_session_t("log-linear")) {
      xpctxso3aem1 <- outer(o3regchg(), beta(input$o3_rtype1, input$o3_crf1, input$o3_incr1, get_session_t("log-linear")), afa)
      xpctxso3aem2 <- outer(o3regchg(), quantile(bo3aem(), 0.025), afa)
      xpctxso3aem3 <- outer(o3regchg(), quantile(bo3aem(), 0.975), afa)
    } else if (input$o3thr > 0 & input$o3_rtype1 == get_session_t("linear")) {
      xpctxso3aem1 <- ypctxso3aem1()
      xpctxso3aem2 <- ypctxso3aem2()
      xpctxso3aem3 <- ypctxso3aem3()
    } else if (input$o3thr > 0 & input$o3_rtype1 == get_session_t("log-linear")) {
      xpctxso3aem1 <- zpctxso3aem1()
      xpctxso3aem2 <- zpctxso3aem2()
      xpctxso3aem3 <- zpctxso3aem3()
    }
    return(list(o3aemaa = xpctxso3aem1, o3aembb = xpctxso3aem2, o3aemcc = xpctxso3aem3))
  })


  fiveo3aem <- reactive({
    (fouro3aem_allages() / 1000000) * fouro3aem_Mort_acute() * xxpctxso3aem()$o3aemaa
  })
  sixo3aem <- reactive({
    (fouro3aem_allages() / 1000000) * fouro3aem_Mort_acute() * xxpctxso3aem()$o3aembb
  })
  seveno3aem <- reactive({
    (fouro3aem_allages() / 1000000) * fouro3aem_Mort_acute() * xxpctxso3aem()$o3aemcc
  })

  eighto3aem <- reactive({
    fiveo3aem() * mean(vm()) * fouro3aem()$cpiadjo3aem * 1000000
  })
  # vo3aeml <- reactive({
  #   (quantile((af(fouro3aem()$rtypeo3aem, bo3aem(), 1) * vo3aem()), 0.025)) / af(fouro3aem()$rtypeo3aem, quantile(bo3aem(), 0.025), 1)
  # })
  #
  # vo3aemu <- reactive({
  #   (quantile((af(fouro3aem()$rtypeo3aem, bo3aem(), 1) * vo3aem()), 0.975)) / af(fouro3aem()$rtypeo3aem, quantile(bo3aem(), 0.975), 1)
  # })

  vo3aeml <- reactive({
    quantile(vm(), 0.025)
  })
  vo3aemu <- reactive({
    quantile(vm(), 0.975)
  })

  nineo3aem <- reactive({
    sixo3aem() * vo3aeml() * fouro3aem()$cpiadjo3aem * 1000000
  })
  teno3aem <- reactive({
    seveno3aem() * vo3aemu() * fouro3aem()$cpiadjo3aem * 1000000
  })
  tenao3aem <- reactive({
    100000 * fiveo3aem() / fouro3aem_allages()
  })


  pctxso3aem <- reactive({
    xxpctxso3aem()$o3aemaa
  })
  pctxso3aeml95 <- reactive({
    xxpctxso3aem()$o3aembb
  })
  pctxso3aemu95 <- reactive({
    xxpctxso3aem()$o3aemcc
  })

  # Summer O3 Chronic Exposure Respiratory Mortality
  set.seed(100)
  bo3cerm <- reactive({
    rnorm(input$itn, beta(input$o3_rtype2, input$o3_crf2, input$o3_incr2, get_session_t("log-linear")), se(input$o3_rtype2, input$o3_u95_2, input$o3_l95_2, input$o3_incr2, get_session_t("log-linear")))
  })

  set.seed(100)
  vo3cerm <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiyo3cerm <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  })
  cpiyo3cerm1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjo3cerm <- reactive({
    (cpiyo3cerm()$cpi / cpiyo3cerm1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeo3cerm <- reactive({
    input$o3_rtype2
  })
  fouro3cerm <- reactive({
    cbind(foura(), rtypeo3cerm(), cpiadjo3cerm())
  })

  fiveo3cerm <- reactive({
    (fouro3cerm_age30plus() / 1000000) * fouro3cerm_Mort_respiratory() * af(fouro3cerm()$rtypeo3cerm, beta(fouro3cerm()$rtypeo3cerm, input$o3_crf2, input$o3_incr2, get_session_t("log-linear")), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })

  sixo3cerm <- reactive({
    (fouro3cerm_age30plus() / 1000000) * fouro3cerm_Mort_respiratory() * af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.025), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })
  seveno3cerm <- reactive({
    (fouro3cerm_age30plus() / 1000000) * fouro3cerm_Mort_respiratory() * af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.975), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })

  eighto3cerm <- reactive({
    fiveo3cerm() * mean(vm()) * fouro3cerm()$cpiadjo3cerm * 1000000
  })

  # vo3cerml <- reactive({
  #   (quantile((af(fouro3cerm()$rtypeo3cerm, bo3cerm(), 1) * vo3cerm()), 0.025)) / af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.025), 1)
  # })
  #
  # vo3cermu <- reactive({
  #   (quantile((af(fouro3cerm()$rtypeo3cerm, bo3cerm(), 1) * vo3cerm()), 0.975)) / af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.975), 1)
  # })

  vo3cerml <- reactive({
    quantile(vm(), 0.025)
  })
  vo3cermu <- reactive({
    quantile(vm(), 0.975)
  })

  nineo3cerm <- reactive({
    sixo3cerm() * vo3cerml() * fouro3cerm()$cpiadjo3cerm * 1000000
  })
  teno3cerm <- reactive({
    seveno3cerm() * vo3cermu() * fouro3cerm()$cpiadjo3cerm * 1000000
  })
  tenao3cerm <- reactive({
    100000 * fiveo3cerm() / fouro3cerm_allages()
  })
  pctxso3cerm <- reactive({
    af(fouro3cerm()$rtypeo3cerm, beta(fouro3cerm()$rtypeo3cerm, input$o3_crf2, input$o3_incr2, get_session_t("log-linear")), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })
  pctxso3cerml95 <- reactive({
    af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.025), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })
  pctxso3cermu95 <- reactive({
    af(fouro3cerm()$rtypeo3cerm, quantile(bo3cerm(), 0.975), chg(fouro3cerm_summero3_2(), fouro3cerm_summero3_1(), input$summero3thr), get_session_t("log-linear"))
  })

  leyrrep <- reactive({
    lifeyr2(pctxso3cerm())
  })
  leyrrepl95 <- reactive({
    lifeyr2(pctxso3cerml95())
  })
  leyrrepu95 <- reactive({
    lifeyr2(pctxso3cermu95())
  })

  # Summer O3 Minor Restricted Activity Days
  set.seed(100)
  bmrad <- reactive({
    chg2(rnorm(input$itn, beta(input$o3_rtype5, input$o3_crf5, input$o3_incr5, get_session_t("log-linear")), se(input$o3_rtype5, input$o3_u95_5, input$o3_l95_5, input$o3_incr5, get_session_t("log-linear"))))
  })

  set.seed(100)
  vmrad <- reactive({
    chg2(valdist(input$sourcevsl8c, input$itn, input$vsl8, input$lvsl8, input$uvsl8, input$pcvsl8, input$plvsl8, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1mrad <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl8b), ]
  })

  cpiadjmrad <- reactive({
    (cpiy()$cpi / cpiy1mrad()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypemrad <- reactive({
    input$o3_rtype5
  })
  fourmrad <- reactive({
    cbind(foura(), rtypemrad(), cpiadjmrad())
  })

  # for threshold>0 and linear crf
  xpctxsmrad1a <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype5, input$o3_crf5, input$o3_incr5, get_session_t("log-linear")), afb)
  })
  xpctxsmrad2a <- reactive({
    thr2(xpctxsmrad1a(), wtnum)
  })
  ypctxsmrad1 <- reactive({
    thr3(xpctxsmrad2a())
  })


  xpctxsmrad1b <- reactive({
    outer(summo3thrchg(), quantile(bmrad(), 0.025), afb)
  })
  xpctxsmrad2b <- reactive({
    thr2(xpctxsmrad1b(), wtnum)
  })
  ypctxsmrad2 <- reactive({
    thr3(xpctxsmrad2b())
  })

  xpctxsmrad1c <- reactive({
    outer(summo3thrchg(), quantile(bmrad(), 0.975), afb)
  })
  xpctxsmrad2c <- reactive({
    thr2(xpctxsmrad1c(), wtnum)
  })
  ypctxsmrad3 <- reactive({
    thr3(xpctxsmrad2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsmrad1d <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype5, input$o3_crf5, input$o3_incr5, get_session_t("log-linear")), afa)
  })
  xpctxsmrad2d <- reactive({
    thr2(xpctxsmrad1d(), wtnum)
  })
  zpctxsmrad1 <- reactive({
    thr3(xpctxsmrad2d())
  })

  xpctxsmrad1e <- reactive({
    outer(summo3thrchg(), quantile(bmrad(), 0.025), afa)
  })
  xpctxsmrad2e <- reactive({
    thr2(xpctxsmrad1e(), wtnum)
  })
  zpctxsmrad2 <- reactive({
    thr3(xpctxsmrad2e())
  })

  xpctxsmrad1f <- reactive({
    outer(summo3thrchg(), quantile(bmrad(), 0.975), afa)
  })
  xpctxsmrad2f <- reactive({
    thr2(xpctxsmrad1f(), wtnum)
  })
  zpctxsmrad3 <- reactive({
    thr3(xpctxsmrad2f())
  })

  xxpctxsmrad <- reactive({
    if (input$summero3thr == 0 & input$o3_rtype5 == get_session_t("linear")) {
      xpctxsmrad1 <- outer(summo3regchg(), beta(input$o3_rtype5, input$o3_crf5, input$o3_incr5, get_session_t("log-linear")), afb)
      xpctxsmrad2 <- outer(summo3regchg(), quantile(bmrad(), 0.025), afb)
      xpctxsmrad3 <- outer(summo3regchg(), quantile(bmrad(), 0.975), afb)
    } else if (input$summero3thr == 0 & input$o3_rtype5 == get_session_t("log-linear")) {
      xpctxsmrad1 <- outer(summo3regchg(), beta(input$o3_rtype5, input$o3_crf5, input$o3_incr5, get_session_t("log-linear")), afa)
      xpctxsmrad2 <- outer(summo3regchg(), quantile(bmrad(), 0.025), afa)
      xpctxsmrad3 <- outer(summo3regchg(), quantile(bmrad(), 0.975), afa)
    } else if (input$summero3thr > 0 & input$o3_rtype5 == get_session_t("linear")) {
      xpctxsmrad1 <- ypctxsmrad1()
      xpctxsmrad2 <- ypctxsmrad2()
      xpctxsmrad3 <- ypctxsmrad3()
    } else if (input$summero3thr > 0 & input$o3_rtype5 == get_session_t("log-linear")) {
      xpctxsmrad1 <- zpctxsmrad1()
      xpctxsmrad2 <- zpctxsmrad2()
      xpctxsmrad3 <- zpctxsmrad3()
    }
    return(list(mradaa = xpctxsmrad1, mradbb = xpctxsmrad2, mradcc = xpctxsmrad3))
  })


  fivemrad <- reactive({
    (popnonasthma(input$asprev, fourmrad_age5_19(), fourmrad_age20plus()) / 1000000) * 0.4192 * fourmrad_Minor_Restricted_Activity() * xxpctxsmrad()$mradaa
  })
  sixmrad <- reactive({
    (popnonasthma(input$asprev, fourmrad_age5_19(), fourmrad_age20plus()) / 1000000) * 0.4192 * fourmrad_Minor_Restricted_Activity() * xxpctxsmrad()$mradbb
  })
  sevenmrad <- reactive({
    (popnonasthma(input$asprev, fourmrad_age5_19(), fourmrad_age20plus()) / 1000000) * 0.4192 * fourmrad_Minor_Restricted_Activity() * xxpctxsmrad()$mradcc
  })

  eightmrad <- reactive({
    fivemrad() * input$vsl8 * fourmrad()$cpiadjmrad
  })

  # vmradl <- reactive({
  #   chg3(af(fourmrad()$rtypemrad, quantile(bmrad(), 0.025), 1), quantile((af(fourmrad()$rtypemrad, bmrad(), 1) * vmrad()), 0.025))
  # })
  #
  #
  # vmradu <- reactive({
  #   (quantile((af(fourmrad()$rtypemrad, bmrad(), 1) * vmrad()), 0.975)) / af(fourmrad()$rtypemrad, quantile(bmrad(), 0.975), 1)
  # })
  vmradl <- reactive({
    quantile(vmrad(), 0.025)
  })
  vmradu <- reactive({
    quantile(vmrad(), 0.975)
  })

  ninemrad <- reactive({
    sixmrad() * vmradl() * fourmrad()$cpiadjmrad
  })
  tenmrad <- reactive({
    sevenmrad() * vmradu() * fourmrad()$cpiadjmrad
  })
  tenamrad <- reactive({
    100000 * fivemrad() / popnonasthma(input$asprev, fourmrad_age5_19(), fourmrad_age20plus())
  })

  pctxsmrad <- reactive({
    0.4192 * xxpctxsmrad()$mradaa
  })
  pctxsmradl95 <- reactive({
    0.4192 * xxpctxsmrad()$mradbb
  })
  pctxsmradu95 <- reactive({
    0.4192 * xxpctxsmrad()$mradcc
  })

  # O3 Summer o3arsd - Acute Respiratory Symptom Days
  set.seed(100)
  bo3arsd <- reactive({
    chg2(rnorm(input$itn, beta(input$o3_rtype3, input$o3_crf3, input$o3_incr3, get_session_t("log-linear")), se(input$o3_rtype3, input$o3_u95_3, input$o3_l95_3, input$o3_incr3, get_session_t("log-linear"))))
  })

  set.seed(100)
  vo3arsd <- reactive({
    chg2(valdist(input$vsl2fm, input$itn, input$vsl2, input$lvsl2, input$uvsl2, input$pcvsl2, input$plvsl2, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1o3arsd <- reactive({
    get_session_cpi()[which(cpi_year() == input$vsl2yr), ]
  })

  cpiadjo3arsd <- reactive({
    (cpiy()$cpi / cpiy1o3arsd()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeo3arsd <- reactive({
    input$o3_rtype3
  })
  fouro3arsd <- reactive({
    cbind(foura(), rtypeo3arsd(), cpiadjo3arsd())
  })


  # for threshold>0 and linear crf
  xpctxso3arsd1a <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype3, input$o3_crf3, input$o3_incr3, get_session_t("log-linear")), afb)
  })
  xpctxso3arsd2a <- reactive({
    thr2(xpctxso3arsd1a(), wtnum)
  })
  ypctxso3arsd1 <- reactive({
    thr3(xpctxso3arsd2a())
  })


  xpctxso3arsd1b <- reactive({
    outer(summo3thrchg(), quantile(bo3arsd(), 0.025), afb)
  })
  xpctxso3arsd2b <- reactive({
    thr2(xpctxso3arsd1b(), wtnum)
  })
  ypctxso3arsd2 <- reactive({
    thr3(xpctxso3arsd2b())
  })

  xpctxso3arsd1c <- reactive({
    outer(summo3thrchg(), quantile(bo3arsd(), 0.975), afb)
  })
  xpctxso3arsd2c <- reactive({
    thr2(xpctxso3arsd1c(), wtnum)
  })
  ypctxso3arsd3 <- reactive({
    thr3(xpctxso3arsd2c())
  })

  # for threshold>0 and log-linear crf
  xpctxso3arsd1d <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype3, input$o3_crf3, input$o3_incr3, get_session_t("log-linear")), afa)
  })
  xpctxso3arsd2d <- reactive({
    thr2(xpctxso3arsd1d(), wtnum)
  })
  zpctxso3arsd1 <- reactive({
    thr3(xpctxso3arsd2d())
  })

  xpctxso3arsd1e <- reactive({
    outer(summo3thrchg(), quantile(bo3arsd(), 0.025), afa)
  })
  xpctxso3arsd2e <- reactive({
    thr2(xpctxso3arsd1e(), wtnum)
  })
  zpctxso3arsd2 <- reactive({
    thr3(xpctxso3arsd2e())
  })

  xpctxso3arsd1f <- reactive({
    outer(summo3thrchg(), quantile(bo3arsd(), 0.975), afa)
  })
  xpctxso3arsd2f <- reactive({
    thr2(xpctxso3arsd1f(), wtnum)
  })
  zpctxso3arsd3 <- reactive({
    thr3(xpctxso3arsd2f())
  })

  xxpctxso3arsd <- reactive({
    if (input$summero3thr == 0 & input$o3_rtype3 == get_session_t("linear")) {
      xpctxso3arsd1 <- outer(summo3regchg(), beta(input$o3_rtype3, input$o3_crf3, input$o3_incr3, get_session_t("log-linear")), afb)
      xpctxso3arsd2 <- outer(summo3regchg(), quantile(bo3arsd(), 0.025), afb)
      xpctxso3arsd3 <- outer(summo3regchg(), quantile(bo3arsd(), 0.975), afb)
    } else if (input$summero3thr == 0 & input$o3_rtype3 == get_session_t("log-linear")) {
      xpctxso3arsd1 <- outer(summo3regchg(), beta(input$o3_rtype3, input$o3_crf3, input$o3_incr3, get_session_t("log-linear")), afa)
      xpctxso3arsd2 <- outer(summo3regchg(), quantile(bo3arsd(), 0.025), afa)
      xpctxso3arsd3 <- outer(summo3regchg(), quantile(bo3arsd(), 0.975), afa)
    } else if (input$summero3thr > 0 & input$o3_rtype3 == get_session_t("linear")) {
      xpctxso3arsd1 <- ypctxso3arsd1()
      xpctxso3arsd2 <- ypctxso3arsd2()
      xpctxso3arsd3 <- ypctxso3arsd3()
    } else if (input$summero3thr > 0 & input$o3_rtype3 == get_session_t("log-linear")) {
      xpctxso3arsd1 <- zpctxso3arsd1()
      xpctxso3arsd2 <- zpctxso3arsd2()
      xpctxso3arsd3 <- zpctxso3arsd3()
    }
    return(list(o3arsdaa = xpctxso3arsd1, o3arsdbb = xpctxso3arsd2, o3arsdcc = xpctxso3arsd3))
  })


  fiveo3arsd <- reactive({
    (popnonasthma(input$asprev, fouro3arsd_age5_19(), fouro3arsd_age20plus()) / 1000000) * 0.4192 * fouro3arsd_Acute_Resp_Symptom_Days() * xxpctxso3arsd()$o3arsdaa
  })
  xfiveo3arsd <- reactive({
    fiveo3arsd() - fivemrad()
  })

  sixo3arsd <- reactive({
    (popnonasthma(input$asprev, fouro3arsd_age5_19(), fouro3arsd_age20plus()) / 1000000) * 0.4192 * fouro3arsd_Acute_Resp_Symptom_Days() * xxpctxso3arsd()$o3arsdbb
  })
  xsixo3arsd <- reactive({
    sixo3arsd() - sixmrad()
  })


  seveno3arsd <- reactive({
    (popnonasthma(input$asprev, fouro3arsd_age5_19(), fouro3arsd_age20plus()) / 1000000) * 0.4192 * fouro3arsd_Acute_Resp_Symptom_Days() * xxpctxso3arsd()$o3arsdcc
  })
  xseveno3arsd <- reactive({
    seveno3arsd() - sevenmrad()
  })

  eighto3arsd <- reactive({
    xfiveo3arsd() * input$vsl2 * fouro3arsd()$cpiadjo3arsd
  })

  # vo3arsdl <- reactive({
  #   chg3(af(fouro3arsd()$rtypeo3arsd, quantile(bo3arsd(), 0.025), 1), quantile((af(fouro3arsd()$rtypeo3arsd, bo3arsd(), 1) * vo3arsd()), 0.025))
  # })
  # xvo3arsdl <- reactive({
  #   vo3arsdl() * fouro3arsd()$cpiadjo3arsd
  # })
  #
  # vo3arsdu <- reactive({
  #   (quantile((af(fouro3arsd()$rtypeo3arsd, bo3arsd(), 1) * vo3arsd()), 0.975)) / af(fouro3arsd()$rtypeo3arsd, quantile(bo3arsd(), 0.975), 1)
  # })
  # xvo3arsdu <- reactive({
  #   vo3arsdu() * fouro3arsd()$cpiadjo3arsd
  # })

  vo3arsdl <- reactive({
    quantile(vo3arsd(), 0.025)
  })
  vo3arsdu <- reactive({
    quantile(vo3arsd(), 0.975)
  })
  xvo3arsdu <- reactive({
    vo3arsdu() * fouro3arsd()$cpiadjo3arsd
  })

  nineo3arsd <- reactive({
    xsixo3arsd() * vo3arsdl() * fouro3arsd()$cpiadjo3arsd
  })

  teno3arsd <- reactive({
    xseveno3arsd() * xvo3arsdu()
  })
  tenao3arsd <- reactive({
    100000 * fiveo3arsd() / popnonasthma(input$asprev, fouro3arsd_age5_19(), fouro3arsd_age20plus())
  })

  pctxso3arsd <- reactive({
    0.4192 * xxpctxso3arsd()$o3arsdaa
  })
  pctxso3arsdl95 <- reactive({
    0.4192 * xxpctxso3arsd()$o3arsdbb
  })
  pctxso3arsdu95 <- reactive({
    0.4192 * xxpctxso3arsd()$o3arsdcc
  })


  # O3 Summer - Asthma Symptom Days
  set.seed(100)
  bo3asd <- reactive({
    chg2(rnorm(input$itn, beta(input$o3_rtype4, input$o3_crf4, input$o3_incr4, get_session_t("log-linear")), se(input$o3_rtype4, input$o3_u95_4, input$o3_l95_4, input$o3_incr4, get_session_t("log-linear"))))
  })

  set.seed(100)
  vo3asd <- reactive({
    valdist(input$sourcevsl4c, input$itn, as.double(input$vsl4), as.double(input$lvsl4), as.double(input$uvsl4), input$pcvsl4, input$plvsl4, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })

  cpiy1o3asd <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl4b), ]
  })

  cpiadjo3asd <- reactive({
    (cpiy()$cpi / cpiy1o3asd()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })
  rtypeo3asd <- reactive({
    input$o3_rtype4
  })

  fouro3asd <- reactive({
    cbind(foura(), rtypeo3asd(), cpiadjo3asd())
  })

  # for threshold>0 and linear crf
  xpctxso3asd1a <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype4, input$o3_crf4, input$o3_incr4, get_session_t("log-linear")), afb)
  })
  xpctxso3asd2a <- reactive({
    thr2(xpctxso3asd1a(), wtnum)
  })
  ypctxso3asd1 <- reactive({
    thr3(xpctxso3asd2a())
  })

  xpctxso3asd1b <- reactive({
    outer(summo3thrchg(), quantile(bo3asd(), 0.025), afb)
  })
  xpctxso3asd2b <- reactive({
    thr2(xpctxso3asd1b(), wtnum)
  })
  ypctxso3asd2 <- reactive({
    thr3(xpctxso3asd2b())
  })

  xpctxso3asd1c <- reactive({
    outer(summo3thrchg(), quantile(bo3asd(), 0.975), afb)
  })
  xpctxso3asd2c <- reactive({
    thr2(xpctxso3asd1c(), wtnum)
  })
  ypctxso3asd3 <- reactive({
    thr3(xpctxso3asd2c())
  })

  # for threshold>0 and log-linear crf
  xpctxso3asd1d <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype4, input$o3_crf4, input$o3_incr4, get_session_t("log-linear")), afa)
  })
  xpctxso3asd2d <- reactive({
    thr2(xpctxso3asd1d(), wtnum)
  })
  zpctxso3asd1 <- reactive({
    thr3(xpctxso3asd2d())
  })

  xpctxso3asd1e <- reactive({
    outer(summo3thrchg(), quantile(bo3asd(), 0.025), afa)
  })
  xpctxso3asd2e <- reactive({
    thr2(xpctxso3asd1e(), wtnum)
  })
  zpctxso3asd2 <- reactive({
    thr3(xpctxso3asd2e())
  })

  xpctxso3asd1f <- reactive({
    outer(summo3thrchg(), quantile(bo3asd(), 0.975), afa)
  })
  xpctxso3asd2f <- reactive({
    thr2(xpctxso3asd1f(), wtnum)
  })
  zpctxso3asd3 <- reactive({
    thr3(xpctxso3asd2f())
  })

  xxpctxso3asd <- reactive({
    if (input$summero3thr == 0 & input$o3_rtype4 == get_session_t("linear")) {
      xpctxso3asd1 <- outer(summo3regchg(), beta(input$o3_rtype4, input$o3_crf4, input$o3_incr4, get_session_t("log-linear")), afb)
      xpctxso3asd2 <- outer(summo3regchg(), quantile(bo3asd(), 0.025), afb)
      xpctxso3asd3 <- outer(summo3regchg(), quantile(bo3asd(), 0.975), afb)
    } else if (input$summero3thr == 0 & input$o3_rtype4 == get_session_t("log-linear")) {
      xpctxso3asd1 <- outer(summo3regchg(), beta(input$o3_rtype4, input$o3_crf4, input$o3_incr4, get_session_t("log-linear")), afa)
      xpctxso3asd2 <- outer(summo3regchg(), quantile(bo3asd(), 0.025), afa)
      xpctxso3asd3 <- outer(summo3regchg(), quantile(bo3asd(), 0.975), afa)
    } else if (input$summero3thr > 0 & input$o3_rtype4 == get_session_t("linear")) {
      xpctxso3asd1 <- ypctxso3asd1()
      xpctxso3asd2 <- ypctxso3asd2()
      xpctxso3asd3 <- ypctxso3asd3()
    } else if (input$summero3thr > 0 & input$o3_rtype4 == get_session_t("log-linear")) {
      xpctxso3asd1 <- zpctxso3asd1()
      xpctxso3asd2 <- zpctxso3asd2()
      xpctxso3asd3 <- zpctxso3asd3()
    }
    return(list(o3asdaa = xpctxso3asd1, o3asdbb = xpctxso3asd2, o3asdcc = xpctxso3asd3))
  })


  fiveo3asd <- reactive({
    (fouro3asd_age5_19() / 1000000) * 0.4192 * (input$asprev / 100) * fouro3asd_Asthma_Symptom_Days() * xxpctxso3asd()$o3asdaa
  })
  sixo3asd <- reactive({
    (fouro3asd_age5_19() / 1000000) * 0.4192 * (input$asprev / 100) * fouro3asd_Asthma_Symptom_Days() * xxpctxso3asd()$o3asdbb
  })
  seveno3asd <- reactive({
    (fouro3asd_age5_19() / 1000000) * 0.4192 * (input$asprev / 100) * fouro3asd_Asthma_Symptom_Days() * xxpctxso3asd()$o3asdcc
  })

  eighto3asd <- reactive({
    fiveo3asd() * mean(vo3asd()) * fouro3asd()$cpiadjo3asd
  })
  nineo3asd <- reactive({
    sixo3asd() * quantile(vo3asd(), 0.025) * fouro3asd()$cpiadjo3asd
  })
  teno3asd <- reactive({
    seveno3asd() * quantile(vo3asd(), 0.975) * fouro3asd()$cpiadjo3asd
  })

  tenao3asd <- reactive({
    100000 * fiveo3asd() / (foura_age5_19() * (input$asprev / 100))
  })

  pctxso3asd <- reactive({
    0.4192 * xxpctxso3asd()$o3asdaa
  })
  pctxso3asdl95 <- reactive({
    0.4192 * xxpctxso3asd()$o3asdbb
  })
  pctxso3asdu95 <- reactive({
    0.4192 * xxpctxso3asd()$o3asdcc
  })

  # O3 Summer  Respiratory Emergency Room Visits
  set.seed(100)
  bo3rerv <- reactive({
    chg2(rnorm(input$itn, beta(input$o3_rtype6, input$o3_crf6, input$o3_incr6, get_session_t("log-linear")), se(input$o3_rtype6, input$o3_u95_6, input$o3_l95_6, input$o3_incr6, get_session_t("log-linear"))))
  })
  set.seed(100)
  vo3rerv <- reactive({
    chg2(valdist(input$sourcevsl9c, input$itn, input$vsl9, input$lvsl9, input$uvsl9, input$pcvsl9, input$plvsl9, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })

  cpiy1o3rerv <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl9b), ]
  })

  cpiadjo3rerv <- reactive({
    (cpiy()$cpi / cpiy1o3rerv()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeo3rerv <- reactive({
    input$o3_rtype6
  })
  fouro3rerv <- reactive({
    cbind(foura(), rtypeo3rerv(), cpiadjo3rerv())
  })

  # for threshold>0 and linear crf
  xpctxso3rerv1a <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype6, input$o3_crf6, input$o3_incr6, get_session_t("log-linear")), afb)
  })
  xpctxso3rerv2a <- reactive({
    thr2(xpctxso3rerv1a(), wtnum)
  })
  ypctxso3rerv1 <- reactive({
    thr3(xpctxso3rerv2a())
  })

  xpctxso3rerv1b <- reactive({
    outer(summo3thrchg(), quantile(bo3rerv(), 0.025), afb)
  })
  xpctxso3rerv2b <- reactive({
    thr2(xpctxso3rerv1b(), wtnum)
  })
  ypctxso3rerv2 <- reactive({
    thr3(xpctxso3rerv2b())
  })

  xpctxso3rerv1c <- reactive({
    outer(summo3thrchg(), quantile(bo3rerv(), 0.975), afb)
  })
  xpctxso3rerv2c <- reactive({
    thr2(xpctxso3rerv1c(), wtnum)
  })
  ypctxso3rerv3 <- reactive({
    thr3(xpctxso3rerv2c())
  })

  # for threshold>0 and log-linear crf
  xpctxso3rerv1d <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype6, input$o3_crf6, input$o3_incr6, get_session_t("log-linear")), afa)
  })
  xpctxso3rerv2d <- reactive({
    thr2(xpctxso3rerv1d(), wtnum)
  })
  zpctxso3rerv1 <- reactive({
    thr3(xpctxso3rerv2d())
  })

  xpctxso3rerv1e <- reactive({
    outer(summo3thrchg(), quantile(bo3rerv(), 0.025), afa)
  })
  xpctxso3rerv2e <- reactive({
    thr2(xpctxso3rerv1e(), wtnum)
  })
  zpctxso3rerv2 <- reactive({
    thr3(xpctxso3rerv2e())
  })

  xpctxso3rerv1f <- reactive({
    outer(summo3thrchg(), quantile(bo3rerv(), 0.975), afa)
  })
  xpctxso3rerv2f <- reactive({
    thr2(xpctxso3rerv1f(), wtnum)
  })
  zpctxso3rerv3 <- reactive({
    thr3(xpctxso3rerv2f())
  })

  xxpctxso3rerv <- reactive({
    if (input$summero3thr == 0 & input$o3_rtype6 == get_session_t("linear")) {
      xpctxso3rerv1 <- outer(summo3regchg(), beta(input$o3_rtype6, input$o3_crf6, input$o3_incr6, get_session_t("log-linear")), afb)
      xpctxso3rerv2 <- outer(summo3regchg(), quantile(bo3rerv(), 0.025), afb)
      xpctxso3rerv3 <- outer(summo3regchg(), quantile(bo3rerv(), 0.975), afb)
    } else if (input$summero3thr == 0 & input$o3_rtype6 == get_session_t("log-linear")) {
      xpctxso3rerv1 <- outer(summo3regchg(), beta(input$o3_rtype6, input$o3_crf6, input$o3_incr6, get_session_t("log-linear")), afa)
      xpctxso3rerv2 <- outer(summo3regchg(), quantile(bo3rerv(), 0.025), afa)
      xpctxso3rerv3 <- outer(summo3regchg(), quantile(bo3rerv(), 0.975), afa)
    } else if (input$summero3thr > 0 & input$o3_rtype6 == get_session_t("linear")) {
      xpctxso3rerv1 <- ypctxso3rerv1()
      xpctxso3rerv2 <- ypctxso3rerv2()
      xpctxso3rerv3 <- ypctxso3rerv3()
    } else if (input$summero3thr > 0 & input$o3_rtype6 == get_session_t("log-linear")) {
      xpctxso3rerv1 <- zpctxso3rerv1()
      xpctxso3rerv2 <- zpctxso3rerv2()
      xpctxso3rerv3 <- zpctxso3rerv3()
    }
    return(list(o3rervaa = xpctxso3rerv1, o3rervbb = xpctxso3rerv2, o3rervcc = xpctxso3rerv3))
  })

  fiveo3rerv <- reactive({
    (fouro3rerv_allages() / 1000000) * 0.4192 * fouro3rerv_Respiratory_Emergency_Room() * xxpctxso3rerv()$o3rervaa
  })
  sixo3rerv <- reactive({
    (fouro3rerv_allages() / 1000000) * 0.4192 * fouro3rerv_Respiratory_Emergency_Room() * xxpctxso3rerv()$o3rervbb
  })
  seveno3rerv <- reactive({
    (fouro3rerv_allages() / 1000000) * 0.4192 * fouro3rerv_Respiratory_Emergency_Room() * xxpctxso3rerv()$o3rervcc
  })


  eighto3rerv <- reactive({
    fiveo3rerv() * mean(vo3rerv()) * fouro3rerv()$cpiadjo3rerv
  })

  vo3rervl <- reactive({
    quantile(vo3rerv(), 0.025)
  })
  # vo3rervu <- reactive({
  #   (quantile((af(fouro3rerv()$rtypeo3rerv, bo3rerv(), 1) * vo3rerv()), 0.975)) / af(fouro3rerv()$rtypeo3rerv, quantile(bo3rerv(), 0.975), 1)
  # })

  vo3rervu <- reactive({
    quantile(vo3rerv(), 0.975)
  })

  nineo3rerv <- reactive({
    sixo3rerv() * vo3rervl() * fouro3rerv()$cpiadjo3rerv
  })
  teno3rerv <- reactive({
    seveno3rerv() * vo3rervu() * fouro3rerv()$cpiadjo3rerv
  })
  tenao3rerv <- reactive({
    100000 * fiveo3rerv() / fouro3rerv_allages()
  })

  pctxso3rerv <- reactive({
    0.4192 * xxpctxso3rerv()$o3rervaa
  })
  pctxso3rervl95 <- reactive({
    0.4192 * xxpctxso3rerv()$o3rervbb
  })
  pctxso3rervu95 <- reactive({
    0.4192 * xxpctxso3rerv()$o3rervcc
  })


  # O3 Summer Respiratory Hospital Admissions
  set.seed(100)
  bo3rha <- reactive({
    chg2(rnorm(input$itn, beta(input$o3_rtype7, input$o3_crf7, input$o3_incr7, get_session_t("log-linear")), se(input$o3_rtype7, input$o3_u95_7, input$o3_l95_7, input$o3_incr7, get_session_t("log-linear"))))
  })

  rtypeo3rha <- reactive({
    input$o3_rtype7
  })
  fouro3rha <- reactive({
    cbind(foura(), rtypeo3rha(), 0)
  })

  # for threshold>0 and linear crf
  xpctxso3rha1a <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype7, input$o3_crf7, input$o3_incr7, get_session_t("log-linear")), afb)
  })
  xpctxso3rha2a <- reactive({
    thr2(xpctxso3rha1a(), wtnum)
  })
  ypctxso3rha1 <- reactive({
    thr3(xpctxso3rha2a())
  })

  xpctxso3rha1b <- reactive({
    outer(summo3thrchg(), quantile(bo3rha(), 0.025), afb)
  })
  xpctxso3rha2b <- reactive({
    thr2(xpctxso3rha1b(), wtnum)
  })
  ypctxso3rha2 <- reactive({
    thr3(xpctxso3rha2b())
  })

  xpctxso3rha1c <- reactive({
    outer(summo3thrchg(), quantile(bo3rha(), 0.975), afb)
  })
  xpctxso3rha2c <- reactive({
    thr2(xpctxso3rha1c(), wtnum)
  })
  ypctxso3rha3 <- reactive({
    thr3(xpctxso3rha2c())
  })

  # for threshold>0 and log-linear crf
  xpctxso3rha1d <- reactive({
    outer(summo3thrchg(), beta(input$o3_rtype7, input$o3_crf7, input$o3_incr7, get_session_t("log-linear")), afa)
  })
  xpctxso3rha2d <- reactive({
    thr2(xpctxso3rha1d(), wtnum)
  })
  zpctxso3rha1 <- reactive({
    thr3(xpctxso3rha2d())
  })

  xpctxso3rha1e <- reactive({
    outer(summo3thrchg(), quantile(bo3rha(), 0.025), afa)
  })
  xpctxso3rha2e <- reactive({
    thr2(xpctxso3rha1e(), wtnum)
  })
  zpctxso3rha2 <- reactive({
    thr3(xpctxso3rha2e())
  })

  xpctxso3rha1f <- reactive({
    outer(summo3thrchg(), quantile(bo3rha(), 0.975), afa)
  })
  xpctxso3rha2f <- reactive({
    thr2(xpctxso3rha1f(), wtnum)
  })
  zpctxso3rha3 <- reactive({
    thr3(xpctxso3rha2f())
  })

  xxpctxso3rha <- reactive({
    if (input$summero3thr == 0 & input$o3_rtype7 == get_session_t("linear")) {
      xpctxso3rha1 <- outer(summo3regchg(), beta(input$o3_rtype7, input$o3_crf7, input$o3_incr7, get_session_t("log-linear")), afb)
      xpctxso3rha2 <- outer(summo3regchg(), quantile(bo3rha(), 0.025), afb)
      xpctxso3rha3 <- outer(summo3regchg(), quantile(bo3rha(), 0.975), afb)
    } else if (input$summero3thr == 0 & input$o3_rtype7 == get_session_t("log-linear")) {
      xpctxso3rha1 <- outer(summo3regchg(), beta(input$o3_rtype7, input$o3_crf7, input$o3_incr7, get_session_t("log-linear")), afa)
      xpctxso3rha2 <- outer(summo3regchg(), quantile(bo3rha(), 0.025), afa)
      xpctxso3rha3 <- outer(summo3regchg(), quantile(bo3rha(), 0.975), afa)
    } else if (input$summero3thr > 0 & input$o3_rtype7 == get_session_t("linear")) {
      xpctxso3rha1 <- ypctxso3rha1()
      xpctxso3rha2 <- ypctxso3rha2()
      xpctxso3rha3 <- ypctxso3rha3()
    } else if (input$summero3thr > 0 & input$o3_rtype7 == get_session_t("log-linear")) {
      xpctxso3rha1 <- zpctxso3rha1()
      xpctxso3rha2 <- zpctxso3rha2()
      xpctxso3rha3 <- zpctxso3rha3()
    }
    return(list(o3rhaaa = xpctxso3rha1, o3rhabb = xpctxso3rha2, o3rhacc = xpctxso3rha3))
  })


  fiveo3rha <- reactive({
    (fouro3rha_allages() / 1000000) * 0.4192 * fouro3rha_Respiratory_Hospital() * xxpctxso3rha()$o3rhaaa
  })
  sixo3rha <- reactive({
    (fouro3rha_allages() / 1000000) * 0.4192 * fouro3rha_Respiratory_Hospital() * xxpctxso3rha()$o3rhabb
  })
  seveno3rha <- reactive({
    (fouro3rha_allages() / 1000000) * 0.4192 * fouro3rha_Respiratory_Hospital() * xxpctxso3rha()$o3rhacc
  })


  tenao3rha <- reactive({
    100000 * fiveo3rha() / fouro3rha_allages()
  })


  pctxso3rha <- reactive({
    0.4192 * xxpctxso3rha()$o3rhaaa
  })
  pctxso3rhal95 <- reactive({
    0.4192 * xxpctxso3rha()$o3rhabb
  })
  pctxso3rhau95 <- reactive({
    0.4192 * xxpctxso3rha()$o3rhacc
  })

  # SO2  Acute Exposure Mortality
  set.seed(100)
  bso2aem <- reactive({
    chg2(rnorm(input$itn, beta(input$so2_rtype, input$so2_crf, input$so2_incr, get_session_t("log-linear")), se(input$so2_rtype, input$so2_u95, input$so2_l95, input$so2_incr, get_session_t("log-linear"))))
  })

  set.seed(100)
  vso2aem <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiyso2aem <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  })
  cpiyso2aem1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjso2aem <- reactive({
    (cpiyso2aem()$cpi / cpiyso2aem1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeso2aem <- reactive({
    input$so2_rtype
  })
  fourso2aem <- reactive({
    cbind(foura(), rtypeso2aem(), cpiadjso2aem())
  })

  # for threshold>0 and linear crf
  xpctxsso2aem1a <- reactive({
    outer(so2thrchg(), beta(input$so2_rtype, input$so2_crf, input$so2_incr, get_session_t("log-linear")), afb)
  })
  xpctxsso2aem2a <- reactive({
    thr2(xpctxsso2aem1a(), wtnum)
  })
  ypctxsso2aem1 <- reactive({
    thr3(xpctxsso2aem2a())
  })

  xpctxsso2aem1b <- reactive({
    outer(so2thrchg(), quantile(bso2aem(), 0.025), afb)
  })
  xpctxsso2aem2b <- reactive({
    thr2(xpctxsso2aem1b(), wtnum)
  })
  ypctxsso2aem2 <- reactive({
    thr3(xpctxsso2aem2b())
  })

  xpctxsso2aem1c <- reactive({
    outer(so2thrchg(), quantile(bso2aem(), 0.975), afb)
  })
  xpctxsso2aem2c <- reactive({
    thr2(xpctxsso2aem1c(), wtnum)
  })
  ypctxsso2aem3 <- reactive({
    thr3(xpctxsso2aem2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsso2aem1d <- reactive({
    outer(so2thrchg(), beta(input$so2_rtype, input$so2_crf, input$so2_incr, get_session_t("log-linear")), afa)
  })
  xpctxsso2aem2d <- reactive({
    thr2(xpctxsso2aem1d(), wtnum)
  })
  zpctxsso2aem1 <- reactive({
    thr3(xpctxsso2aem2d())
  })

  xpctxsso2aem1e <- reactive({
    outer(so2thrchg(), quantile(bso2aem(), 0.025), afa)
  })
  xpctxsso2aem2e <- reactive({
    thr2(xpctxsso2aem1e(), wtnum)
  })
  zpctxsso2aem2 <- reactive({
    thr3(xpctxsso2aem2e())
  })

  xpctxsso2aem1f <- reactive({
    outer(so2thrchg(), quantile(bso2aem(), 0.975), afa)
  })
  xpctxsso2aem2f <- reactive({
    thr2(xpctxsso2aem1f(), wtnum)
  })
  zpctxsso2aem3 <- reactive({
    thr3(xpctxsso2aem2f())
  })

  xxpctxsso2aem <- reactive({
    if (input$so2thr == 0 & input$so2_rtype == get_session_t("linear")) {
      xpctxsso2aem1 <- outer(so2regchg(), beta(input$so2_rtype, input$so2_crf, input$so2_incr, get_session_t("log-linear")), afb)
      xpctxsso2aem2 <- outer(so2regchg(), quantile(bso2aem(), 0.025), afb)
      xpctxsso2aem3 <- outer(so2regchg(), quantile(bso2aem(), 0.975), afb)
    } else if (input$so2thr == 0 & input$so2_rtype == get_session_t("log-linear")) {
      xpctxsso2aem1 <- outer(so2regchg(), beta(input$so2_rtype, input$so2_crf, input$so2_incr, get_session_t("log-linear")), afa)
      xpctxsso2aem2 <- outer(so2regchg(), quantile(bso2aem(), 0.025), afa)
      xpctxsso2aem3 <- outer(so2regchg(), quantile(bso2aem(), 0.975), afa)
    } else if (input$so2thr > 0 & input$so2_rtype == get_session_t("linear")) {
      xpctxsso2aem1 <- ypctxsso2aem1()
      xpctxsso2aem2 <- ypctxsso2aem2()
      xpctxsso2aem3 <- ypctxsso2aem3()
    } else if (input$so2thr > 0 & input$so2_rtype == get_session_t("log-linear")) {
      xpctxsso2aem1 <- zpctxsso2aem1()
      xpctxsso2aem2 <- zpctxsso2aem2()
      xpctxsso2aem3 <- zpctxsso2aem3()
    }
    return(list(so2aemaa = xpctxsso2aem1, so2aembb = xpctxsso2aem2, so2aemcc = xpctxsso2aem3))
  })

  fiveso2aem <- reactive({
    (fourso2aem_allages() / 1000000) * fourso2aem_Mort_acute() * xxpctxsso2aem()$so2aemaa
  })
  sixso2aem <- reactive({
    (fourso2aem_allages() / 1000000) * fourso2aem_Mort_acute() * xxpctxsso2aem()$so2aembb
  })
  sevenso2aem <- reactive({
    (fourso2aem_allages() / 1000000) * fourso2aem_Mort_acute() * xxpctxsso2aem()$so2aemcc
  })

  eightso2aem <- reactive({
    fiveso2aem() * mean(vm()) * fourso2aem()$cpiadjso2aem * 1000000
  })
  #
  #   vso2aeml <- reactive({
  #     (quantile((af(fourso2aem()$rtypeso2aem, bso2aem(), 1) * vso2aem()), 0.025)) / af(fourso2aem()$rtypeso2aem, quantile(bso2aem(), 0.025), 1)
  #   })
  #   vso2aemu <- reactive({
  #     (quantile((af(fourso2aem()$rtypeso2aem, bso2aem(), 1) * vso2aem()), 0.975)) / af(fourso2aem()$rtypeso2aem, quantile(bso2aem(), 0.975), 1)
  #   })

  vso2aeml <- reactive({
    quantile(vm(), 0.025)
  })
  vso2aemu <- reactive({
    quantile(vm(), 0.975)
  })

  nineso2aem <- reactive({
    sixso2aem() * vso2aeml() * fourso2aem()$cpiadjso2aem * 1000000
  })
  tenso2aem <- reactive({
    sevenso2aem() * vso2aemu() * fourso2aem()$cpiadjso2aem * 1000000
  })
  tenaso2aem <- reactive({
    100000 * fiveso2aem() / fourso2aem_allages()
  })

  pctxsso2aem <- reactive({
    xxpctxsso2aem()$so2aemaa
  })
  pctxsso2aeml95 <- reactive({
    xxpctxsso2aem()$so2aembb
  })
  pctxsso2aemu95 <- reactive({
    xxpctxsso2aem()$so2aemcc
  })

  # CO (24 hour)  Acute Exposure Mortality
  set.seed(100)
  bco24aem <- reactive({
    chg2(rnorm(input$itn, beta(input$co24_rtype, input$co24_crf, input$co24_incr, get_session_t("log-linear")), se(input$co24_rtype, input$co24_u95, input$co24_l95, input$co24_incr, get_session_t("log-linear"))))
  })
  set.seed(100)

  vco24aem <- reactive({
    valdist(input$vslfm, input$itn, input$vsl, input$lvsl, input$uvsl, input$pcvsl, input$plvsl, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular"))
  })
  cpiyco24aem <- reactive({
    get_session_cpi()[which(cpi_year() == input$curr), ]
  })
  cpiyco24aem1 <- reactive({
    get_session_cpi()[which(cpi_year() == input$vslyr), ]
  })

  cpiadjco24aem <- reactive({
    (cpiyco24aem()$cpi / cpiyco24aem1()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeco24aem <- reactive({
    input$co24_rtype
  })
  fourco24aem <- reactive({
    cbind(foura(), rtypeco24aem(), cpiadjco24aem())
  })

  # for threshold>0 and linear crf
  xpctxsco24aem1a <- reactive({
    outer(co24thrchg(), beta(input$co24_rtype, input$co24_crf, input$co24_incr, get_session_t("log-linear")), afb)
  })
  xpctxsco24aem2a <- reactive({
    thr2(xpctxsco24aem1a(), wtnum)
  })
  ypctxsco24aem1 <- reactive({
    thr3(xpctxsco24aem2a())
  })

  xpctxsco24aem1b <- reactive({
    outer(co24thrchg(), quantile(bco24aem(), 0.025), afb)
  })
  xpctxsco24aem2b <- reactive({
    thr2(xpctxsco24aem1b(), wtnum)
  })
  ypctxsco24aem2 <- reactive({
    thr3(xpctxsco24aem2b())
  })

  xpctxsco24aem1c <- reactive({
    outer(co24thrchg(), quantile(bco24aem(), 0.975), afb)
  })
  xpctxsco24aem2c <- reactive({
    thr2(xpctxsco24aem1c(), wtnum)
  })
  ypctxsco24aem3 <- reactive({
    thr3(xpctxsco24aem2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsco24aem1d <- reactive({
    outer(co24thrchg(), beta(input$co24_rtype, input$co24_crf, input$co24_incr, get_session_t("log-linear")), afa)
  })
  xpctxsco24aem2d <- reactive({
    thr2(xpctxsco24aem1d(), wtnum)
  })
  zpctxsco24aem1 <- reactive({
    thr3(xpctxsco24aem2d())
  })

  xpctxsco24aem1e <- reactive({
    outer(co24thrchg(), quantile(bco24aem(), 0.025), afa)
  })
  xpctxsco24aem2e <- reactive({
    thr2(xpctxsco24aem1e(), wtnum)
  })
  zpctxsco24aem2 <- reactive({
    thr3(xpctxsco24aem2e())
  })

  xpctxsco24aem1f <- reactive({
    outer(co24thrchg(), quantile(bco24aem(), 0.975), afa)
  })
  xpctxsco24aem2f <- reactive({
    thr2(xpctxsco24aem1f(), wtnum)
  })
  zpctxsco24aem3 <- reactive({
    thr3(xpctxsco24aem2f())
  })

  xxpctxsco24aem <- reactive({
    if (input$cothr == 0 & input$co24_rtype == get_session_t("linear")) {
      xpctxsco24aem1 <- outer(co24regchg(), beta(input$co24_rtype, input$co24_crf, input$co24_incr, get_session_t("log-linear")), afb)
      xpctxsco24aem2 <- outer(co24regchg(), quantile(bco24aem(), 0.025), afb)
      xpctxsco24aem3 <- outer(co24regchg(), quantile(bco24aem(), 0.975), afb)
    } else if (input$cothr == 0 & input$co24_rtype == get_session_t("log-linear")) {
      xpctxsco24aem1 <- outer(co24regchg(), beta(input$co24_rtype, input$co24_crf, input$co24_incr, get_session_t("log-linear")), afa)
      xpctxsco24aem2 <- outer(co24regchg(), quantile(bco24aem(), 0.025), afa)
      xpctxsco24aem3 <- outer(co24regchg(), quantile(bco24aem(), 0.975), afa)
    } else if (input$cothr > 0 & input$co24_rtype == get_session_t("linear")) {
      xpctxsco24aem1 <- ypctxsco24aem1()
      xpctxsco24aem2 <- ypctxsco24aem2()
      xpctxsco24aem3 <- ypctxsco24aem3()
    } else if (input$cothr > 0 & input$co24_rtype == get_session_t("log-linear")) {
      xpctxsco24aem1 <- zpctxsco24aem1()
      xpctxsco24aem2 <- zpctxsco24aem2()
      xpctxsco24aem3 <- zpctxsco24aem3()
    }
    return(list(co24aemaa = xpctxsco24aem1, co24aembb = xpctxsco24aem2, co24aemcc = xpctxsco24aem3))
  })

  fiveco24aem <- reactive({
    (fourco24aem_allages() / 1000000) * fourco24aem_Mort_acute() * xxpctxsco24aem()$co24aemaa
  })
  xsixco24aem <- reactive({
    (fourco24aem_allages() / 1000000) * fourco24aem_Mort_acute() * xxpctxsco24aem()$co24aembb
  })
  sixco24aem <- reactive({
    chg2(xsixco24aem())
  })

  sevenco24aem <- reactive({
    (fourco24aem_allages() / 1000000) * fourco24aem_Mort_acute() * xxpctxsco24aem()$co24aemcc
  })

  eightco24aem <- reactive({
    fiveco24aem() * mean(vm()) * fourco24aem()$cpiadjco24aem * 1000000
  })

  # vco24aeml <- reactive({
  #   (quantile((af(fourco24aem()$rtypeco24aem, bco24aem(), 1) * vco24aem()), 0.025)) / af(fourco24aem()$rtypeco24aem, quantile(bco24aem(), 0.025), 1)
  # })
  #
  # vco24aemu <- reactive({
  #   (quantile((af(fourco24aem()$rtypeco24aem, bco24aem(), 1) * vco24aem()), 0.975)) / af(fourco24aem()$rtypeco24aem, quantile(bco24aem(), 0.975), 1)
  # })

  vco24aeml <- reactive({
    quantile(vm(), 0.025)
  })
  vco24aemu <- reactive({
    quantile(vm(), 0.975)
  })

  nineco24aem <- reactive({
    sixco24aem() * vco24aeml() * fourco24aem()$cpiadjco24aem * 1000000
  })
  tenco24aem <- reactive({
    sevenco24aem() * vco24aemu() * fourco24aem()$cpiadjco24aem * 1000000
  })
  tenaco24aem <- reactive({
    100000 * fiveco24aem() / fourco24aem_allages()
  })

  pctxsco24aem <- reactive({
    xxpctxsco24aem()$co24aemaa
  })
  pctxsco24aeml95 <- reactive({
    xxpctxsco24aem()$co24aembb
  })
  pctxsco24aemu95 <- reactive({
    xxpctxsco24aem()$co24aemcc
  })

  # CO (1 hour) Elderly Cardiac Hospital Admissions
  set.seed(100)
  becha <- reactive({
    chg2(rnorm(input$itn, beta(input$co1_rtype, input$co1_crf, input$co1_incr, get_session_t("log-linear")), se(input$co1_rtype, input$co1_u95, input$co1_l95, input$co1_incr, get_session_t("log-linear"))))
  })

  set.seed(100)
  vecha <- reactive({
    chg2(valdist(input$sourcevsl7c, input$itn, input$vsl7, input$lvsl7, input$uvsl7, input$pcvsl7, input$plvsl7, get_session_t("normal"), get_session_t("discrete"), get_session_t("triangular")))
  })
  cpiy1echa <- reactive({
    get_session_cpi()[which(cpi_year() == input$sourcevsl7b), ]
  })

  cpiadjecha <- reactive({
    (cpiy()$cpi / cpiy1echa()$cpi) / ((1 + 0.01 * input$discountrate)^(foura_year() - input$baseyr))
  })

  rtypeecha <- reactive({
    input$co1_rtype
  })
  fourecha <- reactive({
    cbind(foura(), rtypeecha(), cpiadjecha())
  })

  # for threshold>0 and linear crf
  xpctxsco1aem1a <- reactive({
    outer(co1thrchg(), beta(input$co1_rtype, input$co1_crf, input$co1_incr, get_session_t("log-linear")), afb)
  })
  xpctxsco1aem2a <- reactive({
    thr2(xpctxsco1aem1a(), wtnum)
  })
  ypctxsco1aem1 <- reactive({
    thr3(xpctxsco1aem2a())
  })

  xpctxsco1aem1b <- reactive({
    outer(co1thrchg(), quantile(becha(), 0.025), afb)
  })
  xpctxsco1aem2b <- reactive({
    thr2(xpctxsco1aem1b(), wtnum)
  })
  ypctxsco1aem2 <- reactive({
    thr3(xpctxsco1aem2b())
  })

  xpctxsco1aem1c <- reactive({
    outer(co1thrchg(), quantile(becha(), 0.975), afb)
  })
  xpctxsco1aem2c <- reactive({
    thr2(xpctxsco1aem1c(), wtnum)
  })
  ypctxsco1aem3 <- reactive({
    thr3(xpctxsco1aem2c())
  })

  # for threshold>0 and log-linear crf
  xpctxsco1aem1d <- reactive({
    outer(co1thrchg(), beta(input$co1_rtype, input$co1_crf, input$co1_incr, get_session_t("log-linear")), afa)
  })
  xpctxsco1aem2d <- reactive({
    thr2(xpctxsco1aem1d(), wtnum)
  })
  zpctxsco1aem1 <- reactive({
    thr3(xpctxsco1aem2d())
  })

  xpctxsco1aem1e <- reactive({
    outer(co1thrchg(), quantile(becha(), 0.025), afa)
  })
  xpctxsco1aem2e <- reactive({
    thr2(xpctxsco1aem1e(), wtnum)
  })
  zpctxsco1aem2 <- reactive({
    thr3(xpctxsco1aem2e())
  })

  xpctxsco1aem1f <- reactive({
    outer(co1thrchg(), quantile(becha(), 0.975), afa)
  })
  xpctxsco1aem2f <- reactive({
    thr2(xpctxsco1aem1f(), wtnum)
  })
  zpctxsco1aem3 <- reactive({
    thr3(xpctxsco1aem2f())
  })

  xxpctxsco1aem <- reactive({
    if (input$cothr == 0 & input$co1_rtype == get_session_t("linear")) {
      xpctxsco1aem1 <- outer(co1regchg(), beta(input$co1_rtype, input$co1_crf, input$co1_incr, get_session_t("log-linear")), afb)
      xpctxsco1aem2 <- outer(co1regchg(), quantile(becha(), 0.025), afb)
      xpctxsco1aem3 <- outer(co1regchg(), quantile(becha(), 0.975), afb)
    } else if (input$cothr == 0 & input$co1_rtype == get_session_t("log-linear")) {
      xpctxsco1aem1 <- outer(co1regchg(), beta(input$co1_rtype, input$co1_crf, input$co1_incr, get_session_t("log-linear")), afa)
      xpctxsco1aem2 <- outer(co1regchg(), quantile(becha(), 0.025), afa)
      xpctxsco1aem3 <- outer(co1regchg(), quantile(becha(), 0.975), afa)
    } else if (input$cothr > 0 & input$co1_rtype == get_session_t("linear")) {
      xpctxsco1aem1 <- ypctxsco1aem1()
      xpctxsco1aem2 <- ypctxsco1aem2()
      xpctxsco1aem3 <- ypctxsco1aem3()
    } else if (input$cothr > 0 & input$co1_rtype == get_session_t("log-linear")) {
      xpctxsco1aem1 <- zpctxsco1aem1()
      xpctxsco1aem2 <- zpctxsco1aem2()
      xpctxsco1aem3 <- zpctxsco1aem3()
    }
    return(list(co1aemaa = xpctxsco1aem1, co1aembb = xpctxsco1aem2, co1aemcc = xpctxsco1aem3))
  })

  fiveecha <- reactive({
    (fourecha_age65plus() / 1000000) * fourecha_Elderly_Cardiac_Hospital() * xxpctxsco1aem()$co1aemaa
  })
  sixecha <- reactive({
    (fourecha_age65plus() / 1000000) * fourecha_Elderly_Cardiac_Hospital() * xxpctxsco1aem()$co1aembb
  })
  sevenecha <- reactive({
    (fourecha_age65plus() / 1000000) * fourecha_Elderly_Cardiac_Hospital() * xxpctxsco1aem()$co1aemcc
  })

  eightecha <- reactive({
    fiveecha() * mean(vecha()) * fourecha()$cpiadjecha
  })

  # vechal <- reactive({
  #   chg3(af(fourecha()$rtypeecha, quantile(becha(), 0.025), 1), quantile((af(fourecha()$rtypeecha, becha(), 1) * vecha()), 0.025))
  # })
  # xechal <- reactive({
  #   vechal() * fourecha()$cpiadjecha
  # })
  #
  # vechau <- reactive({
  #   (quantile((af(fourecha()$rtypeecha, becha(), 1) * vecha()), 0.975)) / af(fourecha()$rtypeecha, quantile(becha(), 0.975), 1)
  # })
  # xvechau <- reactive({
  #   vechau() * fourecha()$cpiadjecha
  # })


  vechal <- reactive({
    quantile(vecha(), 0.025)
  })
  vechau <- reactive({
    quantile(vecha(), 0.975)
  })

  nineecha <- reactive({
    sixecha() * vechal() * fourecha()$cpiadjecha
  })
  tenecha <- reactive({
    sevenecha() * vechau() * fourecha()$cpiadjecha
  })
  tenaecha <- reactive({
    100000 * fiveecha() / fourecha_age65plus()
  })

  pctxsecha <- reactive({
    xxpctxsco1aem()$co1aemaa
  })
  pctxsechal95 <- reactive({
    xxpctxsco1aem()$co1aembb
  })
  pctxsechau95 <- reactive({
    xxpctxsco1aem()$co1aemcc
  })

  # Benzene cancer
  fivebzc <- reactive({
    (fourb_allages()) * input$bziur * (fourb()$bz_2 - fourb()$bz_1)
  })
  sixbzc <- reactive(100000 * fivebzc() / fourb_allages())
  eightbzc <- reactive({
    fivebzc() * input$bzdaly
  })
  elevenbzc <- reactive({
    cbind(fourb_year(), fourb_scenario(), foura()[c(1, 2)], input$bzname1, input$bzoutcome1, fivebzc(), sixbzc(), eightbzc(), fourb_allages())
  })

  # 1,3 butadiene cancer
  fivebtc <- reactive({
    (fourb_allages()) * input$btiur * (fourb()$bt_2 - fourb()$bt_1)
  })
  sixbtc <- reactive(100000 * fivebtc() / fourb_allages())
  eightbtc <- reactive({
    fivebtc() * input$btdaly
  })
  elevenbtc <- reactive({
    cbind(fourb_year(), fourb_scenario(), foura()[c(1, 2)], input$btname1, input$btoutcome1, fivebtc(), sixbtc(), eightbtc(), fourb_allages())
  })
  # combine the two cancer outcomes
  eleventx_r1 <- reactive({
    rbind(elevenbzc(), setNames(elevenbtc(), names(elevenbzc())))
  })

  eleventx_r2 <- reactive({
    txc1 <- eleventx_r1()
    colnames(txc1) <- c(
      get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), "province", get_session_t("pollutant"),
      get_session_t("endpoint"), "counts", "counts_per_100k", "DALYs", "pop"
    )
    txc1
  })

  # aggregate toxic on cancer nationally
  caneleventx_r1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, DALYs, pop) ~ year + scenario + pollutant + endpoint, eleventx_r2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, DALYs, pop) ~ année + scénario + polluant + paramètre, eleventx_r2(), sum, na.action = NULL)
    }
  })

  caneleventx_r2 <- reactive({
    cbind("Canada", caneleventx_r1()[c(1, 2, 3, 4, 5)], 100000 * caneleventx_r1()$counts / caneleventx_r1()$pop, caneleventx_r1()$DALYs)
  })
  caneleventx_r3 <- reactive({
    cbind(caneleventx_r2()[c(2, 3, 1, 4, 5, 6, 7, 8)])
  })

  caneleventx_r4 <- reactive({
    txc2 <- caneleventx_r3()
    colnames(txc2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "counts_per_100k", "DALYs"
    )
    txc2
  })

  # aggregate toxic on cancer at provincial level
  proeleventx_r1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, DALYs, pop) ~ year + scenario + province + pollutant + endpoint, eleventx_r2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, DALYs, pop) ~ année + scénario + province + polluant + paramètre, eleventx_r2(), sum, na.action = NULL)
    }
  })

  proeleventx_r2 <- reactive({
    cbind(proeleventx_r1()[c(1, 2, 3, 4, 5, 6)], 100000 * proeleventx_r1()$counts / proeleventx_r1()$pop, proeleventx_r1()$DALYs)
  })

  proeleventx_r3 <- reactive({
    txc3 <- proeleventx_r2()
    colnames(txc3) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"),
      "counts", "counts_per_100k", "DALYs"
    )
    txc3
  })

  # merge results at national and provincial level
  canproeleventx_r1 <- reactive({
    rbind(proeleventx_r3(), caneleventx_r4())
  })
  canproeleventx_r2 <- reactive({
    merge(canproeleventx_r1(), get_session_geocode(), by.x = "region", by.y = "Name", all.x = TRUE)
  })

  canproeleventx_r3 <- reactive({
    canproeleventx_r2()[c(2, 3, 9, 1, 10, 4, 5, 6, 7, 8)]
  })

  cdeleventx_r1 <- reactive({
    cbind(eleventx_r2()[c(1, 2, 3, 4)], get_session_t("CD"), eleventx_r2()[c(5, 6, 7, 8, 9)])
  })
  cdeleventx_r2 <- reactive({
    txc4 <- cdeleventx_r1()
    colnames(txc4) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "region", "geotype",
      get_session_t("pollutant"), get_session_t("endpoint"), "counts", "counts_per_100k", "DALYs"
    )
    txc4
  })

  finaleleventx_r1 <- reactive({
    rbind(cdeleventx_r2(), canproeleventx_r3())
  })

  finaleleventx_r2 <- reactive({
    if (get_session_lang() == "en") {
      finaleleventx_r1()[with(finaleleventx_r1(), order(year, scenario, pollutant)), ]
    } else {
      finaleleventx_r1()[with(finaleleventx_r1(), order(année, scénario, polluant)), ]
    }
  })

  xfinaleleventx_r2 <- reactive({
    xtxc4 <- finaleleventx_r2()
    colnames(xtxc4) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Region"), get_session_t("Type of Geography"),
      get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("Counts per 100,000"),
      get_session_t("Disability-Adjusted Life Years")
    )
    xtxc4
  })

  x1finaleleventx_r2 <- reactive({
    merge_data(finaleleventx_r2(), get_session_xprov(), by.x = "region", by.y = "Province")
  })
  x2finaleleventx_r2 <- reactive({
    x1finaleleventx_r2()[, c(2, 3, 4, 5, 12, 6:10)]
  })

  x3finaleleventx_r2 <- reactive({
    x3tox1 <- x2finaleleventx_r2()
    colnames(x3tox1) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Type of Geography"), get_session_t("Region"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("Counts per 100,000"), get_session_t("Disability-Adjusted Life Years")
    )
    x3tox1
  })

  x4finaleleventx_r2 <- reactive({
    if (get_session_lang() == "en") {
      x3finaleleventx_r2()[with(x3finaleleventx_r2(), order(Year, Scenario, Geocode)), ]
    } else {
      x3finaleleventx_r2()[with(x3finaleleventx_r2(), order(Année, Scénario, Géocode)), ]
    }
  })

  # benzene non-cancer
  fivebznc <- reactive({
    (fourb()$bz_2 - fourb()$bz_1) / input$bzanrfc
  })
  elevenbznc <- reactive({
    cbind(fourb_year(), fourb_scenario(), foura()[c(1, 2)], input$bzname2, input$bzoutcome2, fivebznc())
  })

  # acetaldehyde non-cancer
  fiveacc <- reactive({
    (fourb()$ac_2 - fourb()$ac_1) / input$acanrfc
  })
  elevenacc <- reactive({
    cbind(fourb_year(), fourb_scenario(), foura()[c(1, 2)], input$acname2, input$acoutcome2, fiveacc())
  })

  # formaldehyde non-cancer
  fivefmc <- reactive({
    (fourb()$fm_2 - fourb()$fm_1) / input$fmanrfc
  })
  elevenfmc <- reactive({
    cbind(fourb_year(), fourb_scenario(), foura()[c(1, 2)], input$fmname2, input$fmoutcome2, fivefmc())
  })

  eleventxa_r1 <- reactive({
    rbind(elevenacc(), setNames(elevenfmc(), names(elevenacc())))
  })
  eleventxa_r2 <- reactive({
    rbind(eleventxa_r1(), setNames(elevenbznc(), names(eleventxa_r1())))
  })


  eleventxa_r3 <- reactive({
    txa1 <- eleventxa_r2()
    colnames(txa1) <- c(
      get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), "province", get_session_t("pollutant"),
      get_session_t("endpoint"), "Hazard_Quotient"
    )
    txa1
  })

  eleventxa_r4 <- reactive({
    cbind(eleventxa_r3()[c(1, 2, 3, 4)], get_session_t("CD"), eleventxa_r3()[c(5, 6, 7)])
  })

  eleventxa_r5 <- reactive({
    txaa2 <- eleventxa_r4()
    colnames(txaa2) <- c(
      get_session_t("year"), get_session_t("scenario"), "geocode", "region", "geotype", get_session_t("pollutant"),
      get_session_t("endpoint"), "Hazard_Quotient"
    )
    txaa2
  })

  # aggregated results at provincial and national level

  canproeleventxa_r1 <- reactive({
    Canprowexposure1()[c(1, 2, 3, 11, 12, 13, 14)]
  })

  canpro_Benzene1 <- reactive({
    cbind(Canprowexposure1()[c(1, 2, 3)], get_session_t("Benzene"), get_session_t("Hematological"), Canprowexposure1_Benzene() / input$bzanrfc)
  })
  canpro_Benzene2 <- reactive({
    ben1 <- canpro_Benzene1()
    colnames(ben1) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "Hazard_Quotient")
    ben1
  })

  canpro_Acetaldehyde1 <- reactive({
    cbind(Canprowexposure1()[c(1, 2, 3)], get_session_t("Acetaldehyde"), get_session_t("Respiratory (histological)"), Canprowexposure1_Acetaldehyde() / input$acanrfc)
  })
  canpro_Acetaldehyde2 <- reactive({
    acet1 <- canpro_Acetaldehyde1()
    colnames(acet1) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "Hazard_Quotient")
    acet1
  })

  canpro_Formaldehyde1 <- reactive({
    cbind(Canprowexposure1()[c(1, 2, 3)], get_session_t("Formaldehyde"), get_session_t("Respiratory (asthma)"), Canprowexposure1_Formaldehyde() / input$fmanrfc)
  })

  canpro_Formaldehyde2 <- reactive({
    forma1 <- canpro_Formaldehyde1()
    colnames(forma1) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "Hazard_Quotient")
    forma1
  })

  canpro_allthreenc1 <- reactive({
    rbind(canpro_Acetaldehyde2(), canpro_Benzene2(), canpro_Formaldehyde2())
  })

  canpro_allthreenc2 <- reactive({
    merge(canpro_allthreenc1(), get_session_geocode(), by.x = "region", by.y = "Name", all.x = TRUE)
  })

  canpro_allthreenc3 <- reactive({
    canpro_allthreenc2()[c(2, 3, 7, 1, 8, 4, 5, 6)]
  })

  toxicnoncancerfinal1 <- reactive({
    rbind(eleventxa_r5(), canpro_allthreenc3())
  })

  toxicnoncancerfinal2 <- reactive({
    if (get_session_lang() == "en") {
      toxicnoncancerfinal1()[with(toxicnoncancerfinal1(), order(year, scenario, pollutant)), ]
    } else {
      toxicnoncancerfinal1()[with(toxicnoncancerfinal1(), order(année, scénario, polluant)), ]
    }
  })

  x1toxicnoncancerfinal2 <- reactive({
    merge_data(toxicnoncancerfinal2(), get_session_xprov(), by.x = "region", by.y = "Province")
  })
  x2toxicnoncancerfinal2 <- reactive({
    x1toxicnoncancerfinal2()[, c(2, 3, 4, 5, 10, 6:8)]
  })

  x3toxicnoncancerfinal2 <- reactive({
    x3btox <- x2toxicnoncancerfinal2()
    colnames(x3btox) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), get_session_t("Type of Geography"), get_session_t("Region"), get_session_t("Pollutant"),
      get_session_t("Endpoint"), get_session_t("Hazard Quotient")
    )
    x3btox
  })

  x4toxicnoncancerfinal2 <- reactive({
    if (get_session_lang() == "en") {
      x3toxicnoncancerfinal2()[with(x3toxicnoncancerfinal2(), order(Year, Scenario, Geocode)), ]
    } else {
      x3toxicnoncancerfinal2()[with(x3toxicnoncancerfinal2(), order(Année, Scénario, Géocode)), ]
    }
  })

  eleven1 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Chronic Exposure Mortality"), five(), six(), seven(), eight(), nine(), ten(), tena(), pctxsmort(), pctxsmortl95(), pctxsmortu95(), leyr(), leyrl95(), leyru95(), get_session_t("mortality"), four_age25plus())
  })
  eleven2 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Acute Respiratory Symptom Days"), fivearsd(), sixarsd(), sevenarsd(), eightarsd(), ninearsd(), tenarsd(), tenaarsd(), pctxsarsd(), pctxsarsdl95(), pctxsarsdu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven3 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Adult Chronic Bronchitis Cases"), fiveacbc(), sixacbc(), sevenacbc(), eightacbc(), nineacbc(), tenacbc(), tenaacbc(), pctxsacbc(), pctxsacbcl95(), pctxsacbcu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven4 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Cardiac Emergency Room Visits"), fivecerv(), sixcerv(), sevencerv(), eightcerv(), ninecerv(), tencerv(), tenacerv(), pctxscerv(), pctxscervl95(), pctxscervu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven5 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Cardiac Hospital Admissions"), fivecha(), sixcha(), sevencha(), NA, NA, NA, tenacha(), pctxscha(), pctxschal95(), pctxschau95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven6 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Asthma Symptom Days"), fiveasd(), sixasd(), sevenasd(), eightasd(), nineasd(), tenasd(), tenaasd(), pctxsasd(), pctxsasdl95(), pctxsasdu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven7 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Child Acute Bronchitis Episodes"), fivecabe(), sixcabe(), sevencabe(), eightcabe(), ninecabe(), tencabe(), tenacabe(), pctxscabe(), pctxscabel95(), pctxscabeu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven8 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Respiratory Emergency Room Visits"), fivererv(), sixrerv(), sevenrerv(), eightrerv(), ninererv(), tenrerv(), tenarerv(), pctxsrerv(), pctxsrervl95(), pctxsrervu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven9 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Respiratory Hospital Admissions"), fiverha(), sixrha(), sevenrha(), NA, NA, NA, tenarha(), pctxsrha(), pctxsrhal95(), pctxsrhau95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven10 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("PM2.5"), get_session_t("Restricted Activity Days"), fiverad(), sixrad(), sevenrad(), eightrad(), ninerad(), tenrad(), tenarad(), pctxsrad(), pctxsradl95(), pctxsradu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven11 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], "NO2", get_session_t("Acute Exposure Mortality"), fiveaem(), sixaem(), sevenaem(), eightaem(), nineaem(), tenaem(), tenaaem(), pctxsno2aem(), pctxsno2aeml95(), pctxsno2aemu95(), NA, NA, NA, get_session_t("mortality"), NA)
  })
  eleven12 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], "O3", get_session_t("Acute Exposure Mortality"), fiveo3aem(), sixo3aem(), seveno3aem(), eighto3aem(), nineo3aem(), teno3aem(), tenao3aem(), pctxso3aem(), pctxso3aeml95(), pctxso3aemu95(), NA, NA, NA, get_session_t("mortality"), NA)
  })
  eleven13 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Chronic Exposure Respiratory Mortality"), fiveo3cerm(), sixo3cerm(), seveno3cerm(), eighto3cerm(), nineo3cerm(), teno3cerm(), tenao3cerm(), pctxso3cerm(), pctxso3cerml95(), pctxso3cermu95(), leyrrep(), leyrrepl95(), leyrrepu95(), get_session_t("mortality"), four_age30plus())
  })
  eleven14 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Minor Restricted Activity Days"), fivemrad(), sixmrad(), sevenmrad(), eightmrad(), ninemrad(), tenmrad(), tenamrad(), pctxsmrad(), pctxsmradl95(), pctxsmradu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven15 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Acute Respiratory Symptom Days"), fiveo3arsd(), sixo3arsd(), seveno3arsd(), eighto3arsd(), nineo3arsd(), teno3arsd(), tenao3arsd(), pctxso3arsd(), pctxso3arsdl95(), pctxso3arsdu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven16 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Asthma Symptom Days"), fiveo3asd(), sixo3asd(), seveno3asd(), eighto3asd(), nineo3asd(), teno3asd(), tenao3asd(), pctxso3asd(), pctxso3asdl95(), pctxso3asdu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven17 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Respiratory Emergency Room Visits"), fiveo3rerv(), sixo3rerv(), seveno3rerv(), eighto3rerv(), nineo3rerv(), teno3rerv(), tenao3rerv(), pctxso3rerv(), pctxso3rervl95(), pctxso3rervu95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven18 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], get_session_t("O3 Summer"), get_session_t("Respiratory Hospital Admissions"), fiveo3rha(), sixo3rha(), seveno3rha(), NA, NA, NA, tenao3rha(), pctxso3rha(), pctxso3rhal95(), pctxso3rhau95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })
  eleven19 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], "SO2", get_session_t("Acute Exposure Mortality"), fiveso2aem(), sixso2aem(), sevenso2aem(), eightso2aem(), nineso2aem(), tenso2aem(), tenaso2aem(), pctxsso2aem(), pctxsso2aeml95(), pctxsso2aemu95(), NA, NA, NA, get_session_t("mortality"), NA)
  })
  eleven20 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], "CO 24h", get_session_t("Acute Exposure Mortality"), fiveco24aem(), sixco24aem(), sevenco24aem(), eightco24aem(), nineco24aem(), tenco24aem(), tenaco24aem(), pctxsco24aem(), pctxsco24aeml95(), pctxsco24aemu95(), NA, NA, NA, get_session_t("mortality"), NA)
  })
  eleven21 <- reactive({
    cbind(foura_year(), foura_scenario(), foura()[c(1, 2)], "CO 1h", get_session_t("Elderly Cardiac Hospital Admissions"), fiveecha(), sixecha(), sevenecha(), eightecha(), nineecha(), tenecha(), tenaecha(), pctxsecha(), pctxsechal95(), pctxsechau95(), NA, NA, NA, get_session_t("morbidity"), NA)
  })


  eleven_r1 <- reactive({
    rbind(eleven1(), setNames(eleven2(), names(eleven1())))
  })
  eleven_r2 <- reactive({
    rbind(eleven_r1(), setNames(eleven3(), names(eleven_r1())))
  })
  eleven_r3 <- reactive({
    rbind(eleven_r2(), setNames(eleven4(), names(eleven_r2())))
  })
  eleven_r4 <- reactive({
    rbind(eleven_r3(), setNames(eleven5(), names(eleven_r3())))
  })
  eleven_r5 <- reactive({
    rbind(eleven_r4(), setNames(eleven6(), names(eleven_r4())))
  })
  eleven_r6 <- reactive({
    rbind(eleven_r5(), setNames(eleven7(), names(eleven_r5())))
  })
  eleven_r7 <- reactive({
    rbind(eleven_r6(), setNames(eleven8(), names(eleven_r6())))
  })
  eleven_r8 <- reactive({
    rbind(eleven_r7(), setNames(eleven9(), names(eleven_r7())))
  })
  eleven_r9 <- reactive({
    rbind(eleven_r8(), setNames(eleven10(), names(eleven_r8())))
  })
  eleven_r10 <- reactive({
    rbind(eleven_r9(), setNames(eleven11(), names(eleven_r9())))
  })
  eleven_r11 <- reactive({
    rbind(eleven_r10(), setNames(eleven12(), names(eleven_r10())))
  })
  eleven_r12 <- reactive({
    rbind(eleven_r11(), setNames(eleven13(), names(eleven_r11())))
  })
  eleven_r13 <- reactive({
    rbind(eleven_r12(), setNames(eleven14(), names(eleven_r12())))
  })
  eleven_r14 <- reactive({
    rbind(eleven_r13(), setNames(eleven15(), names(eleven_r13())))
  })
  eleven_r15 <- reactive({
    rbind(eleven_r14(), setNames(eleven16(), names(eleven_r14())))
  })
  eleven_r16 <- reactive({
    rbind(eleven_r15(), setNames(eleven17(), names(eleven_r15())))
  })
  eleven_r17 <- reactive({
    rbind(eleven_r16(), setNames(eleven18(), names(eleven_r16())))
  })
  eleven_r18 <- reactive({
    rbind(eleven_r17(), setNames(eleven19(), names(eleven_r17())))
  })
  eleven_r19 <- reactive({
    rbind(eleven_r18(), setNames(eleven20(), names(eleven_r18())))
  })
  eleven_r20 <- reactive({
    rbind(eleven_r19(), setNames(eleven21(), names(eleven_r19())))
  })

  reqvars <- reactive({
    na_replace(eleven_r20()[c(1:21)], 0)
  })
  reqvars2 <- reactive({
    na_replace(eleven_r20()[c(1:20)], 0)
  })

  twelveb0 <- reactive({
    a <- reqvars()
    colnames(a) <- c(
      get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), "province", get_session_t("pollutant"), get_session_t("endpoint"), "counts",
      "L95CI_counts", "U95CI_counts", "valuation", "L95CI_val", "U95CI_val", "counts_per_100K",
      "proportional_change", "L95CI_p", "U95CI_p", "life_year", "L95CI_le", "U95CI_le", "mort", "pop"
    )
    a
  })

  twelveb <- reactive({
    if (get_session_lang() == "en") {
      twelveb0()[with(twelveb0(), order(year, scenario, CDUID, pollutant)), ]
    } else {
      twelveb0()[with(twelveb0(), order(année, scénario, IDUDR, polluant)), ]
    }
  })

  # prepare data for maps
  # select status quo concentrations
  # pone <- reactive({
  #   pollutantdata()[, c(1:3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)]
  # })
  # # convert from wide to long format
  # plong1 <- reactive({
  #   if (get_session_lang() == "en") {
  #     gather(pone(), pollutant, value, pm25_2, no2_2, o3_2, summero3_2, co24h_2, co1h_2, so2_2, bz_2, bt_2, ac_2, fm_2)
  #   } else {
  #     gather(pone(), polluant, valeur, pm25_2, no2_2, o3_2, o3été_2, co24h_2, co1h_2, so2_2, bz_2, bt_2, ac_2, fm_2)
  #   }
  # })

  # plong2 <- reactive({
  #   merge(plong1(), pollnames, by = get_session_t("pollutant"))
  # })
  # plong3 <- reactive({
  #   cbind(plong2()[c(2:4, 6)], get_session_t("None"), get_session_t("Pollutant concentration"), plong2()[c(5)])
  # })
  # plong <- reactive({
  #   a <- plong3()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  # obtain counts/100k
  # map1a <- reactive({
  #   cbind(twelveb()[c(1:3, 5, 6)], get_session_t("Counts/100k"), twelveb()[c(13)])
  # })
  # map1 <- reactive({
  #   a <- map1a()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  # obtain valuation, population, merge
  # vals <- reactive({
  #   twelveb()[c(1:3, 5, 6, 10)]
  # })
  # pops <- reactive({
  #   popdata()[c(1, 2, 10)]
  # })
  # vp <- reactive({
  #   merge(vals(), pops(), by = c(get_session_t("year"), get_session_t("CDUID")))
  # })
  # # calculate per capita valuation
  # map2a <- reactive({
  #   if (get_session_lang() == "en") {
  #     cbind(vp(), vp()$valuation / vp()$allages)
  #   } else {
  #     cbind(vp(), vp()$valuation / vp()$touslesâges)
  #   }
  # })
  #
  # map2b <- reactive({
  #   cbind(map2a()[c(1, 3, 2, 4, 5)], get_session_t("Per capita valuation"), map2a()[c(8)])
  # })
  # map2 <- reactive({
  #   a <- map2b()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  # obtain life years
  # map3a <- reactive({
  #   cbind(twelveb()[c(1:3, 5, 6)], get_session_t("Change in life expectancy"), twelveb()[c(17)])
  # })
  # map3 <- reactive({
  #   a <- map3a()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })

  # sum all mortality by CD
  # mapagg <- reactive({
  #   twelveb()[c(1:3, 5:7)]
  # })
  #
  # cdaggmort1 <- reactive({
  #   if (get_session_lang() == "en") {
  #     mapagg()[which(mapagg()$endpoint == "Chronic Exposure Mortality" | mapagg()$endpoint == "Chronic Exposure Respiratory Mortality" | mapagg()$endpoint == "Acute Exposure Mortality"), ]
  #   } else {
  #     mapagg()[which(mapagg()$paramètre == "Mortalité liée à une exposition chronique" | mapagg()$paramètre == "Mortalité respiratoire liée à une exposition chronique" | mapagg()$paramètre == "Mortalité liée à une exposition aiguë"), ]
  #   }
  # })

  # cdaggmort2 <- reactive({
  #   if (get_session_lang() == "en") {
  #     as.data.frame(aggregate(counts ~ year + scenario + CDUID, cdaggmort1(), sum, na.action = NULL))
  #   } else {
  #     as.data.frame(aggregate(counts ~ année + scénario + IDUDR, cdaggmort1(), sum, na.action = NULL))
  #   }
  # })
  #
  # cdaggmort2a <- reactive({
  #   popdata()[c(1, 2, 10)]
  # })
  # cdaggmort2b <- reactive({
  #   merge(cdaggmort2(), cdaggmort2a(), by = c(get_session_t("year"), get_session_t("CDUID")))
  # })

  # cdaggmort2c <- reactive({
  #   if (get_session_lang() == "en") {
  #     cbind(cdaggmort2b()[c(1, 3, 2)], "All (CO, NO2, O3, PM2.5, SO2)", "Total Mortality", "Counts/100k", 100000 * cdaggmort2b()$counts / cdaggmort2b()$allages)
  #   } else {
  #     cbind(cdaggmort2b()[c(1, 3, 2)], "Tous (CO, NO2, O3, PM2,5, SO2)", "Total du nombre de mortalités", "Comptes par 100 000", 100000 * cdaggmort2b()$counts / cdaggmort2b()$touslesâges)
  #   }
  # })

  # cdaggmort3 <- reactive({
  #   as.data.frame(cdaggmort2c())
  # })
  #
  # cdaggmort4 <- reactive({
  #   mtot <- cdaggmort3()
  #   colnames(mtot) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   mtot
  # })

  # sum valuation by CD
  # mapaggv <- reactive({
  #   twelveb()[c(1:3, 5:10)]
  # })
  #
  # cdaggval2 <- reactive({
  #   if (get_session_lang() == "en") {
  #     as.data.frame(aggregate(valuation ~ year + scenario + CDUID, mapaggv(), sum, na.action = NULL))
  #   } else {
  #     as.data.frame(aggregate(valuation ~ année + scénario + IDUDR, mapaggv(), sum, na.action = NULL))
  #   }
  # })

  # cdaggval2a <- reactive({
  #   popdata()[c(1, 2, 10)]
  # })
  # cdaggval2b <- reactive({
  #   merge(cdaggval2(), cdaggval2a(), by = c(get_session_t("year"), get_session_t("CDUID")))
  # })
  #
  # cdaggval2c <- reactive({
  #   if (get_session_lang() == "en") {
  #     cbind(cdaggval2b()[c(1, 3, 2)], "All (CO, NO2, O3, PM2.5, SO2)", "Total Valuation", "Per capita valuation", cdaggval2b()$valuation / cdaggval2b()$allages)
  #   } else {
  #     cbind(cdaggval2b()[c(1, 3, 2)], "Tous (CO, NO2, O3, PM2,5, SO2)", "Évaluation économique totale", "Évaluation économique par habitant", cdaggval2b()$valuation / cdaggval2b()$touslesâges)
  #   }
  # })

  # cdaggval3 <- reactive({
  #   as.data.frame(cdaggval2c())
  # })
  #
  #
  # cdaggval4 <- reactive({
  #   vtot <- cdaggval3()
  #   colnames(vtot) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   vtot
  # })
  #
  # mapbz1 <- reactive({
  #   cbind(elevenbzc()[c(1, 2, 3, 5, 6)], get_session_t("Counts/100k"), elevenbzc()[c(8)])
  # })
  # mapbz <- reactive({
  #   a <- mapbz1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  # mapbt1 <- reactive({
  #   cbind(elevenbtc()[c(1, 2, 3, 5, 6)], get_session_t("Counts/100k"), elevenbtc()[c(8)])
  # })
  # mapbt <- reactive({
  #   a <- mapbt1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })

  # mapbznc1 <- reactive({
  #   cbind(elevenbznc()[c(1, 2, 3, 5, 6)], get_session_t("Hazard Quotient"), elevenbznc()[c(7)])
  # })
  # mapbznc <- reactive({
  #   a <- mapbznc1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  # mapacc1 <- reactive({
  #   cbind(elevenacc()[c(1, 2, 3, 5, 6)], get_session_t("Hazard Quotient"), elevenacc()[c(7)])
  # })
  # mapacc <- reactive({
  #   a <- mapacc1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  #
  # mapfmc1 <- reactive({
  #   cbind(elevenfmc()[c(1, 2, 3, 5, 6)], get_session_t("Hazard Quotient"), elevenfmc()[c(7)])
  # })
  # mapfmc <- reactive({
  #   a <- mapfmc1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  #
  # mapbzbt1 <- reactive({
  #   rbind(mapbz(), mapbt())
  # })
  #
  # mapbzbt2 <- reactive({
  #   if (get_session_lang() == "en") {
  #     aggregate(value ~ year + scenario + CDUID, mapbzbt1(), sum, na.action = NULL)
  #   } else {
  #     aggregate(valeur ~ année + scénario + IDUDR, mapbzbt1(), sum, na.action = NULL)
  #   }
  # })
  #
  # mapbzbt3 <- reactive({
  #   cbind(mapbzbt2()[c(1:3)], get_session_t("All Toxics (cancer)"), "Cancer", get_session_t("Counts/100k"), mapbzbt2()[c(4)])
  # })
  # mapbzbt <- reactive({
  #   a <- mapbzbt3()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  #
  # mapbmr1 <- reactive({
  #   cbind(three[c(4)], get_session_t("None"), three[c(1)], get_session_t("None"), get_session_t("Non-accidental mortality"), get_session_t("Baseline rate/100k"), three[c(5)] / 10)
  # })
  # mapbmr <- reactive({
  #   a <- mapbmr1()
  #   colnames(a) <- c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value"))
  #   a
  # })
  #
  # # concatenate rows with status quo concentrations, counts/100k, valuation/100k, life years
  # mapdata0 <- reactive({
  #   rbind(plong(), map1(), map2(), map3(), cdaggmort4(), cdaggval4(), mapbz(), mapbt(), mapbzbt(), mapbmr(), mapbznc(), mapacc(), mapfmc())
  # })
  #
  #
  # mapep <- reactive({
  #   ifelse(input$mdp == get_session_t("PM2.5"), input$mde1,
  #     ifelse(input$mdp == "NO2", input$mde2,
  #       ifelse(input$mdp == "CO 24h", input$mde3,
  #         ifelse(input$mdp == get_session_t("1,3-Butadiene"), input$mde6,
  #           ifelse(input$mdp == "SO2", input$mde11,
  #             ifelse(input$mdp == "O3", input$mde12,
  #               ifelse(input$mdp == get_session_t("O3 Summer"), input$mde13,
  #                 ifelse(input$mdp == get_session_t("All (CO, NO2, O3, PM2.5, SO2)"), input$mde4,
  #                   ifelse(input$mdp == get_session_t("Benzene"), input$mde5,
  #                     ifelse(input$mdp == get_session_t("Formaldehyde"), input$mde7,
  #                       ifelse(input$mdp == get_session_t("Acetaldehyde"), input$mde8,
  #                         ifelse(input$mdp == get_session_t("All Toxics (cancer)"), input$mde9,
  #                           ifelse(input$mdp == get_session_t("All Toxics (non-cancer)"), input$mde10,
  #                             ifelse(input$mdp == get_session_t("None"), input$mde14)
  #                           )
  #                         )
  #                       )
  #                     )
  #                   )
  #                 )
  #               )
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  # })
  # mapmt <- reactive({
  #   ifelse(input$mdp == get_session_t("PM2.5"),
  #     ifelse(input$mde1 == get_session_t("None"), input$mdc1, input$mdm1),
  #     ifelse(input$mdp == "NO2",
  #       ifelse(input$mde2 == get_session_t("None"), input$mdc2, input$mdm2),
  #       ifelse(input$mdp == get_session_t("Benzene"),
  #         ifelse(input$mde5 == get_session_t("None"), input$mdc3, ifelse(input$mde5 == "Cancer", input$mdm5, input$mdm6)),
  #         ifelse(input$mdp == get_session_t("1,3-Butadiene"),
  #           ifelse(input$mde6 == get_session_t("None"), input$mdc4, input$mdm7),
  #           ifelse(input$mdp == get_session_t("Formaldehyde"),
  #             ifelse(input$mde7 == get_session_t("None"), input$mdc5, input$mdm9),
  #             ifelse(input$mdp == get_session_t("Acetaldehyde"),
  #               ifelse(input$mde8 == get_session_t("None"), input$mdc6, input$mdm8),
  #               ifelse(input$mdp == "CO 24h",
  #                 ifelse(input$mde3 == get_session_t("None"), input$mdc7, input$mdm13),
  #                 ifelse(input$mdp == "SO2",
  #                   ifelse(input$mde11 == get_session_t("None"), input$mdc8, input$mdm14),
  #                   ifelse(input$mdp == "O3",
  #                     ifelse(input$mde12 == get_session_t("None"), input$mdc9, input$mdm15),
  #                     ifelse(input$mdp == get_session_t("O3 Summer"),
  #                       ifelse(input$mde13 == get_session_t("None"), input$mdc10, input$mdm12),
  #                       ifelse(input$mdp == get_session_t("All (CO, NO2, O3, PM2.5, SO2)"),
  #                         ifelse(input$mde4 == get_session_t("Total Mortality"), input$mdm3, input$mdm4),
  #                         ifelse(input$mdp == get_session_t("All Toxics (cancer)"),
  #                           ifelse(input$mde9 == "Cancer", input$mdm10),
  #                           ifelse(input$mdp == get_session_t("All Toxics (non-cancer)"),
  #                             ifelse(input$mde10 == get_session_t("Non-cancer"), input$mdm11),
  #                             ifelse(input$mdp == get_session_t("None"), input$mdm16)
  #                           )
  #                         )
  #                       )
  #                     )
  #                   )
  #                 )
  #               )
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  # })
  #
  # mapdata1 <- reactive({
  #   if (get_session_lang() == "en") {
  #     mapdata0()[which(mapdata0()$year == input$mdy & mapdata0()$scenario == input$scenario & mapdata0()$pollutant == input$mdp & mapdata0()$endpoint == mapep() & mapdata0()$metric == mapmt()), ]
  #   } else {
  #     mapdata0()[which(mapdata0()$année == input$mdy & mapdata0()$scénario == input$scenario & mapdata0()$polluant == input$mdp & mapdata0()$paramètre == mapep() & mapdata0()$mesure == mapmt()), ]
  #   }
  # })
  #
  # prov1 <- reactive({
  #   if (get_session_lang() == "en") {
  #     cbind(mapdata1()$CDUID, substr(mapdata1()$CDUID, 1, 2))
  #   } else {
  #     cbind(mapdata1()$IDUDR, substr(mapdata1()$IDUDR, 1, 2))
  #   }
  # })

  # prov2 <- reactive({
  #   a <- prov1()
  #   colnames(a) <- c(get_session_t("CDUID"), "PRUID")
  #   a
  # })
  #
  # prov3 <- reactive({
  #   merge(prov2(), prov, by.x = "PRUID", by.y = as.character("PRUID"))
  # })
  # prov4 <- reactive({
  #   prov3()[c(2, 3)]
  # })
  # provn <- reactive({
  #   a <- prov4()
  #   colnames(a) <- c(get_session_t("CDUID"), "Province")
  #   a
  # })
  # mapdata2 <- reactive({
  #   merge(mapdata1(), provn(), by = get_session_t("CDUID"))
  # })
  # mapdata3 <- reactive({
  #   mapdata2()[c(2, 3, 1, 8, 4:7)]
  # })
  # mapdata <- reactive({
  #   merge(cdmap, mapdata3(), by = get_session_t("CDUID"))
  # })

  # mapdatap1 <- reactive({
  #   if (get_session_lang() == "en") {
  #     mapdata3()[which(mapdata3()$Province == input$mdg, mapdata3()$year == input$mdy & mapdata3()$scenario == input$scenario & mapdata3()$pollutant == input$mdp & mapdata3()$endpoint == mapep() & mapdata3()$metric == mapmt()), ]
  #   } else {
  #     mapdata3()[which(mapdata3()$Province == input$mdg, mapdata3()$année == input$mdy & mapdata3()$scénario == input$scenario & mapdata3()$polluant == input$mdp & mapdata3()$paramètre == mapep() & mapdata3()$mesure == mapmt()), ]
  #   }
  # })
  #
  # mapdatap <- reactive({
  #   if (get_session_lang() == "en") {
  #     merge(cdmap1, mapdatap1(), by = "CDUID", all.x = FALSE)
  #   } else {
  #     merge(cdmap1_download, mapdatap1(), by = "IDUDR", all.x = FALSE)
  #   }
  # })

  twelveb2 <- reactive({
    twelveb()[c(1:20)]
  })

  xtwelveb1 <- reactive({
    subset(twelveb(), twelveb_endpoint() == get_session_t("Chronic Exposure Mortality") | twelveb_endpoint() == get_session_t("Chronic Exposure Respiratory Mortality"), select = c(1:19))
  })

  xxtwelveb1 <- reactive({
    xhh2 <- xtwelveb1()
    colnames(xhh2) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("CDUID"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"),
      get_session_t("L95CI Counts"), get_session_t("U95CI Counts"), get_session_t("Valuation"), get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"),
      get_session_t("Counts per 100,000"), get_session_t("Proportional Change"), get_session_t("L95CI Proportional Change"),
      get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"),
      get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    xhh2
  })

  # xxxtwelveb1 <- reactive({cbind(xxtwelveb1()[c(1,2,3,4,5,6)], as.data.frame(sapply((xxtwelveb1()[c(7:13)]),format_numbers, simplify = FALSE)))})
  xxxtwelveb1 <- reactive({
    xxtwelveb1()[, c(1:3, 5, 6, 7:12)]
  })
  a1cdtable <- reactive({
    if (get_session_lang() == "en") {
      cbind(xxxtwelveb1()[C(1:5)], as.data.frame(sapply((xxxtwelveb1()[c(6:11)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xxxtwelveb1()[C(1:5)], as.data.frame(sapply((xxxtwelveb1()[c(6:11)]), format_numbers2, simplify = FALSE)))
    }
  })
  xtwelveb2 <- reactive({
    subset(twelveb(), twelveb_endpoint() != get_session_t("Chronic Exposure Mortality") & twelveb_endpoint() != get_session_t("Chronic Exposure Respiratory Mortality"), select = c(1:16))
  })
  xtwelveb2b <- reactive({
    cbind(xtwelveb2(), NA, NA, NA)
  })
  xtwelveb2c <- reactive({
    hh2 <- xtwelveb2b()
    colnames(hh2) <- c(
      get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), "province", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val", "counts_per_100K", "proportional_change", "L95CI_p", "U95CI_p", "life_year", "L95CI_le", "U95CI_le"
    )
    hh2
  })

  xtwelveb3a <- reactive({
    list(xtwelveb1(), xtwelveb2c())
  })
  xtwelveb3b <- reactive({
    rbindlist(xtwelveb3a(), use.names = TRUE, fill = TRUE)
  })

  twelveb3i <- reactive({
    hh3 <- xtwelveb3b()
    colnames(hh3) <- c(
      get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), "province", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val", "counts_per_100K", "proportional_change", "L95CI_p", "U95CI_p", "life_expectancy_chg", "L95CI_le", "U95CI_le"
    )
    hh3
  })

  twelveb3 <- reactive({
    if (get_session_lang() == "en") {
      twelveb3i()[with(twelveb3i(), order(year, scenario, CDUID, province, pollutant, endpoint)), ]
    } else {
      twelveb3i()[with(twelveb3i(), order(année, scénario, IDUDR, province, polluant, paramètre)), ]
    }
  })

  outresultcd <- reactive({
    if (get_session_lang() == "en") {
      twelveb3()[with(twelveb3(), order(year, scenario, CDUID, province, pollutant, endpoint)), ]
    } else {
      twelveb3()[with(twelveb3(), order(année, scénario, IDUDR, province, polluant, paramètre)), ]
    }
  })

  x1outresultcd <- reactive({
    merge_data(outresultcd(), get_session_xprov(), by.x = "province", by.y = "Province")
  })
  x2outresultcd <- reactive({
    x1outresultcd()[, c(2, 3, 4, 21, 5:19)]
  })

  x3outresultcd <- reactive({
    x3cd <- x2outresultcd()
    colnames(x3cd) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("CDUID"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"),
      get_session_t("U95CI Counts"), get_session_t("Valuation"), get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Counts per 100,000"),
      get_session_t("Proportional Change"), get_session_t("L95CI Proportional Change"), get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"),
      get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    x3cd
  })

  x4outresultcd <- reactive({
    if (get_session_lang() == "en") {
      x3outresultcd()[with(x3outresultcd(), order(Year, Scenario, CDUID, Province, Pollutant, Endpoint)), ]
    } else {
      x3outresultcd()[with(x3outresultcd(), order(Année, Scénario, IDUDR, Province, Polluant, Paramètre)), ]
    }
  })

  # aggregate results nationally

  Canaggcountval1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change), chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p)) ~ year + scenario + pollutant + endpoint, twelveb2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change), chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p)) ~ année + scénario + polluant + paramètre, twelveb2(), sum, na.action = NULL)
    }
  })

  Canaggcountval3 <- reactive({
    cbind(
      Canaggcountval1()[c(1:10)], chg7(Canaggcountval1()$counts, Canaggcountval1()$V7),
      chg7(Canaggcountval1()$L95CI_counts, Canaggcountval1()$V8), chg7(Canaggcountval1()$U95CI_counts, Canaggcountval1()$V9)
    )
  })
  # Canaggcountval3 <- reactive({na_replace(Canaggcountval2(),0) })
  Canaggcountval4 <- reactive({
    cbind("Canada", Canaggcountval3())
  })
  Canaggcountval5 <- reactive({
    Canaggcountval4()[, c(2, 3, 1, 4:14)]
  })
  Canaggcountval6 <- reactive({
    k <- Canaggcountval5()
    colnames(k) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val", "proportional_change", "L95CI_p", "U95CI_p"
    )
    k
  })

  Canagglifeyr1 <- reactive({
    subset(twelveb(), twelveb_endpoint() == get_session_t("Chronic Exposure Mortality") | twelveb_endpoint() == get_session_t("Chronic Exposure Respiratory Mortality"), select = c(1:6, 17:21))
  })

  Canagglifeyr2 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + scenario + pollutant + endpoint, Canagglifeyr1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + scénario + polluant + paramètre, Canagglifeyr1(), sum, na.action = NULL)
    }
  })

  Canagglifeyr3a <- reactive({
    if (get_session_lang() == "en") {
      cbind.data.frame(
        Canagglifeyr2()$year, Canagglifeyr2()$scenario, "Canada", Canagglifeyr2()$pollutant, Canagglifeyr2()$endpoint, as.data.frame(chg7(Canagglifeyr2()$V1, Canagglifeyr2()$pop)),
        as.data.frame(chg7(Canagglifeyr2()$V2, Canagglifeyr2()$pop)), as.data.frame(chg7(Canagglifeyr2()$V3, Canagglifeyr2()$pop))
      )
    } else {
      cbind.data.frame(
        Canagglifeyr2()$année, Canagglifeyr2()$scénario, "Canada", Canagglifeyr2()$polluant, Canagglifeyr2()$paramètre, as.data.frame(chg7(Canagglifeyr2()$V1, Canagglifeyr2()$pop)),
        as.data.frame(chg7(Canagglifeyr2()$V2, Canagglifeyr2()$pop)), as.data.frame(chg7(Canagglifeyr2()$V3, Canagglifeyr2()$pop))
      )
    }
  })

  Canagglifeyr3 <- reactive({
    as.data.frame(Canagglifeyr3a())
  })

  Canagglifeyr4 <- reactive({
    le <- Canagglifeyr3()
    colnames(le) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "life_year", "L95CI_le", "U95CI_le")
    le
  })

  # aggregate results by province
  paggcountval1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change), chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p)) ~ year + scenario + province + pollutant + endpoint, twelveb2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val, chg7(counts, proportional_change), chg7(L95CI_counts, L95CI_p), chg7(U95CI_counts, U95CI_p)) ~ année + scénario + province + polluant + paramètre, twelveb2(), sum, na.action = NULL)
    }
  })

  paggcountval3 <- reactive({
    cbind(
      paggcountval1()[c(1:11)], chg7(paggcountval1()$counts, paggcountval1()$V7),
      chg7(paggcountval1()$L95CI_counts, paggcountval1()$V8), chg7(paggcountval1()$U95CI_counts, paggcountval1()$V9)
    )
  })

  paggcountval4 <- reactive({
    ac <- paggcountval3()
    colnames(ac) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val", "proportional_change", "L95CI_p", "U95CI_p"
    )
    ac
  })

  Proagglifeyr1 <- reactive({
    subset(twelveb(), twelveb_endpoint() == get_session_t("Chronic Exposure Mortality") | twelveb_endpoint() == get_session_t("Chronic Exposure Respiratory Mortality"), select = c(1:6, 17:21))
  })

  Proagglifeyr2 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ year + scenario + province + pollutant + endpoint, Proagglifeyr1(), sum, na.action = NULL)
    } else {
      aggregate(cbind(life_year * pop, L95CI_le * pop, U95CI_le * pop, pop) ~ année + scénario + province + polluant + paramètre, Proagglifeyr1(), sum, na.action = NULL)
    }
  })

  Proagglifeyr3 <- reactive({
    cbind(Proagglifeyr2()[c(1:5)], chg7(Proagglifeyr2()$V1, Proagglifeyr2()$pop), chg7(Proagglifeyr2()$V2, Proagglifeyr2()$pop), chg7(Proagglifeyr2()$V3, Proagglifeyr2()$pop))
  })

  Proagglifeyr4 <- reactive({
    le <- Proagglifeyr3()
    colnames(le) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "life_year", "L95CI_le", "U95CI_le")
    le
  })

  CanProagglifeyr <- reactive({
    rbind(Proagglifeyr4(), Canagglifeyr4())
  })

  CanProagglifeyr2 <- reactive({
    if (get_session_lang() == "en") {
      cbind(as.integer(CanProagglifeyr()$year), as.integer(CanProagglifeyr()$scenario), CanProagglifeyr()[c(3:8)])
    } else {
      cbind(as.integer(CanProagglifeyr()$année), as.integer(CanProagglifeyr()$scénario), CanProagglifeyr()[c(3:8)])
    }
  })

  CanProagglifeyr3 <- reactive({
    le2 <- CanProagglifeyr2()
    colnames(le2) <- c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "life_year", "L95CI_le", "U95CI_le")
    le2
  })

  # combine national and province results

  canprocountval1 <- reactive({
    rbind(paggcountval4(), Canaggcountval6())
  })

  canproresults1a <- reactive({
    left_join(canprocountval1(), CanProagglifeyr3(), by = c(get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint")))
  })

  canproresults1b <- reactive({
    subset(canproresults1a(), canproresults1a_endpoint() != get_session_t("Chronic Exposure Mortality") & canproresults1a_endpoint() != get_session_t("Chronic Exposure Respiratory Mortality"), select = c(1:14))
  })
  canproresults1c <- reactive({
    cbind(canproresults1b(), NA, NA, NA)
  })
  canproresults1d <- reactive({
    re2 <- canproresults1c()
    colnames(re2) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val", "proportional_change", "L95CI_p", "U95CI_p", "life_year", "L95CI_le", "U95CI_le"
    )
    re2
  })

  canproresults1e <- reactive({
    subset(canproresults1a(), canproresults1a_endpoint() == get_session_t("Chronic Exposure Mortality") | canproresults1a_endpoint() == get_session_t("Chronic Exposure Respiratory Mortality"))
  })
  xcanproresults1 <- reactive({
    rbind(canproresults1e(), canproresults1d())
  })

  xcanproresults1i <- reactive({
    hh5 <- xcanproresults1()
    colnames(hh5) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("pollutant"), get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts", "valuation",
      "L95CI_val", "U95CI_val", "proportional_change", "L95CI_p", "U95CI_p", "life_expectancy_chg", "L95CI_le", "U95CI_le"
    )
    hh5
  })

  canproresults1 <- reactive({
    if (get_session_lang() == "en") {
      xcanproresults1i()[with(xcanproresults1i(), order(year, scenario, region, pollutant, endpoint)), ]
    } else {
      xcanproresults1i()[with(xcanproresults1i(), order(année, scénario, region, polluant, paramètre)), ]
    }
  })

  outresultcanpro3 <- reactive({
    if (get_session_lang() == "en") {
      canproresults1()[with(canproresults1(), order(year, scenario, region, pollutant, endpoint)), ]
    } else {
      canproresults1()[with(canproresults1(), order(année, scénario, region, polluant, paramètre)), ]
    }
  })

  x1outresultcanpro3 <- reactive({
    merge_data(outresultcanpro3(), get_session_xprov(), by.x = "region", by.y = "Province")
  })
  x2outresultcanpro3 <- reactive({
    x1outresultcanpro3()[, c(2, 3, 19, 4:17)]
  })

  x3outresultcanpro3 <- reactive({
    x3pt <- x2outresultcanpro3()
    colnames(x3pt) <- c(
      get_session_t("Year"), get_session_t("Scenario"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"),
      get_session_t("Valuation"), get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation"), get_session_t("Proportional Change"), get_session_t("L95CI Proportional Change"),
      get_session_t("U95CI Proportional Change"), get_session_t("Life Expectancy Change"), get_session_t("L95CI Life Expectancy Change"), get_session_t("U95CI Life Expectancy Change")
    )
    x3pt
  })

  x4outresultcanpro3 <- reactive({
    if (get_session_lang() == "en") {
      x3outresultcanpro3()[with(x3outresultcanpro3(), order(Year, Scenario, Province, Pollutant, Endpoint)), ]
    } else {
      x3outresultcanpro3()[with(x3outresultcanpro3(), order(Année, Scénario, Province, Polluant, Paramètre)), ]
    }
  })


  # summary results
  # summary for variable "mort"

  provsummary1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val) ~ year + scenario + province + mort, twelveb2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val) ~ année + scénario + province + mort, twelveb2(), sum, na.action = NULL)
    }
  })

  provsummary2 <- reactive({
    if (get_session_lang() == "en") {
      provsummary1()[with(provsummary1(), order(year, scenario)), ]
    } else {
      provsummary1()[with(provsummary1(), order(année, scénario)), ]
    }
  })

  provsummary3 <- reactive({
    p1 <- provsummary2()
    colnames(p1) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val"
    )
    p1
  })

  cansummary1 <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val) ~ year + scenario + mort, twelveb2(), sum, na.action = NULL)
    } else {
      aggregate(cbind(counts, L95CI_counts, U95CI_counts, valuation, L95CI_val, U95CI_val) ~ année + scénario + mort, twelveb2(), sum, na.action = NULL)
    }
  })

  cansummary2 <- reactive({
    if (get_session_lang() == "en") {
      cansummary1()[with(cansummary1(), order(year, scenario)), ]
    } else {
      cansummary1()[with(cansummary1(), order(année, scénario)), ]
    }
  })

  cansummary3 <- reactive({
    cbind("Canada", cansummary2())
  })
  cansummary4 <- reactive({
    cansummary3()[, c(2, 3, 1, 4:10)]
  })
  cansummary5 <- reactive({
    can1 <- cansummary4()
    colnames(can1) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val"
    )
    can1
  })

  # for summary mortality

  canprovsummary <- reactive({
    rbind(provsummary3(), cansummary5())
  })

  canprovsummort <- reactive({
    subset(canprovsummary(), canprovsummary_endpoint() == get_session_t("mortality"))
  })

  # for summary morbidity
  canprovsummorb <- reactive({
    subset(canprovsummary(), canprovsummary_endpoint() == get_session_t("morbidity"))
  })

  canprovsummorb1 <- reactive({
    cbind(canprovsummorb()[c(1, 2, 3, 4)], NA, NA, NA, canprovsummorb()[c(8, 9, 10)])
  })
  canprovsummorb2 <- reactive({
    morb <- canprovsummorb1()
    colnames(morb) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val"
    )
    morb
  })


  summarymortmorb <- reactive({
    rbind(canprovsummort(), canprovsummorb2())
  })

  # for summary allendpoint

  sumallendpoint <- reactive({
    if (get_session_lang() == "en") {
      aggregate(cbind(valuation, L95CI_val, U95CI_val) ~ year + scenario + region, canprovsummary(), sum, na.action = NULL)
    } else {
      aggregate(cbind(valuation, L95CI_val, U95CI_val) ~ année + scénario + region, canprovsummary(), sum, na.action = NULL)
    }
  })

  sumallendpoint1 <- reactive({
    if (get_session_lang() == "en") {
      sumallendpoint()[with(sumallendpoint(), order(year, scenario)), ]
    } else {
      sumallendpoint()[with(sumallendpoint(), order(année, scénario)), ]
    }
  })

  sumallendpoint2 <- reactive({
    cbind(sumallendpoint1()[c(1, 2, 3)], get_session_t("allendpoints"), NA, NA, NA, sumallendpoint1()[c(4, 5, 6)])
  })
  sumallendpoint3 <- reactive({
    allendp <- sumallendpoint2()
    colnames(allendp) <- c(
      get_session_t("year"), get_session_t("scenario"), "region", get_session_t("endpoint"), "counts", "L95CI_counts", "U95CI_counts",
      "valuation", "L95CI_val", "U95CI_val"
    )
    allendp
  })


  summaryresultsi <- reactive({
    rbind(summarymortmorb(), sumallendpoint3())
  })

  summaryresults <- reactive({
    if (get_session_lang() == "en") {
      summaryresultsi()[with(summaryresultsi(), order(year, scenario, endpoint)), ]
    } else {
      summaryresultsi()[with(summaryresultsi(), order(année, scénario, paramètre)), ]
    }
  })

  x1summaryresults <- reactive({
    merge_data(summaryresults(), get_session_xprov(), by.x = "region", by.y = "Province")
  })
  x2summaryresults <- reactive({
    x1summaryresults()[, c(2, 3, 12, 4:10)]
  })

  x3summaryresults <- reactive({
    x3sum <- x2summaryresults()
    colnames(x3sum) <- c(
      get_session_t("Year"), get_session_t("Scenario"), "Province", get_session_t("Endpoint"), get_session_t("Counts"), get_session_t("L95CI Counts"), get_session_t("U95CI Counts"),
      get_session_t("Valuation"), get_session_t("L95CI Valuation"), get_session_t("U95CI Valuation")
    )
    x3sum
  })

  x4summaryresults <- reactive({
    if (get_session_lang() == "en") {
      x3summaryresults()[with(x3summaryresults(), order(Year, Scenario, Province, Endpoint)), ]
    } else {
      x3summaryresults()[with(x3summaryresults(), order(Année, Scénario, Province, Paramètre)), ]
    }
  })

  # join national and provincial results

  # output CD,national and provincial aggregate results and summary results

  data_list1 <- reactive({
    if (get_session_lang() == "en") {
      list(
        CDresults = x4outresultcd(),
        PTNationalresults = x4outresultcanpro3(),
        Summaryresults = x4summaryresults()
      )
    } else {
      list(
        RésultatsDR = x4outresultcd(),
        RésultatsPTnationale = x4outresultcanpro3(),
        Résultatssommaire = x4summaryresults()
      )
    }
  })

  output$d2 <- downloadHandler(
    filename = function() {
      get_session_t("AllResults.xlsx")
    },
    content = function(file) {
      write_xlsx(data_list1(), path = file)
    }
  )

  # Two handlers so language is fixed by which button was rendered (works in iframe / cross-tab)
  output$xsample_en <- downloadHandler(
    filename = function() "sample_inputs.csv",
    content = function(file) write.csv(xsample1_en, file, row.names = FALSE)
  )
  output$xsample_fr <- downloadHandler(
    filename = function() "exemple_entrée.csv",
    content = function(file) write.csv(xsample1_fr, file, row.names = FALSE)
  )

  data_listtx <- reactive({
    if (get_session_lang() == "en") {
      list(
        Cancer = x4finaleleventx_r2(),
        `Non-cancer` = x4toxicnoncancerfinal2()
      )
    } else {
      list(
        Cancer = x4finaleleventx_r2(),
        `Non-cancérigène` = x4toxicnoncancerfinal2()
      )
    }
  })

  output$d6 <- downloadHandler(
    filename = function() {
      get_session_t("Toxics.xlsx")
    },
    content = function(file) {
      write_xlsx(data_listtx(), path = file)
    }
  )

  output$d7 <- downloadHandler(
    filename = function() {
      get_session_t("basedata.xlsx")
    },
    content = function(file) {
      write_xlsx(x4outbasedata(), path = file)
    }
  )

  handle_error <- function(e, identifier) {
    # Log the error
    message("Error occurred in ", identifier, ": ", conditionMessage(e))

    # Remove 'hidden' class from elements with the specified data-identifier
    shinyjs::runjs(sprintf("
      document.querySelectorAll('[data-identifier=\"%s\"]').forEach(function(el) {
        el.classList.remove('hidden');
      });
    ", identifier))

    shinyjs::runjs("
      document.querySelectorAll('.loader-container').forEach(function(el) {
        el.classList.add('hidden');
      });
    ")

    print(identifier)
    return(NULL)
  }

  hide_error <- function(e, identifier) {
    # Log the error
    shinyjs::runjs(sprintf("
      document.querySelectorAll('[data-identifier=\"%s\"]').forEach(function(el) {
        el.classList.remove('hidden');
      });
    ", identifier))

    return(NULL)
  }
  # OVERALL RESULTS TABLE
  output$outputcd <- renderTable({
    tryCatch(
      {
        table_data <- head(a1cdtable(), 3)
        colnames(table_data) <- gsub("\\.", " ", colnames(table_data))
        table_data
      },
      error = function(e) handle_error(e, "outputcd")
    )
  })

  output$outputpt <- renderTable({
    tryCatch(
      {
        head(canproresults1(), 3)
      },
      error = function(e) handle_error(e, "outputpt")
    )
  })

  output$outputsummary <- renderTable({
    tryCatch(
      {
        head(summaryresults(), 3)
      },
      error = function(e) handle_error(e, "outputsummary")
    )
  })

  xxfinaleleventx_r2 <- reactive({
    if (get_session_lang() == "en") {
      cbind(xfinaleleventx_r2()[c(1:7)], as.data.frame(sapply((xfinaleleventx_r2()[c(8:10)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xfinaleleventx_r2()[c(1:7)], as.data.frame(sapply((xfinaleleventx_r2()[c(8:10)]), format_numbers2, simplify = FALSE)))
    }
  })

  # TOXICS TABLE
  output$outputtox <- renderTable({
    tryCatch(
      {
        head(xxpnametoxic(), 3)
      },
      error = function(e) handle_error(e, "outputtox")
    )
  })

  pnametoxic <- reactive({
    merge_data(xxfinaleleventx_r2(), get_session_xprov(), by.x = get_session_t("Region"), by.y = "Province")
  })
  xpnametoxic <- reactive({
    cbind(pnametoxic()[c(2, 3)], as.integer(pnametoxic_Geocode()), pnametoxic()[c(12, 6:10)])
  })

  xxpnametoxic <- reactive({
    xthh2 <- xpnametoxic()
    colnames(xthh2) <- c(
      get_session_t("Year"), get_session_t("Scenario"), get_session_t("Geocode"), "Province", get_session_t("Pollutant"), get_session_t("Endpoint"), get_session_t("Counts"),
      get_session_t("Counts per 100,000"), get_session_t("Disability-Adjusted Life Years")
    )
    xthh2
  })

  # Toxics non cancer
  output$outputtox1 <- renderTable({
    tryCatch(
      {
        head(toxicnoncancerfinal2(), 3)
      },
      error = function(e) handle_error(e, "outputtox1")
    )
  })

  # Baseline Data
  xoutbasedata <- reactive({
    merge_data(basedata(), get_session_xprov(), by.x = "province", by.y = "Province")
  })
  xxoutbasedata <- reactive({
    xoutbasedata()[c(2, 3, 31, 4:29)]
  })
  xxxoutbasedata <- reactive({
    xbase <- xxoutbasedata()
    colnames(xbase) <- c(
      get_session_t("Year"), get_session_t("CDUID"), "Province", get_session_t("Acute Exposure Mortality"), get_session_t("Chronic Exposure Mortality"),
      get_session_t("Respiratory Mortality"), get_session_t("Cardiovascular Mortality"), get_session_t("Cerebrovascular Mortality"),
      get_session_t("COPD Mortality"), get_session_t("Ischemic Heart Disease Mortality"), get_session_t("Lung Cancer Mortality"),
      get_session_t("Acute Respiratory Symptom Days"), get_session_t("Adult Chronic Bronchitis Cases"), get_session_t("Asthma Symptom Days"),
      get_session_t("Cardiac Emergency Room Visits"), get_session_t("Cardiac Hospital Admissions"), get_session_t("Child Acute Bronchitis"),
      get_session_t("Elderly Cardiac Hospital Admissions"), get_session_t("Minor Restricted Activity Days"),
      get_session_t("Respiratory Emergency Room Visits"), get_session_t("Respiratory Hospital Admissions"), get_session_t("Restricted Activity Days"),
      get_session_t("Aged 5 to 19"), get_session_t("Aged 5 to 19 non-asthma"), get_session_t("Aged 20 plus"), get_session_t("Aged 25 plus"),
      get_session_t("Aged 30 plus"), get_session_t("Aged 65 plus"), get_session_t("All Ages")
    )
    xbase
  })

  x4outbasedata <- reactive({
    if (get_session_lang() == "en") {
      xxxoutbasedata()[with(xxxoutbasedata(), order(Year, CDUID)), ]
    } else {
      xxxoutbasedata()[with(xxxoutbasedata(), order(Année, IDUDR)), ]
    }
  })

  xxxbaserates <- reactive({
    if (get_session_lang() == "en") {
      cbind(xxbaserates()[c(1:3)], as.data.frame(sapply((xxbaserates()[c(4:9)]), format_numbers, simplify = FALSE)))
    } else {
      cbind(xxbaserates()[c(1:3)], as.data.frame(sapply((xxbaserates()[c(4:9)]), format_numbers2, simplify = FALSE)))
    }
  })

  # BASELINE TABLE
  output$outputbaserate <- renderTable({
    tryCatch(
      {
        head(xxpnamebaseline(), 3)
      },
      error = function(e) handle_error(e, "outputbaserate")
    )
  })


  pnamebaseline <- reactive({
    merge_data(xxxbaserates(), get_session_xprov(), by.x = "Province", by.y = "Province")
  })
  xpnamebaseline <- reactive({
    pnamebaseline()[c(2, 3, 11, 4:9)]
  })

  xxpnamebaseline <- reactive({
    xbphh2 <- xpnamebaseline()
    colnames(xbphh2) <- c(
      get_session_t("Year"), get_session_t("Geocode"), "Province", get_session_t("Acute Exposure Mortality"), get_session_t("Chronic Exposure Mortality"), get_session_t("Respiratory Mortality"),
      get_session_t("Cardiovascular Mortality"), get_session_t("Cerebrovascular Mortality"), get_session_t("COPD Mortality")
    )
    xbphh2
  })


  # generate interactive map

  tmap_options(
    check.and.fix = TRUE, basemaps = c(Canvas = "Esri.WorldTopoMap"),
    overlays = c(Labels = paste0(
      "http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
      "World_Topo_Map_Reference/MapServer/tile/{z}/{y}/{x}"
    ))
  )
  df <- eventReactive(input$genmap, {
    tm_shape(mapdata()) + tm_fill(
      col = get_session_t("value"), title = paste(
        input$mdy, ifelse(input$scenario == get_session_t("None"), "", paste(get_session_t("Scenario"), input$scenario)), ifelse(input$mdp == get_session_t("None"), "", input$mdp),
        ifelse(mapep() == get_session_t("None"), "", mapep()), ifelse(mapmt() == get_session_t("Pollutant concentration"), "", mapmt()), ifelse(!mapep() == get_session_t("None"), "",
          ifelse(input$mdp == "CO 24h", "(ppm)",
            ifelse(input$mdp == "NO2" | input$mdp == "O3" | input$mdp == get_session_t("O3 Summer") | input$mdp == "SO2", "(ppb)", "(ug/m3)")
          )
        )
      ),
      palette = "Reds", n = 6, alpha = 0.5, id = get_session_t("CDNAME"), popup.vars = c(get_session_t("year"), get_session_t("scenario"), get_session_t("CDUID"), get_session_t("CDNAME"), "Province", get_session_t("pollutant"), get_session_t("endpoint"), get_session_t("metric"), get_session_t("value")),
      popup.format = popup_workaround()
    ) + tm_view(set.view = 4) + tm_layout(legend.format = legend_workaround(), legend.outside = TRUE) + tmap_options(qtm.minimap = TRUE) + tm_borders(col = "Black", lwd = 1, lty = "solid", alpha = NA, zindex = NA, group = NA)
  })

  # can't use i18n within a list, so need to create an object
  popup_workaround <- reactive({
    if (get_session_lang() == "en") {
      list("value" = list(digits = 1), "year" = list(big.mark = ""), "CDUID" = list(big.mark = ""))
    } else {
      list("valeur" = list(digits = 1), "année" = list(big.mark = ""), "IDUDR" = list(big.mark = ""))
    }
  })

  legend_workaround <- reactive({
    if (get_session_lang() == "en") {
      list(text.separator = "to")
    } else {
      list(text.separator = "à")
    }
  })

  output$my_tmap <- renderTmap({
    req(df())
    df()
  })

  observeEvent(input$genmap, {
    # Delay to ensure map rendering is complete
    shinyjs::delay(1000, {
      session$sendCustomMessage(type = "setMapHeight", list(height = "800px"))
    })
  })

  tmap_mode("plot")

  # generate national map for download
  output$downm <- downloadHandler(
    filename = function() {
      paste0(input$mdy, ifelse(input$scenario == get_session_t("None"), "", paste(get_session_t("Scenario"), input$scenario)), ifelse(input$mdp == get_session_t("None"), "", input$mdp), ifelse(mapep() == get_session_t("None"), "", mapep()), ifelse(mapmt() == get_session_t("Pollutant concentration"), "", mapmt()), get_session_t("map"), ".jpg")
    },
    content = function(file) {
      m <- tm_shape(mapdata()) + tm_fill(col = get_session_t("value"), title = paste(input$mdy, ifelse(input$scenario == get_session_t("None"), "", paste(get_session_t("Scenario"), input$scenario)), ifelse(input$mdp == get_session_t("None"), "", input$mdp), ifelse(mapep() == get_session_t("None"), "", mapep()), ifelse(mapmt() == get_session_t("Pollutant concentration"), "", mapmt()), ifelse(!mapep() == get_session_t("None"), "", ifelse(input$mdp == "CO 24h", "(ppm)", ifelse(input$mdp == "NO2" | input$mdp == "O3" | input$mdp == get_session_t("O3 Summer") | input$mdp == "SO2", "(ppb)", "(ug/m3)")))), palette = "Reds", id = get_session_t("value")) + tm_borders(col = "Grey", lwd = 1, lty = "solid", zindex = NA, group = NA)
      tmap_save(m, file = file)
    }
  )


  # generate provincial map for download based on user selected province
  observe({
    req(input$mdg)
    enable("downp")
  })

  output$downmp <- downloadHandler(
    filename = function() {
      paste0(input$mdg, input$mdy, ifelse(input$scenario == get_session_t("None"), "", paste(get_session_t("Scenario"), input$scenario)), ifelse(input$mdp == get_session_t("None"), "", input$mdp), ifelse(mapep() == get_session_t("None"), "", mapep()), ifelse(mapmt() == get_session_t("Pollutant concentration"), "", mapmt()), get_session_t("map"), ".jpg")
    },
    content = function(file) {
      mp <- tm_shape(mapdatap()) + tm_fill(col = get_session_t("value"), title = paste(input$mdy, ifelse(input$scenario == get_session_t("None"), "", paste(get_session_t("Scenario"), input$scenario)), ifelse(input$mdp == get_session_t("None"), "", input$mdp), ifelse(mapep() == get_session_t("None"), "", mapep()), ifelse(mapmt() == get_session_t("Pollutant concentration"), "", mapmt()), ifelse(!mapep() == get_session_t("None"), "", ifelse(input$mdp == "CO 24h", "(ppm)", ifelse(input$mdp == "NO2" | input$mdp == "O3" | input$mdp == get_session_t("O3 Summer") | input$mdp == "SO2", "(ppb)", "(ug/m3)")))), palette = "Reds", id = get_session_t("value")) + tm_borders(col = "Grey", lwd = 1, lty = "solid", zindex = NA, group = NA)
      tmap_save(mp, file = file)
    }
  )
}

shinyApp(ui, server)

# Set the directory to the root directory of your repository
# app_dir <- "./"

# Generate the 'dist' folder
# deployApp(appDir = app_dir)
