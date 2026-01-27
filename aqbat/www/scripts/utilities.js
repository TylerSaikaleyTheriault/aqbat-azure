const setUrlParam = (param) => {
  let url = new URL(window.location.href);
  url.searchParams.set('tab', param);
  window.history.pushState({}, '', url);
}

$(document).ready(function() {
  // change background color for nav bar if on French version
    if (window.location.href.includes("sante") || window.location.toString().includes("fr")) {
        document.styleSheets[0].insertRule(
          "html { background: linear-gradient(#f5f5f5 105.5px, white 100px) !important; }",
          document.styleSheets[0].cssRules.length
      );
    }


    wb.init(document);
    wb.init(document);
    // Get language from attribute, or detect from URL if not set yet (handles race condition)
    // Check multiple possible locations for lang attribute
    let currentLang = $('#aqbat').attr('lang') || 
                      $('html').attr('lang') || 
                      document.documentElement.lang ||
                      document.documentElement.getAttribute('lang');
    if (!currentLang || currentLang === '') {
      // If lang attribute not set, check URL for language indicators
      const url = window.location.href;
      currentLang = (url.includes("sante") || url.includes("/fr") || url.includes("lang=fr")) ? 'fr' : 'en';
    }
    
    // Add this code right here
    $(document).on('wb-ready.wb', function() {
      $('#full-screen').trigger('open.wb-overlay');
    });
    
    // console.log(currentLang);
    let timeExtensionIsValid = true;
    
    const TIMEOUT_WARNING_TIME = 9900000; // 2 hours 45 minutes 

    // Bind click events to elements with class 'internal-link'
    $(document).on('click', '.internal-link', function(e) {
      e.preventDefault(); // Prevent the default action
      const target = $(this).data('target'); // Use .data() to get the data-target attribute
      if (target) {
          // setUrlParam(target);
          Shiny.setInputValue('internal_link_clicked', target, {priority: 'event'}); // Set Shiny input value with the target tab
      }
    });

    // REMOVE ARIA-LABEL FROM ICONS
    function removeAriaLabel() {
      $('.fa-download').removeAttr('aria-label');
    }
    // Call the function to execute it
    removeAriaLabel();

    // Function to update button text
    function updateButtonText() {
      // Re-check language each time to handle cases where it's set later
      const aqbatLang = $('#aqbat').attr('lang');
      const htmlLang = $('html').attr('lang');
      const docLang = document.documentElement.lang;
      const docAttrLang = document.documentElement.getAttribute('lang');
      
      let lang = aqbatLang || htmlLang || docLang || docAttrLang;
      
      console.log('[Button Text Debug] Language detection:', {
        aqbatLang: aqbatLang,
        htmlLang: htmlLang,
        docLang: docLang,
        docAttrLang: docAttrLang,
        initialLang: lang,
        url: window.location.href
      });
      
      // If still not found, check URL
      if (!lang || lang === '' || lang === null || lang === undefined) {
        const url = window.location.href.toLowerCase();
        // Only set to French if we have clear French indicators
        if (url.includes("sante") || url.includes("/fr") || url.includes("lang=fr") || url.includes("?fr")) {
          lang = 'fr';
          console.log('[Button Text Debug] Detected French from URL');
        } else {
          // Default to English if no clear French indicators
          lang = 'en';
          console.log('[Button Text Debug] Defaulting to English (no French indicators in URL)');
        }
      }
      
      // Ensure we have a valid language value
      lang = (lang === 'fr') ? 'fr' : 'en';
      
      const buttonText = lang === "en" ? 'Upload' : "Télécharger";
      
      console.log('[Button Text Debug] Final language:', lang, 'Button text:', buttonText);
      
      $('.btn-file').each(function() {
        const $btn = $(this);
        const originalText = $btn.text().trim();
        
        // Try to find and replace the text node
        const textNodes = $btn.contents().filter(function() {
          return this.nodeType === 3; // Text nodes only
        });
        
        if (textNodes.length > 0) {
          // Replace the first text node
          textNodes.first().replaceWith(buttonText);
          console.log('[Button Text Debug] Replaced text node. Original:', originalText, 'New:', buttonText);
        } else {
          // If no text node found, try to set text content of first child or prepend
          const firstChild = $btn.children().first();
          if (firstChild.length) {
            firstChild.text(buttonText);
            console.log('[Button Text Debug] Set first child text. Original:', originalText, 'New:', buttonText);
          } else {
            $btn.prepend(buttonText);
            console.log('[Button Text Debug] Prepended text. Original:', originalText, 'New:', buttonText);
          }
        }
      });
    }
    
    // CHANGE BROWSE TO UPLOAD - Initial attempt
    updateButtonText();
    
    // Retry after a short delay in case lang attribute is set later
    setTimeout(updateButtonText, 100);
    setTimeout(updateButtonText, 500);
    
    // Add focus event listeners to buttons
    $('.btn-file').each(function() {
      $(this).on('focus', function(event) {
        event.target.scrollIntoView({ block: 'nearest', inline: 'nearest', behavior: 'smooth' });
      });
    });

    // Change placeholder text
    $('.form-control').each(function() {
      $(this).attr('placeholder', currentLang === 'en' ? 'No file selected' : 'Aucun fichier sélectionné');
    });

    // Use jQuery to select all spans with class 'scroll-link'
    $('.scroll-link').on('click', function(event) {
      $(this).addClass('clicked');
      event.preventDefault(); // Prevent any default behavior, just in case
      // Get the target ID from the 'data-link' attribute
      var targetId = $(this).attr('data-link'); // No need to remove the '#' since it's just the ID
      var targetElement = document.getElementById(targetId);

      if (targetElement) {
        targetElement.scrollIntoView({ behavior: 'auto' });
      } else {
        console.log('Target element not found for ID:', targetId);
      }
    });

    // MODAL SCROLL LOGIC
    $(".modal").on('shown.bs.modal', function () {
      const modalElement = document.getElementById("data-warning-centred-popup-modal");
      if (modalElement) {
          // Prevent default scrolling behavior
          const rect = modalElement.getBoundingClientRect();
          const scrollPosition = {
              type: 'scroll',
              top: window.scrollY || document.documentElement.scrollTop,
              left: window.scrollX || document.documentElement.scrollLeft
          };
          // Send the position to the parent window
          window.parent.postMessage(scrollPosition, '*');
      }
    });

    setInterval(() => {
      // Show the timeout warning modal by triggering the Bootstrap modal
      if(timeExtensionIsValid){
        $('#timeout-warning-centred-popup-modal').modal('show');
      }
    }, TIMEOUT_WARNING_TIME); // subtracting 15 minutes 

    $(document).on('shiny:connected', function() {
      window.onerror = function(message, source, lineno, colno, error) {
        Shiny.onInputChange('jsError', {message: message, source: source});
        return true;
      }
    });

    $('input[type="file"]').css({
      top: '0'
    });
    
    // remove all placeholder text for all inputs
    $('input').attr('placeholder', ''); 

    // disconnection alert
    $(document).off('shiny:disconnected').on('shiny:disconnected', function(e) {
      // hide timeout warning modal
      timeExtensionIsValid = false;
      $('#timeout-warning-centred-popup-modal').modal('hide');
      // show disconnection warning modal
      $('#disconnect-warning-centred-popup-modal').modal('show');
      $('#disconnect-message').removeClass('hidden'); 
    });

    // remove tabindex from tab panels
    function updateTabIndex() {
      const tabPanels = document.querySelectorAll('.tab-pane');
      tabPanels.forEach(panel => {
        panel.setAttribute('tabindex', '-1');
      });
    }

    document.addEventListener('DOMContentLoaded', function() {
      updateTabIndex(); // Initial setting on page load

      // Listen for tab change events and update tabindex
      const tabLinks = document.querySelectorAll('a[data-toggle=\"tab\"]');
      tabLinks.forEach(link => {
        link.addEventListener('shown.bs.tab', function() {
          updateTabIndex(); // Reset tabindex after each tab switch
        });
      });
    });

    if (currentLang === 'en') {
      $('#download_aqbat_appendix_en').show();
      $('#download_aqbat_appendix_fr').hide();
    } else if (currentLang === 'fr') {
        $('#download_aqbat_appendix_fr').show();
        $('#download_aqbat_appendix_en').hide();
    }
});