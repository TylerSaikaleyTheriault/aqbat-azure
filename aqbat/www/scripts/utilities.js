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
    const currentLang = $('#aqbat').attr('lang') || 'en'; // Default to 'en' if not set
    
    // Add this code right here
    $(document).on('wb-ready.wb', function() {
      $('#full-screen').trigger('open.wb-overlay');
    });
    
    // console.log(currentLang);

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

    // CHANGE BROWSE TO UPLOAD
    $('.btn-file').each(function() {
      $(this).contents().first().replaceWith(currentLang === "en" ? 'Upload' : "Télécharger");
      
      // Add a focus event listener to prevent scrolling when focused
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