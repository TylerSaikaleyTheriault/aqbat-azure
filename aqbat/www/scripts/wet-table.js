// tables examples: https://wet-boew.github.io/v4.0-ci/demos/tables/tables-en.html
// tables docs: https://wet-boew.github.io/wet-boew-styleguide/design/tables-en.html
// tables plugins: https://wet-boew.github.io/v4.0-ci/docs/ref/tables/tables-en.html

// WET Tables integration with R Shiny
$(document).ready(function() {
  // Normalize French/European numbers so they sort numerically: space/nbsp = thousands, comma = decimal.
  function normalizeFormattedNum(s) {
    if (s === '-' || s === '' || s == null) return (s == null ? '' : s);
    s = (s + '').replace(/[\s\u00A0]/g, '');  // remove spaces and non-breaking space
    // French: comma before final digits is decimal (e.g. "1234,5"); English thousands comma unchanged
    return s.replace(/,(?=\d+$)/, '.');
  }

  function applyFrenchNumSortPatch() {
    if (typeof wb === 'undefined' || !wb.formattedNumCompare) return;
    var orig = wb.formattedNumCompare;
    if (orig._frenchNumOrig) return;  // already patched with our wrapper
    wb.formattedNumCompare = function(a, b) {
      return orig(normalizeFormattedNum(a), normalizeFormattedNum(b));
    };
    wb.formattedNumCompare._frenchNumOrig = orig;
  }

  applyFrenchNumSortPatch();

  // Listen for Shiny output updates
  $(document).on('shiny:value', function(event) {
    // Delay initialization to ensure the DOM is fully ready
    setTimeout(function() {
      applyFrenchNumSortPatch();
      // Apply WET table classes if not already applied
      $('table:not(.wb-tables)')
        .addClass('wb-tables table table-striped table-hover')
        .attr('data-wb-tables', '{"ordering" : true, "searching" : false, "paging" : false,  "info" : false, "lengthChange" : false}' )

      $( ".wb-tables" ).trigger( "wb-init.wb-tables" );
    }, 0);
  });

  // Listen for the WET tables initialization to complete
  $(document).on("wb-ready.wb-tables", function(event) {
    // Apply custom styles or actions after initialization
    // For example, changing the background color of the table header
    $('.loader-container').hide()
    $(event.target).closest('.shiny-table').css('display', 'table');
  });
});