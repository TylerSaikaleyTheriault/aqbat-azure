// tables examples: https://wet-boew.github.io/v4.0-ci/demos/tables/tables-en.html
// tables docs: https://wet-boew.github.io/wet-boew-styleguide/design/tables-en.html
// tables plugins: https://wet-boew.github.io/v4.0-ci/docs/ref/tables/tables-en.html

// WET Tables integration with R Shiny
$(document).ready(function() {
  // Listen for Shiny output updates
  $(document).on('shiny:value', function(event) {
    // Check if the event is for the specific output
    // if (event.target.id === 'outputbaserate') {
      // Delay initialization to ensure the DOM is fully ready
      setTimeout(function() {
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