// tables examples: https://wet-boew.github.io/v4.0-ci/demos/tables/tables-en.html
// tables docs: https://wet-boew.github.io/wet-boew-styleguide/design/tables-en.html
// tables plugins: https://wet-boew.github.io/v4.0-ci/docs/ref/tables/tables-en.html

// WET Tables integration with R Shiny
$(document).ready(function() {
  // Parse French/European number string to number (space thousands, comma decimal).
  // Used so DataTables sorts numerically instead of by string.
  function parseFrenchNum(s) {
    if (s === '-' || s === '' || s == null) return NaN;
    s = (s + '').replace(/<[^>]*>/g, '').trim();
    s = s.replace(/[\s\u00A0]/g, '');
    s = s.replace(/,(?=\d+$)/, '.');
    var n = parseFloat(s);
    return isNaN(n) ? NaN : n;
  }

  function applyDataTablesFrenchSort() {
    if (typeof $ === 'undefined' || !$.fn.dataTableExt || !$.fn.dataTableExt.type) return;
    var type = $.fn.dataTableExt.type;
    var order = type.order;
    if (!order) return;

    order['formatted-num-asc'] = function(a, b) {
      var na = parseFrenchNum(a), nb = parseFrenchNum(b);
      if (isNaN(na) && isNaN(nb)) return 0;
      if (isNaN(na)) return 1;
      if (isNaN(nb)) return -1;
      return na < nb ? -1 : na > nb ? 1 : 0;
    };
    order['formatted-num-desc'] = function(a, b) {
      var na = parseFrenchNum(a), nb = parseFrenchNum(b);
      if (isNaN(na) && isNaN(nb)) return 0;
      if (isNaN(na)) return 1;
      if (isNaN(nb)) return -1;
      return na > nb ? -1 : na < nb ? 1 : 0;
    };

    // Ensure columns that look like French numbers are detected as formatted-num (WET may detect as string).
    if (type.detect && Array.isArray(type.detect) && type.detect.indexOf(_frenchNumDetector) === -1) {
      type.detect.unshift(_frenchNumDetector);
    }
  }

  var frenchNumRe = /^\s*-?[\d\s\u00A0]+(,[\d]+)?\s*$/;
  function _frenchNumDetector(sData) {
    if (sData == null || sData === '') return null;
    var t = (sData + '').replace(/<[^>]*>/g, '').trim();
    if (frenchNumRe.test(t)) return 'formatted-num';
    return null;
  }

  // Listen for Shiny output updates
  $(document).on('shiny:value', function(event) {
    setTimeout(function() {
      $('table:not(.wb-tables)')
        .addClass('wb-tables table table-striped table-hover')
        .attr('data-wb-tables', '{"ordering" : true, "searching" : false, "paging" : false,  "info" : false, "lengthChange" : false}' )

      $( ".wb-tables" ).trigger( "wb-init.wb-tables" );
    }, 0);
  });

  // When WET table is ready, override DataTables sort so French numbers sort numerically.
  $(document).on("wb-ready.wb-tables", function(event) {
    applyDataTablesFrenchSort();
    var tbl = event.target;
    if (tbl && $.fn.DataTable) {
      try {
        var api = $(tbl).DataTable ? $(tbl).DataTable() : ($(tbl).dataTable && $(tbl).dataTable().api ? $(tbl).dataTable().api() : null);
        if (!api || !api.settings || !api.settings()[0]) throw new Error('no settings');
        {
          var s0 = api.settings()[0];
          var cols = s0.aoColumns, data = s0.aoData;
          for (var c = 0; c < cols.length; c++) {
            var cellVal = '';
            if (data && data[0]) {
              if (data[0]._aSortData && data[0]._aSortData[c] !== undefined) cellVal = data[0]._aSortData[c];
              else if (data[0].anCells && data[0].anCells[c]) cellVal = (data[0].anCells[c].textContent || data[0].anCells[c].innerText || '').trim();
            }
            if ((cellVal + '').replace(/<[^>]*>/g, '').trim().match(frenchNumRe)) cols[c].sType = 'formatted-num';
          }
        }
      } catch (e) {}
    }
    $('.loader-container').hide();
    $(tbl).closest('.shiny-table').css('display', 'table');
  });

  // Fix for NVDA double-reading table headers with DataTables
  $(document).on('draw.dt', function(e) {
    $(e.target).find('th').removeAttr('aria-label');
  });
});
