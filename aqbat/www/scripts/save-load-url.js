$(document).ready(function() {
  // Handle tab clicks
//   const lang = document.documentElement.lang;

//   $('#alltabpanel a[data-toggle="tab"]').on('click', function(e) {
//       // Get the friendly name from the data-value attribute
//       var friendlyName = $(this).data('value');
      
//       // Update the URL parameter if not inside an iframe
//       if (window.self === window.top) {
//           // This means we are not in an iframe
//           let url = new URL(window.location.href);
//           url.searchParams.set('tab', friendlyName);
//           window.history.pushState({}, '', url);
//       } else {
//           // If inside an iframe, send a message to the parent window
//           window.parent.postMessage({
//               type: 'changeTab',
//               tab: friendlyName
//           }, '*');
//       }
//   });

  // Check the URL parameter on load
    // Get the language from the <html> tag
    const lang = document.documentElement.lang;

    // Parse the URL parameters
    const urlParams = new URLSearchParams(window.location.search);
    const cleared = lang === 'fr' ? urlParams.get('efface') : urlParams.get('cleared');

    // Determine if the tab should be switched
    if (cleared === 'true' || cleared === 'vrai') {
        if (lang === 'en') {
            $('#alltabpanel a[data-value="crfs"]').tab('show');
        } else if (lang === 'fr') {
            $('#alltabpanel a[data-value="fcr"]').tab('show');
        }

        // Remove the URL parameter
        urlParams.delete(lang === 'fr' ? 'efface' : 'cleared');

        // Construct the new URL without the removed parameter
        const newUrl = window.location.pathname + (urlParams.toString() ? '?' + urlParams.toString() : '');

        // Update the URL without reloading the page
        window.history.replaceState(null, '', newUrl);
    }
});