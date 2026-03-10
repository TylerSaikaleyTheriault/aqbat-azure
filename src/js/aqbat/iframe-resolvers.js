const allowedOrigins = {
  'http://127.0.0.1:6865': true,
  'http://127.0.0.1:5500': true,
  'https://aqbat.netlify.app': true,
  'https://production.example.com': true,
  'https://infobase-dev.com/aqbat/': true,
};

// window.addEventListener('message', function(event) {
//     if(allowedOrigins[event.origin]) {
//         return false;
//     } else {
//         console.warn('Message from unauthorized origin:', event.origin);
//     }
// });

// $(document).ready(function() {
//     const iframe = document.getElementById('shinyIframe');
//     iframe.contentWindow.postMessage(message, '*');
// });

window.addEventListener('message', function (event) {
  // Process the message based on its type
  const data = event.data;

  if (data.type === 'scroll') {
    if (typeof data.top === 'number') {
      const iframe = document.getElementById('shinyIframe');
      if (iframe) {
        // Get the iframe's position relative to the parent window
        const iframeRect = iframe.getBoundingClientRect();
        const parentScrollTop = window.scrollY || document.documentElement.scrollTop;

        // Calculate the absolute top position in the parent window
        // iframeRect.top is relative to the viewport, so add current scroll to get absolute
        const iframeAbsoluteTop = iframeRect.top + parentScrollTop;

        // Final scroll position: iframe top + relative position of modal inside iframe
        // We subtract a small offset (e.g., 100px) so the modal isn't flush against the top
        const absoluteTargetTop = iframeAbsoluteTop + data.top - 100;

        window.scrollTo({
          top: Math.max(0, absoluteTargetTop),
          behavior: 'smooth'
        });
      }
    }
  }
  // else if (data.type === 'store-value') {
  //     // Update the parent window's URL
  //     sessionStorage.setItem(data.key, data.value);
  // }
  // else if (data.type === 'changeTab') {
  //     // Update the parent window's URL
  //     var url = new URL(window.location.href);
  //     url.searchParams.set('tab', data.tab);
  //     window.history.pushState({}, '', url);
  // } else {
  //     // console.warn('Unknown message type:', data.type);
  //     return false;
  // }
});
