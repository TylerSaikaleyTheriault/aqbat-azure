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
      const currentTop = window.scrollY || document.documentElement.scrollTop;
      const scrollThreshold = 500;
      if (Math.abs(data.top - currentTop) > scrollThreshold) {
        // Scroll the parent window to the received position
        window.scrollTo({
          top: data.top,
          behavior: 'smooth', // or 'auto' for instant scroll
        });
      } else {
        return false;
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
