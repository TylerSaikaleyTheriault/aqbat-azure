/**
 * Replace Shiny's default English error message with French when in French mode.
 * Used when CSV upload fails (e.g. encoding) and R-side handling cascades.
 */
(function() {
  var EN_MSG = 'An error has occurred. Check your logs or contact the app author for clarification.';
  var FR_MSG = "Erreur lors de la lecture du fichier. Le fichier n'est peut-\u00eatre pas encod\u00e9 en UTF-8 ou contient des caract\u00e8res sp\u00e9ciaux. Veuillez enregistrer votre fichier CSV en encodage UTF-8 et r\u00e9essayer.";

  function isFrench() {
    var el = document.getElementById('aqbat') || document.documentElement;
    var lang = el && el.getAttribute ? el.getAttribute('lang') : null;
    if (lang === 'fr') return true;
    if (typeof window.location !== 'undefined' && window.location.search.indexOf('lang=fr') !== -1) return true;
    if (typeof window.location !== 'undefined' && (window.location.href.indexOf('sante') !== -1 || window.location.href.indexOf('/fr') !== -1)) return true;
    return false;
  }

  function replaceInNode(node) {
    if (node.nodeType === Node.TEXT_NODE) {
      var text = node.textContent || '';
      if (text.trim() === EN_MSG && isFrench()) {
        node.textContent = FR_MSG;
        return true;
      }
    } else if (node.nodeType === Node.ELEMENT_NODE && node.classList && node.classList.contains('shiny-output-error')) {
      if (node.textContent.trim() === EN_MSG && isFrench()) {
        node.textContent = FR_MSG;
        return true;
      }
    }
    if (node.childNodes && node.childNodes.length) {
      for (var i = 0; i < node.childNodes.length; i++) {
        if (replaceInNode(node.childNodes[i])) return true;
      }
    }
    return false;
  }

  function scanAndReplace() {
    if (!isFrench()) return;
    var targets = document.querySelectorAll('.shiny-output-error');
    for (var i = 0; i < targets.length; i++) {
      if (targets[i].textContent.trim() === EN_MSG) {
        targets[i].textContent = FR_MSG;
      }
    }
  }

  function init() {
    scanAndReplace();
    var observer = new MutationObserver(function(mutations) {
      scanAndReplace();
    });
    observer.observe(document.body || document.documentElement, { childList: true, subtree: true });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
