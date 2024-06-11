// ==UserScript==
// @name            Scrollbar Fix
// @version         0.1
// @license         CC0-1
// @match           *://*/*
// @exclude         *://discord.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

let MutationObserver =
  window.MutationObserver ||
  window.WebKitMutationObserver ||
  window.MozMutationObserver;
let observer = new MutationObserver((e) => {
  // default scrollbars
  [].forEach.call(document.styleSheets, function (sheet) {
    try {
      for (var i = 0; i < sheet.rules.length; ++i) {
        var rule = sheet.rules[i];
        if (/::-webkit-scrollbar/.test(rule.selectorText)) {
          sheet.deleteRule(i--);
        }
      }
    } catch {
      /* ignore */
    }
  });
});
observer.observe(document.body, { childList: true, subtree: true });
