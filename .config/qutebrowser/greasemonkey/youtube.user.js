// ==UserScript==
// @name            Youtube Fixer
// @version         0.1
// @license         CC0-1
// @match           *://*.youtube.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

let MutationObserver =
  window.MutationObserver ||
  window.WebKitMutationObserver ||
  window.MozMutationObserver;
let observer = new MutationObserver((e) => {
  // clean links
  let results = document.querySelectorAll('a[href*="/redirect?"]');
  for (let i = 0; i < results.length; i++) {
    let url = new URL(results[i].href);
    results[i].href = url.searchParams.get("q");
  }
});
observer.observe(document.body, { childList: true, subtree: true });
