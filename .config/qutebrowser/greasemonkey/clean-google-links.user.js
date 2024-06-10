// ==UserScript==
// @name            Google Tracking Cleaner
// @version         0.1
// @license         CC0-1
// @match           *://*.google.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

let results = document.querySelectorAll('a[href^="/url"]');
for (let i = 0; i < results.length; i++) {
  let url = new URL(results[i].href);
  results[i].href = url.searchParams.get("q");
}

for (const span of document.querySelectorAll("span")) {
  if (span.textContent.includes("People also ask")) {
    span.parentNode.parentNode.parentNode.remove();
  }
}
