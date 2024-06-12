// ==UserScript==
// @name            Reddit Cleaner
// @version         0.1
// @license         CC0-1
// @match           *://*.reddit.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

for (const el of document.querySelectorAll(".commentsignupbar")) {
  el.remove();
}
for (const el of document.querySelectorAll(".promoted")) {
  el.remove();
}
for (const el of document.querySelectorAll(".listingsignupbar")) {
  el.remove();
}
for (const el of document.querySelectorAll(".premium-banner-outer")) {
  el.remove();
}
for (const el of document.querySelectorAll(".sidebox.submit")) {
  el.parentElement.remove();
}
