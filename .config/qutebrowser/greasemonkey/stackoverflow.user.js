// ==UserScript==
// @name            StackOverflow Cleaner
// @version         0.1
// @license         CC0-1
// @match           https://stackoverflow.com/*
// @match           https://*.stackexchange.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

list = [
  "#announcement-banner",
  ".bottom-notice",
  "#custom-header",
  "#footer",
  ".js-add-link",
  ".js-post-menu",
  ".js-top-bar",
  "#left-sidebar",
  "#left-sidebar",
  "#notify-container",
  "#one-tap-container",
  "#post-form",
  "#sidebar",
  "#signup-dialog-container",
  "#signup-modal-container",
  ".site-header",
  ".votecell",
];

for (selector in list) {
  elements = document.querySelectorAll(list[selector]);
  for (element in elements) {
    try {
      elements[element].remove();
    } catch {
      // do nothing
    }
  }
}
