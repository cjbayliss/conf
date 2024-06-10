// ==UserScript==
// @name            StackOverflow Cleaner
// @version         0.1
// @license         CC0-1
// @match           https://stackoverflow.com/*
// @match           https://*.stackexchange.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

document.querySelector(".js-top-bar").remove();
document.querySelector("#announcement-banner").remove();
document.querySelector("#left-sidebar").remove();
