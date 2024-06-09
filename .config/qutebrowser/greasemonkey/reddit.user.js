// ==UserScript==
// @name            Reddit Cleaner
// @version         0.1
// @license         CC0-1
// @match           *://*.reddit.com/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

document.querySelector('.commentsignupbar').remove();
document.querySelector('.listingsignupbar').remove();
document.querySelector('.premium-banner-outer').remove();
document.querySelector('.sidebox.submit').parentElement.remove();
document.querySelector('.sidebox.submit').parentElement.remove();
