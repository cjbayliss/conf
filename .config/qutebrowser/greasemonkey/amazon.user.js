// ==UserScript==
// @name            Amazon Fixer
// @version         0.1
// @license         CC0-1
// @match           *://*.amazon.com/*
// @match           *://*.amazon.com.au/*
// @grant           none
// @qute-js-world   user
// ==/UserScript==

let MutationObserver =
  window.MutationObserver ||
  window.WebKitMutationObserver ||
  window.MozMutationObserver;
let observer = new MutationObserver((e) => {
  // remove ads
  for (const el of document.querySelectorAll(
    'div[data-component-props*="SponsoredProductsEventTracking"',
  )) {
    el.parentElement.remove();
  }
  for (const el of document.querySelectorAll('a[aria-label^="Sponsored ad"]')) {
    el.parentElement.remove();
  }
  for (const el of document.querySelectorAll('a[aria-labelledby="ad"]')) {
    el.parentElement.remove();
  }
  for (const el of document.querySelectorAll('iframe[title="Sponsored ad"]')) {
    el.remove();
  }
  for (const el of document.querySelectorAll('div[class*="sbv-ad-content-container"]')) {
    el.parentElement.parentElement.remove();
  }
  for (const el of document.querySelectorAll(
    'span[data-component-type="sbv-video-single-product"]',
  )) {
    el.parentElement.remove();
  }

  // clean links
  results = document.querySelectorAll('a[href*="/redirect.html"]');
  for (let i = 0; i < results.length; i++) {
    let url = new URL(results[i].href);
    results[i].href = url.searchParams.get("location");
  }
  results = document.querySelectorAll('a[href*="/sspa/"]');
  for (let i = 0; i < results.length; i++) {
    let url = new URL(results[i].href);
    results[i].href = url.origin + url.searchParams.get("url");
  }
  results = document.querySelectorAll('a[href*="/dp/"]');
  for (let i = 0; i < results.length; i++) {
    let url = new URL(results[i].href);
    try {
      results[i].href =
        url.origin + url.pathname.match(/.*?(\/dp\/.*?\/).*/)[1];
    } catch {}
  }
  results = document.querySelectorAll('a[href*="/product-reviews/"]');
  for (let i = 0; i < results.length; i++) {
    let url = new URL(results[i].href);
    try {
      results[i].href =
        url.origin + url.pathname.match(/.*?(\/product-reviews\/.*?\/).*/)[1];
    } catch {}
  }
});
observer.observe(document.body, { childList: true, subtree: true });
