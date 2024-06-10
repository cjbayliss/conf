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
  // skip adds
  const btn = document.querySelector(
    ".videoAdUiSkipButton,.ytp-ad-skip-button",
  );
  if (btn) {
    btn.click();
  }
  const ad = [...document.querySelectorAll(".ad-showing")][0];
  if (ad) {
    const video = document.querySelector("video");
    video.muted = true;
    video.hidden = true;

    if (video.duration != NaN) {
      video.currentTime = video.duration;
    }

    video.playbackRate = 16;
  }

  // remove homepage ads
  for (const el of document.querySelectorAll(".ytd-ad-slot-renderer")) {
    el.remove();
  }

  // remove video previews on hover
  for (const el of document.querySelectorAll("#video-preview")) {
    el.remove();
  }
  for (const el of document.querySelectorAll("#mouseover-overlay")) {
    el.remove();
  }
});
observer.observe(document.body, { childList: true, subtree: true });
