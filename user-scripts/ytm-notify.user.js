// ==UserScript==
// @name        YTM Notify
// @description Send a notification when the track changes
// @author      Christopher Bayliss
// @version     1.0
// @match       https://music.youtube.com/*
// @grant       none
// ==/UserScript==

const title = document.querySelector(
  ".middle-controls .content-info-wrapper .title"
);

new MutationObserver(() => {
  new Notification(title.innerText, {
    icon: document.querySelector(".middle-controls img").getAttribute("src"),
    body: document.querySelector(
      ".middle-controls .content-info-wrapper .byline .yt-simple-endpoint"
    ).innerText,
  });
}).observe(title, { attributes: true, characterData: true, childList: true });
