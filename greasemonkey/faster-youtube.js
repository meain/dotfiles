// ==UserScript==
// @name     faster-youtube
// @version  1
// @grant    none
// @match    https://youtube.com/*
// ==/UserScript==

let lastUpdated = ""
setInterval(() => {
  if (lastUpdated != window.location.toString()) {
    setTimeout(() => {
      document.getElementsByTagName("video")[0].playbackRate = 2;
      lastUpdated = window.location.toString()
    }, 2000);
  }
}, 2000);
