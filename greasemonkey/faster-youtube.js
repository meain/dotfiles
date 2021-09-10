// ==UserScript==
// @name     faster-youtube
// @version  1
// @grant    none
// @match    https://youtube.com/*
// ==/UserScript==

setTimeout(() => {
  document.getElementsByTagName("video")[0].playbackRate = 2;
}, 100);
