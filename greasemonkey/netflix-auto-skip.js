// ==UserScript==
// @name     netflix-auto-skip
// @version  1
// @grant    none
// @match    https://www.netflix.com/watch/*
// ==/UserScript==

setInterval(() => {
  let elements = document.getElementsByClassName(
    "watch-video--skip-content-button"
  );
  if (elements.length > 0) {
    elements[0].click();
  }
}, 1000);
