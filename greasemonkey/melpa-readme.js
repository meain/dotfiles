// ==UserScript==
// @name     melpa-readme
// @version  1
// @grant    none
// @match    https://melpa.org/*
// ==/UserScript==

console.log("almighty-readme-pull");

function getSourceButton() {
  return new Promise((resolve, reject) => {
    const sourceCodeButtonSelector = "a.btn:nth-child(3)";
    let scButton = null;
    console.log("Waiting for page to load..");
    const timer = setInterval(() => {
      scButton = document.querySelector(sourceCodeButtonSelector);
      if (scButton) {
        clearInterval(timer);
        resolve(scButton);
      }
    }, 100);
  });
}

let lastProcessed = null;
function doThing() {
  getSourceButton().then((scButton) => {
    if (lastProcessed == window.location.href) {
      // to avoid rendering it twice, that seemsed to be happening
      return;
    }
    lastProcessed = window.location.href;

    console.log("Fetching readme...");
    // https://github.com/meain/evil-textobj-tree-sitter/tree/1d22b0a317aaa6ca7d5135eb147322393356e171
    // https://api.github.com/repos/meain/evil-textobj-tree-sitter/git/trees/1d22b0a317aaa6ca7d5135eb147322393356e171
    if (window.location.href.endsWith("/#/")) {
      console.log("Not a package page");
      return;
    }
    let fileTreeUrl = scButton.href
      .replace("/github.com/", "/api.github.com/repos/")
      .replace("/tree/", "/git/trees/");
    fetch(fileTreeUrl).then((response) => {
      response.json().then((fileTree) => {
        let files = fileTree.tree.map((x) => x.path);
        let readmeFile = null;

        for (let file of files) {
          if (file.toLowerCase().split(".")[0] == "readme") {
            // length check useful when there might be readme in multiple languages (readme.ja.md)
            if (readmeFile == null || file.length < readmeFile.length) {
              readmeFile = file;
            }
          }
        }

        // https://github.com/meain/evil-textobj-tree-sitter/tree/1d22b0a317aaa6ca7d5135eb147322393356e171
        // https://api.github.com/repos/meain/evil-textobj-tree-sitter/contents/README.md
        let blobsBase = scButton.href
          .replace("/github.com/", "/api.github.com/repos/")
          .replace(/\/tree\/.*$/, "/contents");
        fetch(blobsBase + "/" + readmeFile).then((response) => {
          response.json().then((response) => {
            let content = atob(response.content);

            let descriptionSelector =
              "#content > section:nth-child(1) > section:nth-child(5)";
            var div = document.querySelector(descriptionSelector),
              clone = div.cloneNode(true); // true means clone all childNodes and all event handlers
            clone.getElementsByTagName("h4")[0].textContent = "Readme";
            clone.getElementsByTagName("pre")[0].textContent = content;
            div.parentNode.insertBefore(clone, div.nextSibling);
          });
        });
      });
    });
  });
}

doThing();
// window.onhashchange = doThing;
// window.addEventListener("popstate", doThing);
// window.addEventListener("pushstate", doThing);
var oldHref = document.location.href;
window.onload = function () {
  var bodyList = document.querySelector("body"),
    observer = new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        if (oldHref != document.location.href) {
          oldHref = document.location.href;
          doThing();
        }
      });
    });

  var config = { childList: true, subtree: true };
  observer.observe(bodyList, config);
};
