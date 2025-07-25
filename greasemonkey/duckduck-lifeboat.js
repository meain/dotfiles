// ==UserScript==
// @name     Alternate search in DDG
// @version  1
// @grant    none
// ==/UserScript==

// Inject via gresemonkey to add other search engine links in DDG
// search. This is useful for quickly switching between search engines
// or even asking tools like ChatGPT or Perplexity.

const ss = {
    Google: "https://www.google.com/search?q={}",
    Perplexity: "https://www.perplexity.ai/?q={}",
    ChatGPT: "https://chatgpt.com/?q={}&hints=search",
    Bing: "https://www.bing.com/search?q={}",
    // DuckDuckGo: "https://duckduckgo.com/?q={}", // already on it
    Yahoo: "https://search.yahoo.com/search?p={}",
    // Ask: "https://www.ask.com/web?q={}",
    // AOL: "https://search.aol.com/aol/search?q={}",
    Yandex: "https://yandex.com/search/?text={}",
    // Baidu: "https://www.baidu.com/s?wd={}",
};

document.addEventListener("DOMContentLoaded", function() {
    // Timeout as sometime the page load still seems to be missing search_form_input  
    setTimeout(() => {
        const sinput = document.getElementById("search_form_input");

        const alternates = document.createElement("div");
        alternates.style.position = "fixed";
        alternates.style.top = "50%";
        alternates.style.right = "0";
        alternates.style.background = "linear-gradient(to left, rgba(241, 241, 241, 0.698), rgba(241, 241, 241, 0))";
        alternates.style.transform = "translateY(-50%)";
        alternates.style.display = "flex";
        alternates.style.flexDirection = "column";
        alternates.style.alignItems = "flex-end";
        alternates.style.marginRight = "10px";
        alternates.style.padding = "10px 10px 10px 5px";
        alternates.style.borderRight = "3px solid lightgray";

        for (let key of Object.keys(ss)) {
            const link = document.createElement("a");
            link.style.margin = "2px";
            link.href = ss[key].replace("{}", sinput.value);
            link.innerText = key;
            alternates.appendChild(link);
        }

        document.body.appendChild(alternates);
    }, 2000);
});
