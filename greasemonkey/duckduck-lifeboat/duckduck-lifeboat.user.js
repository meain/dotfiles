// ==UserScript==
// @name     duckduck-lifeboat
// @version  1
// @grant    none
// @match    https://duckduckgo.com/*
// ==/UserScript==

he = document.getElementsByClassName("header__search-wrap")[0];
sb = document.createElement("a");
tex = document.createTextNode("useGoogle()");
sb.setAttribute('style', 'margin-left:10px; color: #ccc;cursor:pointer;')
sb.appendChild(tex);
he.appendChild(sb);
sb.onclick = () => {
  const searchTerm = document.getElementById("search_form_input").value;
  if (searchTerm)
    window.location = `https://www.google.com/search?q=${searchTerm}`;
};
