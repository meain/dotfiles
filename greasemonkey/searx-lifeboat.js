// ==UserScript==
// @name     searx-lifeboat
// @version  1
// @grant    none
// @match    https://searx.lukesmith.xyz/*
// ==/UserScript==

const q = document.getElementById('q')
const a = document.createElement('a')
a.href = `https://google.com/search?q=${q.value}`
a.textContent = '░'
a.setAttribute('style', 'display:flex;justify-content:center;align-items:center;margin:5px;cursor:pointer;opacity:0.5;')
q.parentElement.appendChild(a)