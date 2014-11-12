// ==UserScript==
// @name Log
// @namespace http://www.amotlpaa.org/gmscripts
// @description Post titles and URLs to a local logging server on request.
// @include *
// @grant GM_registerMenuCommand
// @grant GM_xmlhttpRequest
// ==/UserScript==

GM_registerMenuCommand(
    "Log this page",
    function () {
        GM_xmlhttpRequest({
            method: "POST",
            url: "http://localhost:3042/",
            data: "* [" + content.document.title + "](" + content.document.location + ")\r\n"
        })
    },
    "l")
