// ==UserScript==
// @name Log
// @namespace http://www.amotlpaa.org/gmscripts
// @description Post titles and URLs to a local logging server on request.
// @include *
// @grant GM_registerMenuCommand
// @grant GM_xmlhttpRequest
// ==/UserScript==

function alertFail(resp) {
    alert("Error!  " + resp.status + " " + resp.statusText
                     + "\n" + resp.responseText);
}

GM_registerMenuCommand(
    "Log this page",
    function () {
        GM_xmlhttpRequest({
            method: "POST",
            url: "http://localhost:3042/",
            data: "* [" + content.document.title + "](" + content.document.location + ")\r\n",
            onerror: alertFail,
            onload: function (resp) {
                if (resp.status!="200") alertFail(resp);
            }
        })
    },
    "l")
