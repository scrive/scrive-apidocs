/*
 * This module uses new HTML5 history api to show correct urls in location bar but not reload whole pages.
 *
 * How to use this:
 *
 *  <html>
 *   <body>
 *     <div id="main-part">
 *     <a href="/normal/link" ajaxrel="#main-part>Click me</a>
 *     </div>
 *   </body>
 *  </html>
 *
 * How this works:
 * 1. checks if browser supports all needed features
 * 2. clicking on a link with ajaxrel attribute load full page in background ajax
 * 3. received page is searched for elements that match ajaxrel value (as jQuery selector)
 * 4. in current page element that matches selector is replaced with the one found in response
 * 5. location bar is properly updated
 * 6. history is handled properly (clicking back/forward just reloads everything)
 *
 * Fixes to be done
 * - browsers are really crappy at handling xml
 * - browsers are really crappy at implementing HTML5 history api correctly
 * - if reloaded page has different scripts those will not be executed
 *
 */

 
function xml2string(node) {
   if (typeof(XMLSerializer) !== 'undefined') {
      var serializer = new XMLSerializer();
      return serializer.serializeToString(node);
   } else if (node.xml) {
      return node.xml;
   }
}

function hasOverrideMimeType() {
    var req = new XMLHttpRequest();
    return req.overrideMimeType !== undefined;
}

$(function () {
    /* 
     * We should not be doing this if there is no chance for his to work
     */
    
    if( typeof(XMLSerializer) !== 'undefined' &&
        hasOverrideMimeType() &&
        !!(window.history && history.pushState)) {

        $('a[ajaxrel]').live("click", function() {
            var href = $(this).attr("href");
            var rel = $(this).attr("ajaxrel");
            console.log("JavaScript history going to " + href + " using " + rel );
            var req = new XMLHttpRequest();
            req.overrideMimeType("text/xml");
            req.open("GET", href, false);
            req.send(null);
            var xml = req.responseXML;
            if ( !xml ) {
                var parser = new DOMParser();
                xml = parser.parseFromString(req.responseText, "text/xml");
            }
            var foundNode = $(xml).find(rel)[0];
            if( foundNode ) {
                var serializer = new XMLSerializer();
                var strhtml = serializer.serializeToString(foundNode);
                $(rel).replaceWith(strhtml);
                history.pushState("zonk", null, href);
                return false;
            }
            else {
                console.log("Ajaxrel did not work properly");
                return true; // means: reload this page once again
            }
        });
        $(window).bind("popstate", function(event) {
            if( event.originalEvent.state==="zonk" ) {
                window.location.href = window.location.href;
            }
        });
    }
});
