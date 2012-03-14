###
# August 21, 2011
# Eric Normand
# Have to set style="overflow:hidden" on html tag to prevent safari from drawing
# unnecessary scrollbars on iframe
# Actually, I removed the overflow:hidden because we don't distinguish
# between embedded and not embedded.
###
embeddedPage= 
$doctype()$
<html>
<head>
$standardHeaderContents()$


<style type="text/css">
$if(background)$
body, #body-container, .body-container {
    background: $background$;
}
$endif$

$if(barsbackground)$
.modal-container .modal-header,
 #signStepsTitleRow,
 #signStepsMenu div.current 
 {
   background: $barsbackground$;
   border-color: $barsbackground$;
}

.tab-viewer .tabs li.active 
 {
   background : $barsbackground$;
}


$endif$

$if(buttons)$
a.btn-big .left , a.btn-small .left , a.btn-tiny .left {
    background-image: url($buttonRestLink$);
}

a.btn-big .right , a.btn-small .right , a.btn-tiny .right {
    background-image: url($buttonRestLink$);
}

a.btn-big .label, a.btn-small .label, a.btn-tiny .label {
    background: url($buttonBodyLink$) 0 0;
}
a.btn-big.green .label, a.btn-small.green .label, a.btn-tiny.green .label,
a.btn-big.red .label, a.btn-small.red .label, a.btn-tiny.red .label,
a.btn-big.blue .label, a.btn-small.blue .label, a.btn-tiny.blue .label,
a.btn-big.black .label, a.btn-small.black .label, a.btn-tiny.black .label
a.btn-big.green .label:hover, a.btn-small.green .label:hover, a.btn-tiny.green .label:hover,
a.btn-big.red .label:hover, a.btn-small.red .label:hover, a.btn-tiny.red .label:hover,
a.btn-big.blue .label:hover, a.btn-small.blue .label:hover, a.btn-tiny.blue .label:hover,
a.btn-big.black .label:hover, a.btn-small.black .label:hover, a.btn-tiny.black .label:hover,
a.btn-big.green .label:active, a.btn-small.green .label:active, a.btn-tiny.green .label:active,
a.btn-big.red .label:active, a.btn-small.red .label:active, a.btn-tiny.red .label:active,
a.btn-big.blue .label:active, a.btn-small.blue .label:active, a.btn-tiny.blue .label:active,
a.btn-big.black .label:active, a.btn-small.black .label:active, a.btn-tiny.black .label:active
{
    text-shadow: none;
}

a.btn-big .btn-symbol, a.btn-small .btn-symbol, a.btn-tiny .btn-symbol{
    display: none;
}

$endif$

$if(buttonstextcolor)$

a.btn-small .label, a.btn-big .label, a.btn-tiny .label  {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}
a.btn-small.red .label, a.btn-big.red .label, a.btn-tiny.red .label  {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}
a.btn-small.green .label, a.btn-big.green .label, a.btn-tiny.green .label  {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}
a.btn-small.blue .label, a.btn-big.blue .label, a.btn-tiny.blue .label  {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}
a.btn-small.black .label, a.btn-big.black .label, a.btn-tiny.black .label      {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}

a.btn-small p, a.btn-big p, a.btn-tiny p  {
    color: $buttonstextcolor$ !important;
    text-shadow: none !important;
}

$endif$


$if(logo)$
.skrivapa-logo {
    background: url("$logoLink$") no-repeat scroll 0 0 transparent;
    margin-left:20px;
}  
.scrive-logo {
    background: url("$logoLink$") no-repeat scroll 0 0 transparent;
    margin-left:20px;
}
$else$
.skrivapa-logo {
    display:none;
}
.scrive-logo {
    display:none;
}
$endif$

.flashmsgbox {
  position:relative;
}

.signStepsBodyUploadBox {
 margin-left: auto;
 margin-right: auto;
 float: none;
}

.flashmsgbox {
  position:relative;
}

.signStepsBodyUploadBox {
 margin-left: auto;
 margin-right: auto;
 float: none;
}
</style>
<script type="text/javascript" src="/js/postmessage.js"></script>
<script>
standardDialogMask = $if(overlaybackground)$ "$overlaybackground$" $else$ "#FFFFFF" $endif$;
standardDialogTop = 100;
whitelabel = true;    
var bodyHeight = 0;
function propagateHeight() {
var h = jQuery("body").height();
if (h == undefined) h= 400;
if (bodyHeight != h) {
if (! ("$location$" == "" && jQuery.browser.msie &&  parseInt(jQuery.browser.version, 10)  < 8))
pm({
  target: window.parent,
  url: "$location$",
  type:"height", 
  data: h
});

bodyHeight = h;
}
setTimeout(propagateHeight,1000);
 }
jQuery(function(){propagateHeight();})


</script>
</head>
<body class="jsintegrationapi">
<div class="mainContainer" style="margin-top:0;">
<div class="mainContainer960">
$flashmessages(whitelabel="true")$

 $loginModal()$
  <div class="body-container">
 $content:noescape()$
  </div>
</div>
</div>
</body>
</html>
###
embeddedErrorPage= 
$doctype()$
<html>
<head>
$standardHeaderContents()$
<script type="text/javascript" src="/js/postmessage.js"></script>
<script>
function propagateError() {
if (! ("$location$" == "" && jQuery.browser.msie &&  parseInt(jQuery.browser.version, 10)  < 8))
pm({
  target: window.parent,
  url: "$location$",
  type:"error", 
  data: "error"
});
setTimeout(propagateError,10000);
}
propagateError();
</script>
</head>
<body ></body>
</html>
####
# August 21, 2011
# Eric Normand
# Safari does not store cookies set in a page in an iframe on a domain not yet visited. This is to 
# maintain privacy from ads that use an iframe and go to a different site. 
# Note: This is not that the iframe cookies are inaccessible outside of the iframe, which is the
# default behavior of most browsers. Safari requires a "user action" to initiate cookie storage.
# Luckily, javascript submitting a form counts as "user action" (probably a bug in Safari, but it
# works for now:). Apparently it has to be a post.
# Here's what we do (and it did work at the time of this writing):
# When a service requests a connectuser link with GET, we send them this page, which gets embedded
# in the iframe.
# This page creates its own iframe way off the screen (top: -100000px).
# It also creates a form with a hidden referer input.
# The js submits the form with the target set as the iframe.
# The browser loads the POST (which stores the cookie!).
# Then we change the target and repost to the same URL, changing the entire iframe.
# It requires three extra requests over the simple GET version (a GET, a POST + 303 redirect, and a final GET).
# But it loads the same page twice,
# so most of the resources should already be loaded in the cache.
connectredirect=
<html>
  <head>
    $standardHeaderContents()$
  </head>
  <body>
    <div id="cookieframe" style="position: absolute; top: -10000px"></div>
    <script type="text/javascript">
      jQuery(function() {
      var a = jQuery('<form method="post" action="$url$" target="yoyo"></form>');
      var i = jQuery('<iframe name="yoyo" src="javascript:false">');
      
      jQuery("#cookieframe").append(i);
      jQuery("#cookieframe").append(a);
      a.submit();
      a.attr("target", "");
      a.attr("action", "$referer$");
      a.attr("method", "GET");
      i.load(function() {
      a.submit();
      });
      });
    </script>
  </body>
</html>
###

