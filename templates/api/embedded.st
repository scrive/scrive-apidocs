###
embeddedPage= 
$doctype()$
<html>
<head>
$standardHeaderContents()$
<script type="text/javascript" src="/js/postmessage.js"></script>
<script>
standardDialogMask = "#FFFFFF";
standardDialogTop = 100;
whitelabel = true;    
var bodyHeight = 0;
function propagateHeight() {
var h = jQuery("body").height();
if (h == null) h= 400;
if (bodyHeight != h) {
jQuery.pm({
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
<body style="background:none">
<div class="mainContainer" style="margin-top:0;">
<div class="mainContainer960">
 $flashmessages(whitelabel="true")$
 $loginModal()$
  <div id="body-container">
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
<script type="text/javascript" src="/js/postmessage.js"></script>
<script>
function propagateError() {
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
<body style="background:none"></body>
</html>
####