###
embeddedPage= 
$doctype()$
<html>
<head>
$standardHeaderContents()$


<style type="text/css">
$if(background)$
body, #body-container {
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
}

$endif$


$if(logo)$
.skrivapa-logo {
    background: url("$logoLink$") no-repeat scroll 0 0 transparent;
    margin-left:20px;
   
}
$endif$

</style>
<script type="text/javascript" src="/js/postmessage.js"></script>
<script>
standardDialogMask = $if(overlaybackground)$ "$overlaybackground$" $else$ "#FFFFFF" $endif$;
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
<body>
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
<body ></body>
</html>
####