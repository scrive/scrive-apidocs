####
pageDocumentSignView=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$

    <script>
        var inService = $if(isservice)$true$else$false$endif$;
    </script>

    $if(isservice)$
<style type="text/css">
$if(background)$
body, #body-container, .body-container {
    background: $background$;
}
$endif$

$if(barsbackground)$
.modal-container .modal-header,
 #signStepsTitleRow,
 #signStepsMenu div.current , 
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

a.btn-big .btn-symbol, a.btn-small .btn-symbol, a.btn-tiny .btn-symbol, .icon.cross{
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
.signview .pageheader div.logo {
    background: url("$logoLink$") no-repeat scroll 0 0 transparent;
    margin-left:20px;
}
$else$

.signview .pageheader div.logo {
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
$endif$
    
  </head>
  <body class="signview">
    <div class="mainContainer960" >
      $flashmessages()$
      <div class="body-container">
        <script>
            
            jQuery(function(){
                var docview = KontraSignDocument.init({
                    id:"$documentid$"
                    $if(siglinkid)$
                    , viewer : new DocumentViewer({
                        signatoryid : "$siglinkid$",
                        magichash : "$sigmagichash$"
                       })
                    $endif$
                });
            jQuery('body').prepend(new DocumentSignViewHeader({model : docview.view.saveAfterSignModel, mainview : docview.view}).el)
            jQuery('body').append(new DocumentSignViewFooter({model : docview.view.saveAfterSignModel,  mainview : docview.view}).el)
            jQuery(".body-container").replaceWith(docview.view.el);
            })
       </script>
      </div>
    </div>
  </body>
</html>
####