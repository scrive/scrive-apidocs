####
pageDocumentSignView=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body class="signview">
    $signviewheader()$
    <div class="mainContainer960" >
      <div class="body-container">
        $pageDocumentSignViewContents()$
      </div>
    </div>
  </body>
</html>
####
signviewheader=
 <div class="pageheader" $if(companybrand.barsbackground)$style="background-image:none;background-color:$companybrand.barsbackground$;"$else$$if(service.barsbackground)$style="background-image:none;background-color:$service.barsbackground$;"$endif$$endif$>
   <div class="content">
    <div class="logowrapper">
      $if(!service.logo)$
      $if(!companybrand.logo)$
      <a href="$linkhome$"><div class='logo'></div></a>
      $else$
      <div class='logo' style="background-image:url('$ctxhostpart$$companybrand.logoLink$');"></div>
      $endif$
      $else$
      <div class='logo' style="background-image:url('$ctxhostpart$$service.logoLink$');"></div>
      $endif$
    </div>

    <div class="sender"  $if(companybrand.barstextcolour)$style="color:$companybrand.barstextcolour$;"$endif$>
      <div class="name">$author.fullname$</div>
      <div class="phone">$author.phone$</div>
    </div>
    <div class="clearboth"></div>
   </div>

    $if(!service.logo)$
    $if(!companybrand.logo)$
    $else$
    <div class='poweredbyscrive'></div>
    $endif$
    $else$
    <div class='poweredbyscrive'></div>
    $endif$
   <div class="clearboth"></div>
 </div>
####
pageDocumentSignViewContents=
<script>
var docview;
jQuery(function(){
docview = KontraSignDocument.init({
    id:"$documentid$"
     $if(siglinkid)$
    , viewer : new DocumentViewer({
        signatoryid : "$siglinkid$",
        magichash : "$sigmagichash$"
        })
    $endif$
});
jQuery(".body-container").replaceWith(docview.view.el);
})
</script>
####
