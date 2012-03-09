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
    $signviewfooter()$
  </body>
</html>
####
signviewheader=
 <div class="pageheader" style="$signviewbarsbackgroundstyle()$$signviewtextcolorstyle()$">
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

    <div class="sender">
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
signviewfooter=
  <div class="pagefooter" style="$signviewbarsbackgroundstyle()$$signviewtextcolorstyle()$">
    <div class="content">
      <div class="sender">
        <div class="name">$author.fullname$</div>
        <div class='position'>$author.position$</div>
        <div class='company'>$author.company$</div>
        <div class="phone">$author.phone$</div>
        <div class="email">$author.email$</div>
      </div>
      <div class='poweredbyscrive'><a href="$linkhome$" style="$signviewtextcolorstyle()$">$_poweredByScrive()$</a></div>
      <div class="clearboth"></div>
    </div>
  </div>
####
signviewbarsbackgroundstyle=$if(companybrand.barsbackground)$background-image:none;background-color:$companybrand.barsbackground$;$else$$if(service.barsbackground)$background-image:none;background-color:$service.barsbackground$;$endif$$endif$
####
signviewtextcolorstyle=$if(companybrand.barstextcolour)$color:$companybrand.barstextcolour$;$endif$
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
