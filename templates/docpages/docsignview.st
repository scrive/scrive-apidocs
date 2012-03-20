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
 <div class="pageheader $signviewwithlogoclass()$" style="$signviewbarsbackgroundstyle()$$signviewtextcolorstyle()$">
   <div class="content">
    <div class="logowrapper">
      $if(!service.logo)$
      $if(!companybrand.logo)$
      <a href="$linkhome$"><div class='logo'></div></a>
      $else$
      <img class='logo' src="$ctxhostpart$$companybrand.logoLink$"></img>
      $endif$
      $else$
      <img class='logo' src="$ctxhostpart$$service.logoLink$"></img>
      $endif$
    </div>

    <div class="sender">
      <div class="name">$author.fullname$</div>
      <div class="phone">$author.phone$</div>
    </div>
    <div class="clearboth"></div>
   </div>

   <div class='poweredbyscrive'></div>
   <div class="clearboth"></div>
 </div>
####
signviewfooter=
  <div class="pagefooter" style="$signviewbarsbackgroundstyle()$$signviewtextcolorstyle()$">
   <div class="dogtooth" />
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
signviewwithlogoclass=$if(!service.logo)$$if(!companybrand.logo)$withstandardlogo$else$withcustomlogo$endif$$else$withcustomlogo$endif$
####
signviewbarsbackgroundstyle=$if(companybrand.barsbackground)$background-image:none;background-color:$companybrand.barsbackground$;$else$$if(service.barsbackground)$background-image:none;background-color:$service.barsbackground$;$endif$$endif$
####
signviewtextcolorstyle=$if(companybrand.barstextcolour)$color:$companybrand.barstextcolour$;$endif$
####
pageDocumentSignViewContents=
<script>

/** this adds a class to the sender when the screen is small so that we
  * can shove the sender along as the "powered by scrive" banner
  * moves along.  kind of icky.
  */
jQuery(function() {
var updateHeaderSenderPosition = function() {
  var sender = jQuery(".signview .pageheader.withcustomlogo .content .sender");
  if (jQuery(window).width() < 1150) {
    sender.addClass("shoved");
  } else {
    sender.removeClass("shoved");
  }
};

jQuery(window).resize(updateHeaderSenderPosition);
updateHeaderSenderPosition();
});

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
