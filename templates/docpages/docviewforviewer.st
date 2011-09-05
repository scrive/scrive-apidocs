####
pageDocumentForViewerHelpers=
<span class="templating_insert"> 
  <div id="signatory_template" class="signatorybox" alt="$_pageDocumentForViewerHelpersPartyName()$">
  <input name="signatoryname" type="text" value="" autocomplete="off"
         infotext="$_signatoryname()$" /><br/>
  <input name="signatorycompany" type="text" value="" autocomplete="off"
         infotext="$_company()$" /><br/>
  <input name="signatorypersonalnumber" type="text" value="" autocomplete="off"
         infotext="$_personnumber()$" /><br/>
  <input name="signatorycompanynumber" type="text" value="" autocomplete="off"
         infotext="$_orgnumber()$" /><br/>
  <input name="signatoryemail"  type="email" value="" autocomplete="off"
         infotext="$_email()$" /><br/>
  <small><a onclick="return signatoryremove(this.parentNode);" href="#">$_signatoryremove()$</a></small>
  </div>
</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$";
  $if(siglinkid)$
  var siglinkid = "$siglinkid$", sigmagichash = "$sigmagichash$";
  $endif$
</script>
###
pageDocumentForViewerContent=
<script>
jQuery(function(){
var docview = KontraStandardDocument.init({
    id:"$documentid$"
    $if(siglinkid)$
    , viewer : new DocumentViewer({
        signatoryid : "$siglinkid$",
        magichash : "$sigmagichash$"
        })
    $endif$
    });
jQuery("#body-container").replaceWith(docview.view.el)
})
</script>


####
