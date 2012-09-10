###
contractrestartbuttontext=$_contractrestartbuttontext()$
###
offerrestartbuttontext=$_offerrestartbuttontext()$
###
orderrestartbuttontext=$_orderrestartbuttontext()$
###
contractcancelbuttontext=$_contractcancelbuttontext()$
###
offercancelbuttontext=$_offercancelbuttontext()$
###
ordercancelbuttontext=$_ordercancelbuttontext()$
###
contractcancelmodaltitle=$_contractcancelmodaltitle()$
###
offercancelmodaltitle=$_offercancelmodaltitle()$
###
ordercancelmodaltitle=$_ordercancelmodaltitle()$
###
###
contractcancelmodaltext=$_contractcancelmodaltext()$
###
offercancelmodaltext=$_offercancelmodaltext()$
###
ordercancelmodaltext=$_ordercancelmodaltext()$
####
contractrejectbuttontext=$_contractrejectbuttontext()$
####
offerrejectbuttontext=$_offerrejectbuttontext()$
####
orderrejectbuttontext=$_orderrejectbuttontext()$
####
contractsignatorysignmodaltitle=$_contractsignatorysignmodaltitle()$
####
offersignatorysignmodaltitle=$_offersignatorysignmodaltitle()$
####
ordersignatorysignmodaltitle=$_ordersignatorysignmodaltitle()$
####
contractsignatorysignmodalcontent= <p>$_contractsignatorysignmodalcontent()$ </p>
####
contractsignatorysignmodalcontentdesignvieweleg=<p> $_contractsignatorysignmodalcontentdesignvieweleg()$ </p>
####
offersignatorysignmodalcontentdesignvieweleg=<p> $_offersignatorysignmodalcontentdesignvieweleg()$ </p>
####
ordersignatorysignmodalcontentdesignvieweleg=<p> $_offersignatorysignmodalcontentdesignvieweleg()$ </p>
####
contractsignatorysignmodalcontentsignvieweleg=<p>  $_contractsignatorysignmodalcontentsignvieweleg()$ </p>
####
offersignatorysignmodalcontentsignvieweleg=<p>  $_offersignatorysignmodalcontentsignvieweleg()$ </p>
####
ordersignatorysignmodalcontentsignvieweleg=<p>  $_ordersignatorysignmodalcontentsignvieweleg()$ </p>
####
contractsignatorysignmodalcontentauthoronly=<p>  $_contractsignatorysignmodalcontentauthoronly()$ </p>
####
offersignatorysignmodalcontentauthoronly=<p>  $_offersignatorysignmodalcontentauthoronly()$ </p>
####
ordersignatorysignmodalcontentauthoronly=<p>  $_ordersignatorysignmodalcontentauthoronly()$ </p>
####
contractauthorsignlastbuttontext=$_contractauthorsignlastbuttontext()$
####
offerauthorsignlastbuttontext=$_offerauthorsignlastbuttontext()$
####
orderauthorsignlastbuttontext=$_orderauthorsignlastbuttontext()$
####
contractsignbuttontext=$_contractsignbuttontext()$
####
offersignbuttontext=$_offersignbuttontext()$
####
ordersignbuttontext=$_ordersignbuttontext()$
####
contractsignbuttontextauthor=$_contractsignbuttontextauthor()$
####
offersignbuttontextauthor=$_offersignbuttontextauthor()$
####
ordersignbuttontextauthor=$_ordersignbuttontextauthor()$
####
contractsignatorycancelmodaltitle=
          <h2>$_contractsignatorycancelmodaltitleHead()$</h2>
          <p>$_contractsignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>.</p>
####
offersignatorycancelmodaltitle=
          <h2>$_offersignatorycancelmodaltitleHead()$</h2>
          <p>$_offersignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>$_offersignatorycancelmodaltitleBody2()$</p>
####
ordersignatorycancelmodaltitle=
          <h2>$_ordersignatorycancelmodaltitleHead()$</h2>
          <p>$_ordersignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>$_ordersignatorycancelmodaltitleBody2()$</p>
####
contractsignguardwarntext=$_contractsignguardwarntext()$
####
offersignguardwarntext=$_offersignguardwarntext()$
######
ordersignguardwarntext=$_ordersignguardwarntext()$
######
###### PART INFO IN DESIGN VIEW
######
contractauthorname=$_contractauthorname()$
#####
contractauthorsignatoryname=$_contractauthorsignatoryname()$
#####
contractsignatoryname=$_contractsignatoryname()$
#####
contractnonsignatoryname=$_contractnonsignatoryname()$
######
offerauthorname=$_offerauthorname()$
#####
offerauthorsignatoryname=$_offerauthorsignatoryname()$
#####
offersignatoryname=$_offersignatoryname()$
#####
offernonsignatoryname=$_offernonsignatoryname()$
######
orderauthorname=$_orderauthorname()$
#####
orderauthorsignatoryname=$_orderauthorsignatoryname()$
#####
ordersignatoryname=$_ordersignatoryname()$
#####
ordernonsignatoryname=$_ordernonsignatoryname()$
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
pageDocumentView=
<script>
var docview;
jQuery(function(){
docview = KontraStandardDocument.init({
    id:"$documentid$"
     $if(siglinkid)$
    , viewer : new DocumentViewer({
        signatoryid : "$siglinkid$",
        magichash : "$sigmagichash$"
        })
    $elseif(authorcompanyadmin)$
    , viewer : new DocumentViewer({
        authorcompanyadmin : true
        })
    $endif$
});
jQuery(".body-container").replaceWith(docview.view.el)
})
</script>
####
