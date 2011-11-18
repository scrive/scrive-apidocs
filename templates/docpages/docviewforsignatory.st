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
contractsignatorysignmodalcontentlast= <p>$_contractsignatorysignmodalcontentlast()$ </p>
####
offersignatorysignmodalcontentlast=<p> $_offersignatorysignmodalcontentlast()$ </p>
####
ordersignatorysignmodalcontentlast=<p> $_ordersignatorysignmodalcontentlast()$ </p>      
####
contractsignatorysignmodalcontentnotlast=<p> $_contractsignatorysignmodalcontentnotlast()$ </p>
####
offersignatorysignmodalcontentnotlast=<p> $_offersignatorysignmodalcontentnotlast()$ </p>
####
ordersignatorysignmodalcontentnotlast= <p> $_ordersignatorysignmodalcontentnotlast()$ </p>         
####
contractsignatorysignmodalcontentauthorlast=<p> $_contractsignatorysignmodalcontentauthorlast()$ </p>
####
offersignatorysignmodalcontentauthorlast=<p> $_offersignatorysignmodalcontentauthorlast()$ </p>
####
ordersignatorysignmodalcontentauthorlast= <p> $_ordersignatorysignmodalcontentauthorlast()$ </p>  
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
pageDocumentForSignContent=
<script>
jQuery(function(){
var docview = KontraStandardDocument.init({
	id:"$documentid$",
	viewer : new DocumentViewer({
				signatoryid : "$siglinkid$",
				magichash : "$sigmagichash$"
			})
	});
jQuery(".body-container").replaceWith(docview.view.el)
})
</script>
#####
