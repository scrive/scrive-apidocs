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
contractcancelbyauthormodaltitle=
         <h2>$_contractcancelbyauthormodaltitleHead()$</h2>
          <p>$_contractcancelbyauthormodaltitleBody()$</p>
###
offercancelbyauthormodaltitle=
         <h2>$_offercancelbyauthormodaltitleHead()$</h2>
          <p>$_offercancelbyauthormodaltitleBody()$</p>
###
ordercancelbyauthormodaltitle=
         <h2>$_ordercancelbyauthormodaltitleHead()$</h2>
          <p>$_ordercancelbyauthormodaltitleBody()$</p>
###
pageDocumentForAuthor=
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
