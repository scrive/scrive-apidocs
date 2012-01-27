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
###
pageDocumentForAuthor=
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
    $endif$
});
jQuery(".body-container").replaceWith(docview.view.el)
})
</script>

####
