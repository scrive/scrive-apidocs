###### PART INFO IN DESIGN VIEW
######
pageDocumentView=
<script>
var docview;
jQuery(function(){
docview = KontraStandardDocument.init({
    id:"$documentid$"
     $if(siglinkid)$
    , viewer : new DocumentViewer({
        signatoryid : "$siglinkid$"
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
