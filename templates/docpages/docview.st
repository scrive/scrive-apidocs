######
pageDocumentView=
<script>
jQuery("body").addClass("authorview");
jQuery(function(){
  jQuery(".body-container").replaceWith(new AuthorView({
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
  }).el());
});
</script>

####
