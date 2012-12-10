######
pageDocumentView=
<div class="mainContainer authorview" >
<div class="body-container">
<script>
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
</div>
</div>

####
