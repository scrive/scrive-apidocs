###### PART INFO IN DESIGN VIEW
######
pageDocumentView=
<script>
var docview;
jQuery(function(){
  docview = new AuthorView({
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
  jQuery(".body-container").replaceWith(docview.el());
  jQuery('.body-container').prepend('<div class="blocking-box" />');

  BlockingInfo.show('.blocking-box');
});
</script>
####
