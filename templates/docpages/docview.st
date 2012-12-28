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
mixpanel.register({DocumentID : "$documentid$"
                   $if(siglinkid)$
                   ,'Signatory ID' : "$siglinkid$"
                   $elseif(authorcompanyadmin)$
                   ,'Admin of author?' : true
                   $endif$
                  });
  mixpanel.track('View Author View');



</script>
</div>
</div>

####
