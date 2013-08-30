######
pageDocumentView=
<div class="mainContainer authorview" >
<div class="body-container">
<script>
jQuery(function(){
  window.authorView = new AuthorView({
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
  jQuery(".body-container").replaceWith(window.authorView.el());
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
