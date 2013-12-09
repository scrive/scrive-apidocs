######
pageDocumentView=
<div class="mainContainer authorview" >
<div class="body-container">
<div class='place-for-authorview'>
</div>
<script>
jQuery(function(){
  if (AuthorViewFirstTime.isFromFTUE()) {
    jQuery(".place-for-authorview").replaceWith(AuthorViewFirstTime.fromFTUEContent());
  } else {
    var authorView = new AuthorView({
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
    jQuery(".place-for-authorview").replaceWith(authorView.el());
  }
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
