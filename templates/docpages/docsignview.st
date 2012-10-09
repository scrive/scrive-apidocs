####
pageDocumentSignView=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$

    <script>
        var inService = $if(isservice)$true$else$false$endif$;
    </script>
      </head>
  <body class="signview">
    <div class="mainContainer960" >
      $flashmessages()$
      <div class="body-container">
        <script>
            
            jQuery(function(){
                var docview = new DocumentSignView({
                    id:"$documentid$"
                    $if(siglinkid)$
                    , viewer : new DocumentViewer({
                        signatoryid : "$siglinkid$"
                       })
                    $endif$
                });
            jQuery('body').prepend(new DocumentSignViewHeader({model : docview.model, mainview : docview.view}).el)
            jQuery('body').append(new DocumentSignViewFooter({model : docview.model,  mainview : docview.view}).el)
            jQuery(".body-container").replaceWith(docview.view.el);
            })
       </script>
      </div>
    </div>
  </body>
</html>
####
