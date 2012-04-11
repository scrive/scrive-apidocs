####
pageDocumentSignView=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body class="signview">
    <div class="mainContainer960" >
      <div class="body-container">
        <script>
            
            jQuery(function(){
                var docview = KontraSignDocument.init({
                    id:"$documentid$"
                    $if(siglinkid)$
                    , viewer : new DocumentViewer({
                        signatoryid : "$siglinkid$",
                        magichash : "$sigmagichash$"
                       })
                    $endif$
                });
            jQuery('body').prepend(new DocumentSignViewHeader({model : docview.view.saveAfterSignModel}).el)
            jQuery('body').append(new DocumentSignViewFooter({model : docview.view.saveAfterSignModel}).el)
            jQuery(".body-container").replaceWith(docview.view.el);
            })
       </script>
      </div>
    </div>
  </body>
</html>
####