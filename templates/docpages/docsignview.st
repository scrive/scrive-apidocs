####
pageDocumentSignView=
$doctype()$
<html>
  <head>
    <script src="//cdn.optimizely.com/js/158965942.js"></script>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$

    <script>
        var inService = $if(isservice)$true$else$false$endif$;
    </script>
    <script src="https://scrive.com/js/docsignviewcreateaccount.js"></script>
    <script src="https://scrive.com/js/docsignviewpromotescrive.js"></script>
      </head>
  <body class="signview">
    <div class="mainContainer" >
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
            });

            mixpanel.register({
              Context : 'Sign view',
              DocumentID : "$documentid$",
              'Signatory ID' : $siglinkid$
            });
            mixpanel.people.set({
             'Last document sign view' : new Date()
            });

       </script>
      </div>
    </div>
  </body>
</html>
####
