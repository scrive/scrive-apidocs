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
    <script src="$homebase$/js/docsignviewcreateaccount.js"></script>
    <script src="$homebase$/js/docsignviewpromotescrive.js"></script>
      </head>
  <body>
    $if(usestandardheaders)$
    $header()$
    $endif$
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="body-container">
          <script>

              jQuery(function(){
                  var docview = new DocumentSignView({
                      id:"$documentid$"
                      $if(usestandardheaders)$
                      ,  ignoredoclang : true
                      ,  usebranding : false
                      $else$
                      ,  usebranding : true
                      $endif$
                      $if(siglinkid)$
                      , viewer : new DocumentViewer({
                          signatoryid : "$siglinkid$"
                         })
                      $endif$
                  });
                $if(!usestandardheaders)$
                  jQuery('.signview').prepend(new DocumentSignViewHeader({model : docview.model, mainview : docview.view}).el)
                  jQuery('.signview').append(new DocumentSignViewFooter({model : docview.model,  mainview : docview.view}).el)
                $endif$
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
    </div>
    $if(usestandardheaders)$
    $pageFooter()$
    $endif$
  </body>
</html>
####
