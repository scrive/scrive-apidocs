####
pageDocumentSignView=
$doctype()$
$htmlopentag()$
  <head>
    <script src="//cdn.optimizely.com/js/158965942.js"></script>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <script src="$homebase$/js/docsignviewcreateaccount.js"></script>
      </head>
  <body  $if(usestandardheaders)$ $if(custombackground)$style="background:$custombackground$;"$endif$    $endif$ >
    $if(usestandardheaders)$
    $header()$
    <div class="wrapper-position-footer">
    $endif$
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="body-container">
          <script>
              var svb =  new BrandingForSignView({ documentid : "$documentid$" , signatoryid : "$siglinkid$" });
              svb.fetch({ processData:  true, cache : false});

              jQuery(function(){
                  var docview = new DocumentSignView({
                      id:"$documentid$",
                      signviewbranding : svb
                      $if(usestandardheaders)$
                      ,  ignoredoclang : true
                      ,  usebranding : false
                      $else$
                      ,  usebranding : true
                      $endif$
                      , viewer : new DocumentViewer({
                          signatoryid : "$siglinkid$"
                         })
                  });
                $if(!usestandardheaders)$
                  jQuery('.signview').prepend(new DocumentSignViewHeader({model : svb}).el);
                  jQuery('.signview').append(new DocumentSignViewFooter({model : svb }).el);
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
    </div>
    $endif$
  </body>
</html>
####
