####
wrapperopentag=
<div class="wrapper-position-footer">
####
wrapperclosingtag=
</div>
########
pageDocumentSignView=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body  $if(usestandardheaders)$ $if(custombackground)$style="background:$custombackground$;"$endif$    $endif$ >
    $if(usestandardheaders)$
    $wrapperopentag()$
    $header()$
    $endif$
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="body-container">
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer',  'legacy_code'], function(jQuery,React,Header,Footer) {
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

                  var headerDiv = jQuery('<div/>');
                  var component = React.renderComponent(
                    Header({
                      signviewbranding: svb,
                        forPad : true
                    }), headerDiv[0]);

                  var footerDiv = jQuery('<div/>');
                  var component = React.renderComponent(
                    Footer({
                      signviewbranding: svb
                    }), footerDiv[0]);
                  jQuery('.signview').prepend(headerDiv);
                  jQuery('.signview').append(footerDiv);

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
           });
         </script>
        </div>
      </div>
    </div>
    $if(usestandardheaders)$
    $pageFooter()$
    $wrapperclosingtag()$
    $endif$
  </body>
$htmlclosingtag$
####
pageDocumentSignPadView=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
    <script>
     require(['Backbone', 'legacy_code'], function(Backbone) {
       // need Backbone to be definied before we load this file
       require(['$homebase$/js/docsignviewcreateaccount.js'], function() {});
     });
    </script>
  </head>
  <body>
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="body-container">
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer',  'legacy_code'], function(jQuery,React,Header,Footer) {
              var svb =  new BrandingForSignView({ documentid : "$documentid$" , signatoryid : "$siglinkid$" });
              svb.fetch({ processData:  true, cache : false});

              jQuery(function(){
                  var docview = new DocumentSignView({
                      id:"$documentid$",
                      signviewbranding : svb,
                      usebranding : true,
                      viewer : new DocumentViewer({
                          signatoryid : "$siglinkid$"
                      })
                  });

              var headerDiv = jQuery('<div/>');
              var component = React.renderComponent(
                Header({
                   signviewbranding: svb,
                    forPad : true
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.renderComponent(
                Footer({
                   signviewbranding: svb
                }), footerDiv[0]);
              jQuery('.signview').prepend(headerDiv);
              jQuery('.signview').append(footerDiv);

              jQuery(".body-container").replaceWith(docview.view.el);
            });

              mixpanel.register({
                Context : 'Sign view for pad',
                DocumentID : "$documentid$",
                'Signatory ID' : $siglinkid$
              });

              mixpanel.people.set({
               'Last document sign view' : new Date()
              });
           });
         </script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag$
####