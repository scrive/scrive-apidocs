####
wrapperopentag=
<div class="wrapper-position-footer">
####
wrapperclosingtag=
</div>
########
padLogin=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'React', 'login/brandedlogin', 'legacy_code'], function(jQuery,React,BrandedLogin) {
    jQuery(document).ready(function() {
      var div = jQuery('.inner');
      React.renderComponent(BrandedLogin({
        referer:   "/to-sign",
        logolink:   "$if(logolink)$$logolink$$else$/img/logo_email.png$endif$",
        servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
        textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
        autofocus: true,
        pad : true,
        nolinks : true
      }),div[0]);
      mixpanel.register({
        Context : 'Login page - branded'
      });
      mixpanel.track('View login page -branded');
    });
   });
  </script>

  </body>
$htmlclosingtag()$
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
              var link = {};
              if (LocalStorage.get("pad","from-list") == "true") {
                link.text= localization.pad.backToList;
                link.onClick = function() {window.location = "/to-sign"};
              }
              else {
                link.text = localization.pad.backToDocument;
                link.onClick = function() {window.location = "/d/$documentid$"};
              }

              var component = React.renderComponent(
                Header({
                   signviewbranding: svb,
                   link : link
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