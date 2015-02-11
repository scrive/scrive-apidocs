####
wrapperopentag=
<div class="wrapper-position-footer">
####
wrapperclosingtag=
</div>
########
signviewAnalyticsVariables=
<script>
  var _gaq = _gaq || [];
  _gaq.push(['_setCustomVar',
    1, // custom var slot (arbitrary between 1-5)
    'Signing Page', // name
    'true', // value (always a string)
    2 // set this custom var to this session, not this visitor
  ]);
</script>
####
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
   require(['jquery', 'React', 'login/login', 'legacy_code'], function(jQuery,React,Login) {
    jQuery(document).ready(function() {
      var div = jQuery('.inner');
      React.render(React.createElement(Login,{
        referer:   "/to-sign",
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
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true')$
    $if(!usestandardheaders)$
      <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='/internal_signview_branding/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $endif$
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
           require(['jquery', 'React' , 'signview/header', 'signview/footer', 'legacy_code'], function(jQuery,React,Header,Footer) {
              var svb =  new BrandingForSignView({ documentid : "$documentid$" , signatoryid : "$siglinkid$" });
              svb.fetch({ processData:  true, cache : false});

              jQuery(function(){
                  var docview = new DocumentSignView({
                      id:"$documentid$",
                      signviewbranding : svb
                      $if(usestandardheaders)$
                      ,  ignoredoclang : true
                      $endif$
                      , viewer : new DocumentViewer({
                          signatoryid : "$siglinkid$"
                         })
                  });


                $if(!usestandardheaders)$

                  var headerDiv = jQuery('<div/>');
                  var component = React.render(React.createElement(Header,{
                      signviewbranding: svb,
                      documentid:"$documentid$",
                      signatorylinkid : "$siglinkid$"
                    }), headerDiv[0]);

                  var footerDiv = jQuery('<div/>');
                  var component = React.render(React.createElement(Footer,{
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
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true')$
    <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="signview">
      <div class="mainContainer" >
        $flashmessages()$

        <div class="body-container">
          <script>
           require(['jquery', 'React' , 'signview/header', 'signview/footer', 'legacy_code'], function(jQuery,React,Header,Footer) {
              var svb =  new BrandingForSignView({ documentid : "$documentid$" , signatoryid : "$siglinkid$" });
              svb.fetch({ processData:  true, cache : false});
              jQuery(function(){
                  var docview = new DocumentSignView({
                      id:"$documentid$",
                      signviewbranding : svb,
                      viewer : new DocumentViewer({
                          signatoryid : "$siglinkid$"
                      })
                  });

              var headerDiv = jQuery('<div/>');
              var link = {};
              var target = LocalStorage.get("backlink", "target");
              if (target == "to-sign") {
                link.text= localization.pad.backToList;
                link.onClick = function() {window.location = "/to-sign"};
              } else if (target == "to-start") {
                link.text = localization.toStart.backFromSigningPage;
                link.onClick = function() {window.location = "/ts/$documentid$"};
              } else {
                link.text = localization.pad.backToDocument;
                link.onClick = function() {window.location = "/d/$documentid$"};
              }

              var component = React.render(React.createElement(Header,{
                   signviewbranding: svb,
                   link : link
                }), headerDiv[0]);

              var footerDiv = jQuery('<div/>');
              var component = React.render(React.createElement(Footer,{
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
