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
scaleReset=
    <meta name="viewport" content="width=1020, user-scalable=no" />
    <script type="text/javascript">
      require(['legacy_code'], function() {
        jQuery(document).ready(function() {
          setTimeout(function() {
            jQuery('meta[name=viewport]').attr('content','width=1020, user-scalable=yes');},
            200);
        });
      });
    </script>
####
padLogin=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'React', 'login/login', 'legacy_code'], function(jQuery,React,Login) {
    jQuery(document).ready(function() {
      var div = jQuery('.global-table-cell');
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
pageDocumentSignView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true')$
    $if(!usestandardheaders)$
      <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='/internal_signview_branding/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $endif$
    $scaleReset()$
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
                      signviewbranding : svb,
                      viewer : new DocumentViewer({
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
$htmlclosingtag()$
####
pageDocumentSignPadView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true')$
    <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $scaleReset()$
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
              var headerOpts = {signviewbranding: svb, link: link};
              var target = LocalStorage.get("backlink", "target");
              if (target == "to-sign") {
                link.text= localization.pad.backToList;
                link.onClick = function() {window.location = "/to-sign"};
                headerOpts.forceShowing = true;
              } else if (target == "to-start") {
                link.text = localization.toStart.backFromSigningPage;
                link.onClick = function() {window.location = "/ts/$documentid$"};
              } else {
                link.text = localization.pad.backToDocument;
                link.onClick = function() {window.location = "/d/$documentid$"};
              }

              var component = React.render(React.createElement(Header, headerOpts), headerDiv[0]);

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
$htmlclosingtag()$
####
pageDocumentIdentifyView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true')$
    <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
  </head>
  <body>
    <div class='global-table'>
      <div class="global-table-cell">
      </div>
    </div>
    <script>
    require(["legacy_code", "jquery", "React", "signview/identify/identifyview"], function(legacy_code, jQuery, React, IdentifyView) {
      var doc = new Document({
                  id: "$documentid$",
                  viewer : new DocumentViewer({
                    signatoryid : "$siglinkid$"
                  })
                });

      jQuery(function () {
        // no design for loading.
        doc.recall(function () {
          React.render(React.createElement(IdentifyView, {
            doc: doc,
            siglinkid: "$siglinkid$"
          }), jQuery(".global-table-cell")[0]);
        });
      });
    });
    </script>
  </body>
$htmlclosingtag()$
####
