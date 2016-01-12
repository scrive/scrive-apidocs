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
signviewScaleReset=
    <meta name="viewport" content="width=1040, user-scalable=no" />
    <script type="text/javascript">
      require(['legacy_code'], function() {
        jQuery(document).ready(function() {
          if (BrowserInfo.isAndroid() && BrowserInfo.isFirefox()) {
            // In firefox on android initial scale computing is broken, and we need to recompute it.
            // Also for FF meta tags needs to be appended to head, not only changed
            setTimeout(function() {
              var scale = (window.innerWidth / 1040);
              scale = scale > 1 ? "1.0" : (scale + "").substring(0,4)
              jQuery('head').append("<meta name='viewport' content='width=1040, user-scalable=no, initial-scale="+scale+"'/>");
              setTimeout(function() {
                  jQuery('head').append("<meta name='viewport' content='width=1040, user-scalable=yes'/>");
                }, 200);
              }, 200);
          } else {
            setTimeout(function() {
              jQuery('meta[name=viewport]').attr('content','width=1040, user-scalable=yes');
            }, 200);
          }

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
    <meta name="format-detection" content="telephone=no"/>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true'; signview='true')$
    <script type="application/base64" id="document-data">$b64documentdata$</script>
    <link href='/document_signview_branding/$documentid$/$siglinkid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    $signviewScaleReset()$
  </head>

  <body>
    $flashmessages()$
    <div class="place-for-signview"></div>
    <script>
     require(['jquery', 'React', 'signview/signview', 'legacy_code'], function(jQuery, React, SignView) {
        jQuery(function(){
          var target = LocalStorage.get("backlink", "target");
          var link = undefined;
          $if(loggedinauthor)$
            if (target == "to-sign") {
              link = {
                text: localization.pad.backToList,
                onClick: function() {window.location = "/to-sign"}
              };
            } else if (target == "to-start") {
              link = {
                text: localization.toStart.backFromSigningPage,
                onClick: function() {window.location = "/ts/$documentid$"}
              };
            } else if (target == "document") {
              link = {
                text: localization.pad.backToDocument,
                onClick: function() {window.location = "/d/$documentid$"}
              };
            }
          $endif$

          $if(loggedinsignatory)$
            if (target == "list") {
             link = {
                text: localization.pad.backToList,
                onClick: function() {window.location =  "/d"}
              };
            } else if (target == "document") {
              link = {
                text: localization.pad.backToDocument,
                onClick: function() {window.location = "/d/$documentid$"}
              };
            }
          $endif$

          React.render(React.createElement(SignView, {
            documentId: "$documentid$",
            sigLinkId: "$siglinkid$",
            allowSaveSafetyCopy: $if(allowsavesafetycopy)$ true $else$ false $endif$,
            loggedInAsAuthor: $if(loggedinauthor)$ true $else$ false $endif$,
            authorFullname: "$authorFullname$",
            authorPhone: "$authorPhone$",
            link: link
          }), jQuery(".place-for-signview")[0]);
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
  </body>
$htmlclosingtag()$
####
pageDocumentIdentifyView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true'; signview='true')$
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
      window.netsIdentifyUrl = "$netsIdentifyUrl$";
      window.netsMerchantIdentifier = "$netsMerchantIdentifier$";
      window.netsTrustedDomain = "$netsTrustedDomain$";
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
