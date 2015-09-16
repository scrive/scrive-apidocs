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
    $flashmessages()$
    $if(usestandardheaders)$
    $wrapperopentag()$
    $header()$
    $endif$
    <div class="place-for-signview"></div>
    <script>
     require(['jquery', 'React', 'signview/signview', 'legacy_code'], function(jQuery, React, SignView) {
        jQuery(function(){
          React.render(React.createElement(SignView, {
            documentId: "$documentid$",
            sigLinkId: "$siglinkid$",
            allowSaveSafetyCopy: $if(allowsavesafetycopy)$ true $else$ false $endif$,
            authorFullname: "$authorFullname$",
            authorPhone: "$authorPhone$",
            useStandardHeaders: $if(usestandardheaders)$ true $else$ false $endif$
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
    <div class="place-for-signview"></div>
    <script>
     require(['jquery', 'React', 'signview/signview', 'legacy_code'], function(jQuery, React, SignView) {
        jQuery(function(){
          var link = {};
          var forceShowing = false;
          var target = LocalStorage.get("backlink", "target");
          if (target == "to-sign") {
            link.text= localization.pad.backToList;
            link.onClick = function() {window.location = "/to-sign"};
            forceShowing = true;
          } else if (target == "to-start") {
            link.text = localization.toStart.backFromSigningPage;
            link.onClick = function() {window.location = "/ts/$documentid$"};
          } else {
            link.text = localization.pad.backToDocument;
            link.onClick = function() {window.location = "/d/$documentid$"};
          }

          React.render(React.createElement(SignView, {
            documentId: "$documentid$",
            sigLinkId: "$siglinkid$",
            allowSaveSafetyCopy: $if(allowsavesafetycopy)$ true $else$ false $endif$,
            authorFullname: "$authorFullname$",
            authorPhone: "$authorPhone$",
            useStandardHeaders: $if(usestandardheaders)$ true $else$ false $endif$,
            link: link,
            forceShowing: forceShowing
          }), jQuery(".place-for-signview")[0]);
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
