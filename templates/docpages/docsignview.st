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
padLogin=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
  <script src="$cdnbaseurl$/compiled/all/padlogin-$versioncode$.js" type="text/javascript"></script>
  </body>
$htmlclosingtag()$
########
pageDocumentSignView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    <meta name="viewport" content="width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no" />
    <meta name="format-detection" content="telephone=no"/>
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true'; signview='true')$
    <link href='$cdnbaseurl$/document_signview_branding/$brandingdomainid$/$documentid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>

  <body>
    $flashmessages()$
    <div class="place-for-signview"></div>
    <script>
        function getLink(target) {
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
        return link
        }

        var documentData;
        try {
          documentData = JSON.parse(Base64.decode("$b64documentdata$"));
        } catch (e) {
          console.log("Could not parse document data embedded in the DOM", e);
        }

        var fromTemplate = {
          documentId: "$documentid$",
          sigLinkId: "$siglinkid$",
          documentData: documentData,
          allowSaveSafetyCopy: $if(allowsavesafetycopy)$ true $else$ false $endif$,
          loggedInAsAuthor: $if(loggedinauthor)$ true $else$ false $endif$,
          authorFullname: "$authorFullname$",
          authorPhone: "$authorPhone$"
        };

      if (trackJs !== undefined) {
        trackJs.configure({userId: "$siglinkid$"});
      }

      mixpanel.register({
        Context : 'Sign view',
        DocumentID : "$documentid$",
        'Signatory ID' : $siglinkid$
      });

      mixpanel.people.set({
       'Last document sign view' : new Date()
      });
   </script>
   <script src="$cdnbaseurl$/compiled/signview/signview-$versioncode$.js"></script>
  </body>
$htmlclosingtag()$
####
pageDocumentIdentifyView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $signviewAnalyticsVariables()$
    $standardHeaderContents(nobranding='true'; noviewport='true'; signview='true')$
    <link href='$cdnbaseurl$/document_signview_branding/$brandingdomainid$/$documentid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
    <meta name="viewport" content="width=device-width, maximum-scale=1, initial-scale=1, user-scalable=no" />
  </head>
  <body>
    <div class='global-table'>
      <div class="global-table-cell">
      </div>
    </div>
    <script>
    window.netsIdentifyUrl = "$netsIdentifyUrl$";
    window.netsMerchantIdentifier = "$netsMerchantIdentifier$";
    window.netsTrustedDomain = "$netsTrustedDomain$";
    var fromTemplate = {
      documentId: "$documentid$",
      sigLinkId: "$siglinkid$"
    };
    </script>
    <script src="$cdnbaseurl$/compiled/signview/identifyview-$versioncode$.js"></script>
  </body>
$htmlclosingtag()$
####
