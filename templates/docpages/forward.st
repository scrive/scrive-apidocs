####
####  This template does not check if user is logged in, since this page
####  always behaves as if the user is *not* logged in.
####
afterForwardPage=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/document_signview_branding/$brandingdomainid$/$documentid$/$brandinghash$-branding.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
  <div class="forwarded-view">
    <div class="mainContainer" >
      $flashmessages()$
      <div>
        <script>
          var fromTemplate = {
            documentId: "$documentid$",
          };
        </script>
        <script src="$cdnbaseurl$/compiled/all/forwarded-$versioncode$.js" crossorigin="anonymous"></script>
      </div>
    </div>
  </div>
</body>
$htmlclosingtag()$
###
