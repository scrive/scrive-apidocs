####
pagePadListView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="pad-list">
          <div class="list-container"></div>
          <script>
            var fromTemplate = {
              headerLogOut: "$_headerLogOut()$"
            };
          </script>
          <script src="$cdnbaseurl$/compiled/all/padlist-$versioncode$.js"></script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
####
