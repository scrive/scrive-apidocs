####
toStartLogin=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
  </head>
  <body style="background: $background$" >
  $flashmessages()$
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
  <script src="$cdnbaseurl$/compiled/all/tostart_login-$versioncode$.js" type="text/javascript" crossorigin="anonymous"></script>
  </body>
$htmlclosingtag()$
###
pageToStartDocumentView=$doctype()$
$htmlopentag()$
  <head>
    <title>$documenttitle$ - $pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="form-container"></div>
          <script>
            var fromTemplate = {
              documentId: "$documentid$"
            };
          </script>
          <script src="$cdnbaseurl$/compiled/all/tostart_view-$versioncode$.js" crossorigin="anonymous"></script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
####
pageToStartListView=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=2" />
    <link href='$cdnbaseurl$/padlist_signview_branding/$brandingdomainid$/$brandinguserid$/$brandinghash$/branding.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="to-start-signview responsive">
      <div class="mainContainer" >
        $flashmessages()$
        <div class="to-start">
          <div class="list-container"></div>
          <script>
            var fromTemplate = {
              headerLogOut: "$_headerLogOut()$"
            };
          </script>
          <script src="$cdnbaseurl$/compiled/all/tostart_list-$versioncode$.js" crossorigin="anonymous"></script>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
###
