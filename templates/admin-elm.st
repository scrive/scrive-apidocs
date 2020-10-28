###
adminElmMain=$doctype()$
  $htmlopentag()$
    <head>
      <title>Scrive AdminOnly</title>
      <link rel="stylesheet" type="text/css" href="$cdnbaseurl$/elm-assets/adminonly-$versioncode$.css"/>
      $IECompatibilityMetaTag()$
    </head>
    <body>
      <div id="elm-mount" class="container">
        <div class="row align-items-center justify-content-center">
          <div class="col-lg-4 col-md-6 col-sm-8 col-10 alert alert-light" role="alert">
            <h1>Scrive AdminOnly</h1>
            <p>Loading...</p>
          </div>
        </div>
      </div>
    </body>
    <script>
    window.cdnBaseUrl="$cdnbaseurl$"
    window.isAdmin=$if(admin)$true$else$false$endif$
    </script>
    <script src="$cdnbaseurl$/elm-assets/adminonly-$versioncode$.js" crossorigin="anonymous"></script>
  $htmlclosingtag()$
####
