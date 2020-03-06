####
####  This template does not check if user is logged in, since this page
####  always behaves as if the user is *not* logged in.
####
verificationPage=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
      <link href='$cdnbaseurl$/scrive_branding/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  <div class="mainContainer" >
    <div class="body-container">
      <div class='box smaller white verificationBox'>
        <span class='verificationDescription'>
          <h1 style="font-weight: 400">$_verificationTitle()$</h1>
          <BR/>
          <h4>$_verificationDescription()$</h4>
          <BR/>
          <BR/>
        </span>
        <span class='verificationContainer'>  </span>
      </div>
      <script src="$cdnbaseurl$/compiled/all/verification-$versioncode$.js" crossorigin="anonymous"></script>
    </div>
  </div>
</div>
</body>
$htmlclosingtag()$
###
