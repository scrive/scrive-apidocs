####
####  This template does not check if user is logged in, since this page
####  always behaves as if the user is *not* logged in.
####
gtVerificationPage=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
      <link href='/scrive_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  $header()$
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

      <script>
      require(['jquery', 'legacy_code'], function(jQuery) {
          jQuery('.verificationContainer').append(FileVerifier.init().view.el);
      });
      </script>
    </div>
  </div>
  $pageFooter()$
</div>
</body>
$htmlclosingtag()$
###