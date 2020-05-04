###
notFound=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
        <div class='divider-line'></div>
        <div class='label'>
          $PoweredByScrive()$
        </div>
      </div>
   </div>
   <div class='error-page-info'>
    <div class='error-page-description'>
        $_notFound()$
    </div>
    <div class='error-page-instruction'>
        $_afterErrorGoToInstruction()$
    </div>

    <div class='error-page-buttons-box'>
      <a class='button main float-left' href='/'>
        $_afterErrorGoHomepage()$
      </a>
      <a href='#' class='button main float-right' onClick='history.go(-1);return false;'>
        $_afterErrorGoBack()$
      </a>
    </div>
  </div>
  </div>
    <script>
    mixpanel.track('Error', {
      Message : '404',
      URL : window.location.href
    });
    </script>
</div>
</body>
$htmlclosingtag()$
####
customHttpCode=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
        <div class='divider-line'></div>
        <div class='label'>
          $PoweredByScrive()$
        </div>
      </div>
   </div>
   <div class='error-page-info'>
    <div class='error-page-description'>
        <h1>$_customHttpError()$</h1>
        <h4>$message$</h4>
    </div>
    <div class='error-page-instruction'>
        $_afterErrorGoToInstruction()$
    </div>

    <div class='error-page-buttons-box'>
      <a class='button main float-left' href='/'>
        $_afterErrorGoHomepage()$
      </a>
      <a href='#' class='button main float-right' onClick='history.go(-1);return false;'>
        $_afterErrorGoBack()$
      </a>
    </div>
  </div>
  </div>
    <script>
    mixpanel.track('Error', {
      Message : '$httpCcode$',
      URL : window.location.href
    });
    </script>
</div>
</body>
$htmlclosingtag()$
####
internalServerError=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
        <div class='divider-line'></div>
        <div class='label'>
          $PoweredByScrive()$
        </div>
      </div>
   </div>
   <div class='error-page-info'>
    <div class='error-page-description'>
      $_internalServerError()$
    </div>
    <div class='error-page-instruction'>
        $_afterErrorGoToInstruction()$
    </div>

    <div class='error-page-buttons-box'>
      <a class='button main float-left' href='/'>
        $_afterErrorGoHomepage()$
      </a>
      <a href='#' class='button main float-right' onClick='history.go(-1);return false;'>
        $_afterErrorGoBack()$
      </a>
    </div>
  </div>
  </div>
    <script>
      mixpanel.track('Error', {
        Message : '500',
        URL : window.location.href
      });
    </script>
</div>
</body>
$htmlclosingtag()$
####
linkInvalid=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
        <div class='divider-line'></div>
        <div class='label'>
          $PoweredByScrive()$
        </div>
      </div>
   </div>
   <div class='error-page-info'>
    <div class='error-page-description'>
      $_linkInvalidError()$
    </div>

    <div class='error-page-buttons-box'>
      <a class='button main float-left' href='/'>
        $_afterErrorGoHomepage()$
      </a>
      <a href='#' class='button main float-right' onClick='history.go(-1);return false;'>
        $_afterErrorGoBack()$
      </a>
    </div>
  </div>
  </div>
    <script>
      mixpanel.track('Error', {
        Message : '422',
        URL : window.location.href
      });
    </script>
</div>
</body>
$htmlclosingtag()$
###
sessionTimeOut=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
    <div class="wrapper-position-footer">
      <div class='mainContainer big error-page'>
        <div class="error-page-header">
          <div class="error-page-header-logo-wrapper" >
            <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
            <div class='divider-line'></div>
            <div class='label'>
              $PoweredByScrive()$
            </div>
          </div>
        </div>
        <div class='error-page-info'>
          <div class='error-page-description'>
            <h1>$_sessionTimeOutHeader()$</h1>
          </div>
          <div class='error-page-instruction'>
            <h4>$_sessionTimeOutContent1()$</h4>
            <h4>$_sessionTimeOutContent2()$</h4>
          </div>
        </div>
      </div>
    </div>
  </body>
$htmlclosingtag()$
