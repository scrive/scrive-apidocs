###
notFound=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    $if(logged)$
      <link href='/service_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='/scrive_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $endif$
  </head>
<body>
<div class="wrapper-position-footer">
  $header()$
  <div class='mainContainer big error-page'>
    <div class='error-page-info'>
      <div class='error-page-description'>
        $_notFound()$
      </div>
      <div class='error-page-instruction'>
        $_afterErrorGoToInstruction()$
      </div>

     <div class='error-page-buttons-box'>
       <a class='button float-left' href='/'>
         $_afterErrorGoHomepage()$
       </a>
       <a href='#' class='button float-right' onClick='history.go(-1);return false;'>
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
  $pageFooter()$
</div>
</body>
$htmlclosingtag()$
###
notFoundWithoutHeaders=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="/login_logo/$brandinghash$" />
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
internalServerError=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    $if(logged)$
      <link href='/service_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $else$
      <link href='/scrive_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    $endif$
  </head>
<body>
<div class="wrapper-position-footer">
  $header()$
  <div class='mainContainer big error-page'>
    <div class='error-page-info'>
      <div class='error-page-description'>
        $_internalServerError()$
      </div>
      <div class='error-page-instruction'>
        $_afterErrorGoToInstruction()$
      </div>

     <div class='error-page-buttons-box'>
       <a class='button float-left' href='/'>
         $_afterErrorGoHomepage()$
       </a>
       <a href='#' class='button float-right' onClick='history.go(-1);return false;'>
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
  $pageFooter()$
</div>
</body>
$htmlclosingtag()$
###
internalServerErrorWithoutHeaders=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
<body>
<div class="wrapper-position-footer">
  <div class='mainContainer big error-page'>
   <div class="error-page-header">
     <div class="error-page-header-logo-wrapper" >
        <img alt='logo' src="/login_logo/$brandinghash$" />
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
###
sessionTimeOut=
<div class='mainContainer big'>
  <img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
  <div class='short-input-section'>
    <div class="shadowed">
      <h1>$_sessionTimeOutHeader()$</h1>
      <h4>$_sessionTimeOutContent1()$</h4>
      <h4>$_sessionTimeOutContent2()$</h4>
    </div>
  </div>
</div>
<script>
  require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(document).ready(function() {
      var cookies = Cookies.buildCookieMap();
      var cookieNames = [];
      for(var key in cookies) {
        cookieNames.push(key);
      }
      mixpanel.track('Cookies enabled but timeouted', {'cookie': cookieNames.toString(),
                                                       'user-agent': navigator.userAgent});
    });
  });
</script>
###