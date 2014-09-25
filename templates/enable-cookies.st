###
enableCookies=
<!DOCTYPE html>
<html>

<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">

<title>How to enable cookies on the iPhone</title>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<meta http-equiv="content-language" content="$ctxlang$" />
<meta http-equiv="X-UA-Compatible" content="IE=10" />
<!--[if lt IE 9]>
<script src="/bower_components/es5-shim/es5-shim.js"></script>
<script src="/bower_components/es5-shim/es5-sham.js"></script>
<script src="/libs/html5shiv.js"></script>
<script src="/bower_components/json3/lib/json3.min.js"></script>
<![endif]-->
<link href='/css/fonts/SourceSansPro.css' rel='stylesheet' type='text/css'/>
<link href='/css/fonts/HelveticaNeue.css' rel='stylesheet' type='text/css'/>
<link rel=" icon" href="/favicon.png" />
<script src="/$ctxlang$/localization/$versioncode$.js"  type="text/javascript"></script>
<script type="text/javascript"> window.versioncode = "$versioncode$" </script>

$if(production)$
<script src="/$versioncode$.optimized-system.js"></script>
$else$
<script src="/compiled_jsx/requirejs_config.js"></script>
<script src="/bower_components/requirejs/require.js"></script>
$endif$

$analyticsLoaderKontrakcja()$

<script>
  require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(document).ready(function() {
      var cookies = Cookies.buildCookieMap();
      var cookieNames = [];
      for(var key in cookies) {
        cookieNames.push(key);
      }
      mixpanel.track('Cookies not enabled', {'cookie': cookieNames.toString(),
                                             'user-agent': navigator.userAgent});
    });
  });
</script>
</head>

<body>

<h2>How to enable cookies on the iPhone</h2>
  <p>For Scrive to work, you must allow cookies to be placed on your telephone so that the server can track current session information.</p>
  <p>To enable cookies in Mobile Safari:</p>
  <ol>
    <li>Go to the Home screen by pressing the Home button or by unlocking your phone.</li>
    <li>Select the <strong>Settings</strong> icon.<br>
    <img src="/img/enable-cookies/pict0044.jpg" alt="iPhone Home"></li>
    <li>Select <strong>Safari</strong> from the <strong>Settings</strong> menu.<br>
    <img src="/img/enable-cookies/iphone_settings_s.jpg" alt="iPhone Settings"></li>
    <li>Select <strong>Accept Cookies</strong> from the <strong>Safari</strong> menu.<br>
    <img src="/img/enable-cookies/iphone_settings_safari.jpg" alt="iPhone Safari Settings"></li>
    <li>Select <strong>From visited</strong> from the <strong>Accept Cookies</strong><strong> menu.<br>
    <img src="/img/enable-cookies/iphone_accept_cookies.jpg" alt="iPhone Safari Accept Cookies"></strong></li>
    <li>Press the <strong>Home</strong> button to return the the iPhone home screen.</li>
    <li>Select the <strong>Safari</strong> icon to return to Safari.</li>
    <li>Before the cookie settings change will take effect, Safari must restart.  To restart Safari <strong>press and hold the Home button (for around five seconds)</strong> until the iPhone display goes blank and the home screen appears.</li>
    <li>Select the <strong>Safari</strong> icon to return to Safari.</li>
  </ol>
  <p>If cookies are successfully enabled, clicking signing link in the email again should lead to signing process.</p>

</body>
</html>
###
