###
loginPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    <meta name="robots" content="noindex" />
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
$if(analytics.gacode)$
    <script>
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', "$analytics.gacode$"]);
      _gaq.push(['_trackPageview']);
      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();
    </script>
$endif$
  </head>
  <body >
  $flashmessages()$
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
  <script type="text/javascript">
    var fromTemplate = {
      referer: "$referer$",
      nolinks: $if(nolinks)$true$else$false$endif$,
      langprefix : "$langprefix()$"
    };
  </script>
  <script src="$cdnbaseurl$/compiled/all/login-$versioncode$.js" crossorigin="anonymous"></script>
  </body>
$htmlclosingtag()$
###
changePasswordPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
<script type="text/javascript">
  var fromTemplate = {
    linkchangepassword: "$linkchangepassword$"
  };
</script>
  <script src="$cdnbaseurl$/compiled/all/new_password-$versioncode$.js" crossorigin="anonymous"></script>
  </body>
$htmlclosingtag()$
###
accountSetupPage=$doctype()$
  $htmlopentag()$
    <head>
      <title>$pagetitle()$</title>
      $IECompatibilityMetaTag()$
      $standardHeaderContents(nobranding='true')$
      <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
    </head>
    <body>
      $flashmessages()$

      <div class='mainContainer create-account-box'>
        <div style="margin-bottom: 20px; margin-top: 64px; text-align: center" >
          <img alt='logo' src="$cdnbaseurl$/login_logo/$brandingdomainid$/$brandinghash$" />
          <div class='divider-line'></div>
          <span class='label' style="text-align :center; width:275px">
            $PoweredByScrive()$
          </span>
        </div>

        <script>
        var fromTemplate = {
          // FIXME will this underscore.js stuff work here?
          // It has not been imported yet, right?
          fstname: _.unescape("$fstname$"), // name can contain some funky chars like &
          sndname: _.unescape("$sndname$"), // -,,-
          email: "$email$",
          company : _.unescape("$company$"),// -,,-
          userid : "$userid$",
          signupmethod : "$signupmethod$",
          companyPosition: _.unescape("$companyPosition$"),
          mobile: "$mobile$",
          companyAdmin : $if(companyAdmin)$true$else$false$endif$
        };
        </script>
        <script src="$cdnbaseurl$/compiled/all/account_setup-$versioncode$.js" crossorigin="anonymous"></script>
      </div>

    </body>
  $htmlclosingtag()$
###
pageAcceptTOS =
<div class='mainContainer big'>
<div class='inner'>
<script src="$cdnbaseurl$/compiled/all/accept_tos-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
####
