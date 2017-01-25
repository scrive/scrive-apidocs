###
loginPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$-styles.css' rel='stylesheet' type='text/css'/>
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
