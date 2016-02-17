###
loginPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
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
  <script src="/compiled/all/login.js"></script>
  </body>
$htmlclosingtag()$
###
changePasswordPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
<script type="text/javascript">
  var fromTemplate = {
    linkchangepassword: "$linkchangepassword$",
    accessnewaccount: false
  };
</script>
  <script src="/compiled/all/new_password.js"></script>
  </body>
$htmlclosingtag()$
###
accessNewAccountPageWithBranding=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents(nobranding='true')$
    <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='global-table'>
      <div class="global-table-cell">
      </div>
  </div>
<script type="text/javascript">
  var fromTemplate = {
    linkchangepassword: "$linkchangepassword$",
    accessnewaccount: true
  };
</script>
  <script src="/compiled/all/new_password.js"></script>
  </body>
$htmlclosingtag()$
###
accountSetupPage=$doctype()$
  $htmlopentag()$
    <head>
      <title>$pagetitle()$</title>
      $IECompatibilityMetaTag()$
      $standardHeaderContents(nobranding='true')$
      <link href='$cdnbaseurl$/login_branding/$brandingdomainid$/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
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
          fstname: _.unescape("$fstname$"), // name can contain some funky chars like &
          sndname: _.unescape("$sndname$"), // -,,-
          email: "$email$",
          company : _.unescape("$company$"),// -,,-
          userid : "$userid$",
          signupmethod : "$signupmethod$",
          companyAdmin : $if(companyAdmin)$true$else$false$endif$
        };
        </script>
        <script src="/compiled/all/account_setup.js"></script>
      </div>

    </body>
  $htmlclosingtag()$
###
pageAcceptTOS =
<div class='mainContainer big'>
<div class='inner'>
<script src="/compiled/all/accept_tos.js"></script>
</div>
</div>
####
