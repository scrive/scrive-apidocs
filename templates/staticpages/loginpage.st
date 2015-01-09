###
loginPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body >
  $flashmessages()$
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'React', 'login/brandedlogin', 'legacy_code'], function(jQuery,React,BrandedLogin) {
    jQuery(document).ready(function() {
      var referer =  "$referer$";
      if (window.location.hash != "" && referer.indexOf("#") == -1) referer = referer + window.location.hash;
      var div = jQuery('.inner');
      React.renderComponent(BrandedLogin({
        referer:   referer,
        buttoncolorclass : "$buttoncolorclass$",
        servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
        textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
        autofocus: true,
        langprefix : $langprefix()$
      }),div[0]);

      mixpanel.register({
        Context : 'Login page - branded'
      });
      mixpanel.track('View login page -branded');
    });
   });
  </script>

  </body>
$htmlclosingtag()$
###
changePasswordPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    jQuery('.inner').append(new NewPassword({
                        linkchangepassword: "$linkchangepassword$",
                       }).el());
  });
 });
</script>
  </body>
$htmlclosingtag()$
###
accessNewAccountPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    mixpanel.track('View access new account page', {
        branded: true
    });
    jQuery('.inner').append(new NewPassword({
                        linkchangepassword: "$linkchangepassword$",
                        accessnewaccount: true,
                       }).el());
  });
 });
</script>
  </body>
$htmlclosingtag()$
###
signupPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents(nobranding='true')$
    <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
  </head>
  <body>
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(document).ready(function() {
      mixpanel.track('Visit signup page');
      jQuery('.inner').append(new Signup({
                                         autofocus: true
                            }).el());
    });
   });
  </script>

  </body>
$htmlclosingtag()$
###
accountSetupPage=
  $doctype()$
  $htmlopentag()$
    <head>
      <title>$pagetitle()$</title>
      $standardHeaderContents(nobranding='true')$
      <link href='/login_branding/$brandinghash$/styles.css' rel='stylesheet' type='text/css'/>
    </head>
    <body>
      $flashmessages()$

      <div class='mainContainer create-account-box'>
        <div style="margin-bottom: 20px; margin-top: 50px; text-align: center" >
          <img alt='logo' src="/login_logo/$brandinghash$" />
          <div class='divider-line'></div>
          <label style="text-align :center; width:275px">
            $PoweredByScrive()$
          </label>
        </div>

        <script type="text/javascript">
        require(['jquery', 'legacy_code'], function(jQuery) {
            jQuery(document).ready(function() {
              jQuery('.create-account-box').append( new AccountSetup({
                                                    fstname: "$fstname$",
                                                    sndname: "$sndname$",
                                                    company : _.unescape("$company$"), // company name can contain some funky chars like &
                                                    userid : "$userid$",
                                                    signupmethod : "$signupmethod$",
                                                    companyAdmin : $if(companyAdmin)$true$else$false$endif$,
                                                    buttoncolorclass : "$if(customdomainbdbuttonclass)$$customdomainbdbuttonclass$$else$green$endif$",
                                                    branded : $if(hasbrandeddomain)$true$else$false$endif$,
                                                    servicelinkcolour: $if(customservicelinkcolour)$"$customservicelinkcolour$"$else$null$endif$
                                                    })
                                                .el());
              jQuery('.short-input-section input:first').focus();
            });
        });
        </script>
      </div>

    </body>
  $htmlclosingtag()$
###
pageAcceptTOS =
<div class='mainContainer big'>
<div class='inner'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    jQuery('.inner').append(new AcceptTOS({}).el());
  });
 });
</script>
</div>
</div>
####
