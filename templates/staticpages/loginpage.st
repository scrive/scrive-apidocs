###
loginPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
   require(['jquery', 'React', 'login/login', 'legacy_code'], function(jQuery,React,Login) {
  jQuery(document).ready(function() {
    var referer =  "$referer$";
    if (window.location.hash != "" && referer.indexOf("#") == -1) referer = referer + window.location.hash;
    var div = jQuery("<div / >");
    jQuery('.inner').append(div);  //Order is important here. To have focus on input, we need to have it rendered on element that is appended.
    React.renderComponent(Login({
        referer:   referer,
        autofocus: true
    }),div[0]);
    mixpanel.register({
      Context : 'Login page'
    });
    mixpanel.track('View login page');
  });
 });
</script>
</div>
</div>
###
loginPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$" >
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
        autofocus: true
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
changePasswordPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    jQuery('.inner').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
 });
</script>
</div>
</div>
###
accessNewAccountPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    mixpanel.track('View access new account page');
    jQuery('.inner').append(new NewPassword({
      linkchangepassword: "$linkchangepassword$",
      accessnewaccount: true
    }).el());
  });
 });
</script>
</div>
</div>
###
###
changePasswordPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    jQuery('.inner').append(new NewPassword({
                        linkchangepassword: "$linkchangepassword$",
                        branded : true,
                        textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
                        buttoncolorclass : "$buttoncolorclass$"

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
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
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
                        branded : true,
                        textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
                        buttoncolorclass : "$buttoncolorclass$"
                       }).el());
  });
 });
</script>
  </body>
$htmlclosingtag()$
###
signupPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(document).ready(function() {
    mixpanel.track('Visit signup page');
    jQuery('.inner').append(new Signup({email:     "$email:noescape()$",
                                        autofocus: true}).el());
  });
 });
</script>
</div>
</div>
######
signupPageWithBranding=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
   require(['jquery', 'legacy_code'], function(jQuery) {
    jQuery(document).ready(function() {
      mixpanel.track('Visit signup page');
      jQuery('.inner').append(new Signup({
                                         branded : true,
                                         buttoncolorclass : "$buttoncolorclass$",
                                         servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
                                         textscolour: $if(textscolour)$"$textscolour$"$else$null$endif$,
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
        $standardHeaderContents()$
      </head>
      <body style="background: $if(backgroundcolour)$$backgroundcolour$$else$none repeat scroll 0 0 #FFFFFF;$endif$">

      $flashmessages()$

      <div style='width:275px;margin:20px auto; text-align: center;'>
        <div style="margin-bottom: 50px; margin-top: 50px;">
          $if(customlogo)$
            <img alt="logo" src="$customlogolink$"/>
            <div class="divider-line"></div>
            <label style="text-align:center;width:275px;$if(customexternaltextcolour)$;color:$customexternaltextcolour$$endif$">E-signing powered by Scrive</label>
          $else$
            <img src="/img/Scrive-logo.png" style='margin: 50px 30px 0px;'/>
          $endif$
        </div>
      </div>
      <div class='mainContainer'>

      <script type="text/javascript">
       require(['jquery', 'legacy_code'], function(jQuery) {
          jQuery(document).ready(function() {
            jQuery('.mainContainer').append( new AccountSetup({
                                                  fstname: "$fstname$",
                                                  sndname: "$sndname$",
                                                  company : "$company$",
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
