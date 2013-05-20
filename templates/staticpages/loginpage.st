###
loginPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new Login({referer:   "$referer$",
                                       autofocus: true}).el());
    mixpanel.register({
      Context : 'Login page'
    });
    mixpanel.track('View login page');
  });
</script>
</div>
</div>
###
loginPageWithBranding=
$doctype()$
<html>
  <head>
    <script src="//cdn.optimizely.com/js/158965942.js"></script>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$" >
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
    jQuery(document).ready(function() {
      jQuery('.inner').append(new Login({
                                         branded : true,
                                         referer:   "$referer$",
                                         logolink:   "$logolink$",
                                         buttoncolorclass : "$buttoncolorclass$",
                                         servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
                                         autofocus: true}).el());
      mixpanel.register({
        Context : 'Login page - branded'
      });
      mixpanel.track('View login page -branded');
    });
  </script>

  </body>
</html>
###
changePasswordPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
</script>
</div>
</div>
###
###
changePasswordPageWithBranding=
$doctype()$
<html>
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
  jQuery(document).ready(function() {
    jQuery('.inner').append(new NewPassword({
                        linkchangepassword: "$linkchangepassword$",
                        branded : true,
                        logolink:   "$logolink$",
                        buttoncolorclass : "$buttoncolorclass$"
                       }).el());
  });
</script>
  </body>
</html>
###
signupPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new Signup({email:     "$email:noescape()$",
                                        autofocus: true}).el());
  });
</script>
</div>
</div>
######
signupPageWithBranding=
$doctype()$
<html>
  <head>
    <script src="//cdn.optimizely.com/js/158965942.js"></script>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body style="background: $background$">
  <div class='mainContainer big' style='height:auto;position: static;overflow-y:hidden;'>
      <div class="inner" style='height:auto;position:static;margin:auto;width:auto;'>
      </div>
  </div>
  <script type="text/javascript">
    jQuery(document).ready(function() {
      jQuery('.inner').append(new Signup({
                                         branded : true,
                                         logolink:   "$logolink$",
                                         buttoncolorclass : "$buttoncolorclass$",
                                         servicelinkcolour: $if(servicelinkcolour)$"$servicelinkcolour$"$else$null$endif$,
                                         autofocus: true
                            }).el());
    });
  </script>

  </body>
</html>
###
accountSetupPage=

    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      <body style="background: $if(custombackground)$$custombackground$$else$none repeat scroll 0 0 #FFFFFF;$endif$">

      $flashmessages()$

      <div style='width:275px;margin:20px auto;'>
        <div style="margin-bottom: 50px; margin-top: 50px;">
          $if(customlogo)$
            <img alt="logo" src="$customlogolink$"/>
            <div class="divider-line"></div>
            <label style="text-align:center;width:275px;">E-signing powered by Scrive</label>
          $else$
            <img src="/img/Scrive-logo.png" style='margin: 50px 30px 0px;'/>
          $endif$
        </div>
      </div>
      <div class='mainContainer'>

      <script type="text/javascript">
          jQuery(document).ready(function() {
            jQuery('.mainContainer').append( new AccountSetup({
                                                  fstname: "$fstname$",
                                                  sndname: "$sndname$",
                                                  company : "$company$",
                                                  userid : "$userid$",
                                                  signupmethod : "$signupmethod$",
                                                  companyFilled : $if(company)$true$else$false$endif$,
                                                  buttoncolorclass : "$if(customdomainbdbuttonclass)$$customdomainbdbuttonclass$$else$blue$endif$",
                                                  branded : $if(hasbrandeddomain)$true$else$false$endif$,
                                                  servicelinkcolour: $if(customservicelinkcolour)$"$customservicelinkcolour$"$else$null$endif$
                                                  })
                                              .el());
            jQuery('.short-input-section input:first').focus();
          });
        </script>
        </div>

      </body>
    </html>
###
pageAcceptTOS =
<div class='mainContainer big'>
<div class='inner'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new AcceptTOS({}).el());
  });
</script>
</div>
</div>
####