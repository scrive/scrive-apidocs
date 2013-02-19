###
loginPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new Login({referer: "$referer$"}).el());

    var emailfield = jQuery('.short-input-section input[type=text]');
    if(emailfield.val()) {
      // Email field already filled, focus password instead
      jQuery('.short-input-section input[type=password]').focus();
    } else {
      emailfield.focus();
    }

    mixpanel.register({
      Context : 'Login page'
    });
    mixpanel.track('View login page');
  });
</script>
</div>
</div>
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
signupPage=
<div class='mainContainer big'>
<div class="inner">
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.inner').append(new Signup({email: "$email:noescape()$"}).el());
    jQuery('.short-input-section input').focus();
  });
</script>
</div>
</div>
###
accountSetupPage=

    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      <body style='background: none repeat scroll 0 0 #FFFFFF;'>

      <header class="site thin">
        <nav>
          <ul class="ct thin">
            <li id="branding">
              <a id="logo" class="page" href="$httplink$$if(langenglish)$/en/$else$/sv/$endif$"><img src="/img/logo.png" width="120" height="22"/></a>
              <div id="tagline" class="h3"></div>
            </li>
            <li class="float-right page-first" style="margin-top: 30px;text-align : right; line-height: 20px">
              <div style='font-size: 14.5px'>
                 <div>Scrive help desk</div>
                 <div>+46 8 519 779 00</div>
              </div>
            </li>
          </ul>
        </nav>
      </header>

       $flashmessages()$

        <div class='mainContainer big' style='min-height: 712px;'>
        <script type="text/javascript">
          jQuery(document).ready(function() {
            jQuery('.mainContainer').append( new AccountSetup({
                                                  fstname: "$fstname$",
                                                  sndname: "$sndname$",
                                                  company : "$company$",
                                                  userid : "$userid$",
                                                  companyFilled : $if(company)$true$else$false$endif$,
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