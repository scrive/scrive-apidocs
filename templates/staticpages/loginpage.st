###
loginPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Login({referer: "$referer$"}).el());
  });
</script> 
</div>
###
changePasswordPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
</script>
</div>
###
signupPage=
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Signup({email: "$email:noescape()$"}).el());
  });
</script>
</div>
###
accountSetupPage=

    $doctype()$
    <html>
      <head>
        <title>$pagetitle()$</title>
        $standardHeaderContents()$
      </head>
      <body style='background: none repeat scroll 0 0 #FFFFFF'>

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

        <div class='mainContainer big'>
        <script type="text/javascript">
          jQuery(document).ready(function() {
            jQuery('.mainContainer').append( new AccountSetup({
                                                  fstname: "$fstname$",
                                                  sndname: "$sndname$"})
                                              .el());
          });
        </script>
        </div>

      </body>
    </html>
###
pageAcceptTOS =
<div class='mainContainer big'>
<img width="1920" height="544" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new AcceptTOS({}).el());
  });
</script>
</div>
####