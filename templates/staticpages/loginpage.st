###
loginPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new Login({referer: "$referer$"}).el());
  });
</script> 
</div>
###
changePasswordPage=
<div class='mainContainer big'>
<img width="1920" height="800" src="/img/bg-home.jpg" class="bg"/>
<script type="text/javascript">
  jQuery(document).ready(function() {
    jQuery('.mainContainer').append(new NewPassword({linkchangepassword: "$linkchangepassword$"}).el());
  });
</script>
###
signupPage=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$ Scrive Features</title>
    <meta name="description" content="$_featuresPageMetaDescription()$" />
    $standardHeaderContents()$
  </head>
  $standardBody(unwrappedbody=signupPageContents())$
</html>
###
signupPageContents=
<script type="text/javascript">
  jQuery(document).ready(function() {
    new Signup({email: $email:noescape()$, signuplink: "$signuplink$"});
  });
</script>
###
accountSetupPage=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$ Scrive Features</title>
    <meta name="description" content="$_featuresPageMetaDescription()$" />
    $standardHeaderContents()$
  </head>
  $standardBody(unwrappedbody=accountSetupPageContents())$
</html>
###
accountSetupPageContents=
<script type="text/javascript">
  jQuery(document).ready(function() {
    new AccountSetup({signuplink: "$signuplink$",
                      fstname: "$fstname$",
                      sndname: "$sndname$"});
  });
</script>
####
