###
loginPage=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$ Scrive Features</title>
    <meta name="description" content="$_featuresPageMetaDescription()$" />
    $standardHeaderContents()$
  </head>
  $standardBody(unwrappedbody=loginPageContents())$
</html>
###
loginPageContents=
<script type="text/javascript">
  jQuery(document).ready(function() {
    var login = new Login({referer: "$referer$"});
  });
</script>
###
changePasswordPage=
$doctype()$
<html>
  <head>
    <title>$pagetitle()$ Scrive Features</title>
    <meta name="description" content="$_featuresPageMetaDescription()$" />
    $standardHeaderContents()$
  </head>
  $standardBody(unwrappedbody=changePasswordPageContents())$
</html>
###
changePasswordPageContents=
<script type="text/javascript">
  jQuery(document).ready(function() {
    new NewPassword({linkchangepassword: "$linkchangepassword$"});
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
