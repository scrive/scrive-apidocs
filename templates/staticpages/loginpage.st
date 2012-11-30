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