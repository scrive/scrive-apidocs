####
#### Page for Verimi redirect
####
postVerimiRedirect=
<html>
  <head>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
  </head>
<body>
<script>
var fromTemplate = {
  redirect: "$redirect$",
  incorrect_data: $if(incorrect_data)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/postverimiredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
postIDINRedirect=
<html>
  <head>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
  </head>
<body>
<script>
var fromTemplate = {
  redirect: "$redirect$",
  incorrect_data: $if(incorrect_data)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/postidinredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
