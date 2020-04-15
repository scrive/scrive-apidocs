####
#### Page for EID Auth redirect (not for Auth at Sign-time)
####
postEIDAuthRedirect=
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
<script src="$cdnbaseurl$/compiled/all/posteidauthdredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
postEIDAuthAtSignRedirect=
<html>
  <head>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
    <link href="/less/signview-less-compiled.css" rel="stylesheet" type="text/css" />
  </head>
<body id="body" class="mainContainer signview">
  <script>
  var fromTemplate = {
    redirect: "$redirect$",
    incorrect_data: $if(incorrect_data)$true$else$false$endif$,
    document_id: "$document_id$",
    signatory_link_id: "$signatory_link_id$"
  };
  </script>
  <script src="$cdnbaseurl$/compiled/all/posteidauthatsignredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
