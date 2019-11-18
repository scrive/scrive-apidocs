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
postIDINViewRedirect=
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
<script src="$cdnbaseurl$/compiled/all/postidinviewredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
postIDINSignRedirect=
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
  <script src="$cdnbaseurl$/compiled/all/postidinsignredirect-$versioncode$.js" crossorigin="anonymous"></script>
</body>
</html>
####
