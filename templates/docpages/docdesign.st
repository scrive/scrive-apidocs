#####
pageDocumentDesign=$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $IECompatibilityMetaTag()$
    $standardHeaderContents()$
  </head>
  <body $if(custombackground)$style="background:$custombackground$;"$endif$ class='designview'>
    $header()$
    $flashmessages()$

    <div class="mainContainer" >
    <div class="body-container">
    <script>
    var fromTemplate = {
      documentId: "$documentid$"
    };

    </script>
    <script src="$cdnbaseurl$/compiled/all/designview-$versioncode$.js" crossorigin="anonymous"></script>
    </div>
    </div>
  $postPageCode()$
  </body>
$htmlclosingtag()$
###
