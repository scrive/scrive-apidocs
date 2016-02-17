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
    <script src="/compiled/all/designview.js"></script>
    </div>
    </div>
  $postPageCode()$
  </body>
$htmlclosingtag()$
###
