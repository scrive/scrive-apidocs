#####
pagePostSignview=
$doctype()$
$htmlopentag()$
  <head>
    <title>$pagetitle()$</title>
    $standardHeaderContents()$
  </head>
  <body>
    $flashmessages()$
    <div class="mainContainer" >
    <div class="body-container">
    <script>
     require(['React', 'postsignview/archive_views', 'postsignview/psv_document_model', 'legacy_code'], function(React, ArchiveView, PsvDocumentModel) {
      var psvDocument = new PsvDocumentModel();
      psvDocument.fetch({
        success: function(document, response) {
          React.render(React.createElement(ArchiveView,{document:document}), jQuery('body')[0]);
        }
      });
     });
    </script>
    </div>
    </div>
  </body>
$htmlclosingtag$
###