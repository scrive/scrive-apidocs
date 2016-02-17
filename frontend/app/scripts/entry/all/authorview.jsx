var React = require("react");
var $ = require("jquery");
var AuthorView = require("../../../js/authorview/authorview").AuthorView;
var DocumentViewer = require("../../../js/documentviewer").DocumentViewer;

$(function () {
  var authorView = new AuthorView({
    id: fromTemplate.documentId,
    viewer : new DocumentViewer({
      signatoryid : fromTemplate.signatoryid,
      authorcompanyadmin : fromTemplate.authorcompanyadmin
    })
  });

  $(".place-for-authorview").replaceWith(authorView.el());

  mixpanel.register({
    DocumentID: fromTemplate.documentId,
    "Signatory ID": fromTemplate.signatoryid,
    "Admin of author?": fromTemplate.authorcompanyadmin
  });

  mixpanel.track("View Author View");
});
