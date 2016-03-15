var React = require("react");
var $ = require("jquery");
var AuthorView = require("../../../js/authorview/authorview").AuthorView;

$(function () {
  var authorView = new AuthorView({
    id: fromTemplate.documentId
  });

  $(".place-for-authorview").replaceWith(authorView.el());

  mixpanel.register({
    DocumentID: fromTemplate.documentId,
    "Signatory ID": fromTemplate.signatoryid,
    "Admin of author?": fromTemplate.authorcompanyadmin
  });

  mixpanel.track("View Author View");
});
