var React = require("react");
var IdentifyView = require("../../signview/identify/identifyview");
var Document = require("../../../js/documents").Document;
var $ = require("jquery");
var FlashMessages = require("../../../js/flashmessages");

$(function () {
  var doc = new Document({
    id: fromTemplate.documentId,
    siglinkid: fromTemplate.sigLinkId
  });

  // no design for loading.
  doc.recall(function () {
    React.render(React.createElement(IdentifyView, {
      doc: doc,
      siglinkid: fromTemplate.sigLinkId,
      useEIDHubForNemID: fromTemplate.useEIDHubForNemID,
      useEIDHubForNOBankIDView: fromTemplate.useEIDHubForNOBankIDView
    }), $(".global-table-cell")[0]);

    FlashMessages.FlashMessageTryFromCookie();
  });
});
