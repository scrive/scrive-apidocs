var React = require("react");
var SignView = require("../../signview/signview");
var LocalStorage = require("../../../js/storage").LocalStorage;
var $ = require("jquery");
var FlashMessages = require("../../../js/flashmessages");

$(function () {
  var target = LocalStorage.get("backlink", "target");
  fromTemplate.link = getLink(target);

  if (fromTemplate.flowDocument) {
    fromTemplate.link = {
      text: "Back to document list",
      onClick: function() { window.location = fromTemplate.flowBacklink; }
    };
  }

  React.render(React.createElement(SignView, fromTemplate), $(".place-for-signview")[0]);
  FlashMessages.FlashMessageTryFromCookie();
});
