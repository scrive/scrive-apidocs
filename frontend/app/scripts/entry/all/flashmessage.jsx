var React = require("react");
var FlashMessages = require("../../../js/flashmessages");
var $ = require("jquery");

$(function () {
  if (fromTemplateHasFlash) {
    new FlashMessages.FlashMessage(fromTemplateFlash);
  }
  FlashMessages.FlashMessageTryFromCookie();
});
