var React = require("react");
var FlashMessage = require("../../../js/flashmessages").FlashMessage;
var $ = require("jquery");

$(function () {
  if (fromTemplateHasFlash) {
    new FlashMessage(fromTemplateFlash);
  }
});
