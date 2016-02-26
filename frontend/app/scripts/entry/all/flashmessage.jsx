var React = require("react");
var Flashmessage = require("../../../js/flashmessages");
var $ = require("jquery");

$(function () {
  if (fromTemplateHasFlash) {
    new FlashMessage(fromTemplateFlash);
  }
});
