var React = require("react");
var $ = require("jquery");
var FlashMessageAfterReload = require("../../../js/flashmessages.js").FlashMessageAfterReload;

$(function () {
  if (fromTemplate.incorrect_data) {
     new FlashMessageAfterReload({type : "error", content: localization.identify.authorizationDataMismatch});
  }
  window.top.location = decodeURIComponent(window.atob(fromTemplate.redirect));
});
