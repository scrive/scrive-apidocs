var React = require("react");
var $ = require("jquery");
var FlashMessageAfterReload = require("../../../js/flashmessages.js").FlashMessageAfterReload;

$(function () {
  if (fromTemplate.incorrect_data) {
     new FlashMessageAfterReload({type : "error", content: localization.identifyVerimi.authorizationDataMismatch});
  }
  window.location = decodeURIComponent(window.atob(fromTemplate.redirect));
});
