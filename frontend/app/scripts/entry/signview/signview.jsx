var React = require("react");
var SignView = require("../../signview/signview");
var LocalStorage = require("../../../js/storage").LocalStorage;
var $ = require("jquery");

$(function () {
  var target = LocalStorage.get("backlink", "target");
  fromTemplate.link = getLink(target);
  React.render(React.createElement(SignView, fromTemplate), $(".place-for-signview")[0]);
});
