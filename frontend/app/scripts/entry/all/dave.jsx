var React = require("react");
var $ = require("jquery");

var Dave = require("../../admin/dave.jsx");

$(function () {
  if (fromTemplate.istemplate) {
    var $container = $("<div></div>");
    var view = React.render(
      React.createElement(Dave.TemplateTransfer, {documentid: fromTemplate.documentid}),
      $container[0]);
    $(".dave-transfer").append($container);
  }
});
