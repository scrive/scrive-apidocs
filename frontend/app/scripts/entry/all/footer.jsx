var React = require("react");
var Footer = require("../../pages/footer");
var $ = require("jquery");

$(function () {
  React.render(React.createElement(Footer, fromTemplateFooter)
  , $('.place-for-footer')[0]);
});
