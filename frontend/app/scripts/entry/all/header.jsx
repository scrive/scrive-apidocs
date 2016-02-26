var React = require("react");
var Header = require("../../pages/header");
var $ = require("jquery");

$(function () {
  React.render(React.createElement(Header, fromTemplateHeader)
  , $('.place-for-header')[0]);
});
