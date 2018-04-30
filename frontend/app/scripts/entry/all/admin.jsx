var React = require("react");
var $ = require("jquery");

var AdminView = require("../../admin/admin.jsx");

$(function () {
  var $container = $("<div></div>");
  var view = React.render(
    React.createElement(AdminView, {forAdmin: fromTemplate.forAdmin}),
    $container[0]
  );

  $(".admin").append($container);
});
