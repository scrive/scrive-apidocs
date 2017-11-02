var React = require("react");
var $ = require("jquery");

var UserAdminView = require("../../admin/useradmin/useradmin.jsx");

$(function () {
  var $container = $("<div></div>");
  var view = React.render(
    React.createElement(UserAdminView, {userId: fromTemplate.userId, forAdmin: fromTemplate.forAdmin}),
    $container[0]
  );

  $(".admin").append($container);
});
