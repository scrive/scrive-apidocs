var React = require("react");
var $ = require("jquery");

var CompanyAdminView = require("../../admin/companyadmin/companyadmin.jsx");

$(function () {
  var $container = $("<div></div>");
  var view = React.render(
    React.createElement(CompanyAdminView, {companyId: fromTemplate.companyId}),
    $container[0]
  );

  $(".admin").append($container);
});
