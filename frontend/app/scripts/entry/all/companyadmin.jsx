var React = require("react");
var $ = require("jquery");

var CompanyAdminView = require("../../admin/companyadmin/companyadmin.jsx");

$(function () {
  var $container = $("<div></div>");
  var view = React.render(
    React.createElement(CompanyAdminView, {companyId: fromTemplate.companyId, forAdmin: fromTemplate.forAdmin}),
    $container[0]
  );

  $(".admin").append($container);
});
