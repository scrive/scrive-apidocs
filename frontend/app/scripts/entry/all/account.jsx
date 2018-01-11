var React = require("react");
var $ = require("jquery");

var AccountView = require("../../account/account");

$(function () {
  var $container = $("<div class='account'></div>");
  var view = React.render(
    React.createElement(
      AccountView, {companyAdmin: fromTemplate.companyAdmin,
                    apiLogEnabled: fromTemplate.apiLogEnabled}
    ),
    $container[0]
  );

  $(".body-container").append($container);
});
