var React = require("react");
var $ = require("jquery");

var NewPasswordView = require("../../../scripts/account/newpassword");

$(function () {
  var view = React.render(
    React.createElement(
      NewPasswordView,
      {
        url: fromTemplate.linkchangepassword
      }
    ),
    $(".global-table-cell")[0]
  )
});
