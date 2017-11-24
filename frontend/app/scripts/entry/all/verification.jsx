var React = require("react");
var $ = require("jquery");

var VerificationView = require("../../pages/verification");

$(function () {
  var view = React.render(
    React.createElement(VerificationView, {}), $(".verificationContainer")[0]
  );
});
