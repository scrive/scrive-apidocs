var React = require("react");
var $ = require("jquery");

var AccountSetupView = require("../../account/setup/accountsetupview");

$(function () {
  var container = document.createElement("div");
  container.className = "short-input-section account-setup";

  var dirs = window.location.pathname.split("/").reverse();
  $(".create-account-box").append(container);
  var view = React.render(
    React.createElement(
      AccountSetupView,
      {
        fstname: fromTemplate.fstname,
        sndname: fromTemplate.sndname,
        email: fromTemplate.email,
        company: fromTemplate.company,
        userid: fromTemplate.userid,
        signupmethod: fromTemplate.signupmethod,
        position: fromTemplate.companyPosition,
        phone: fromTemplate.mobile,
        companyAdmin: fromTemplate.companyAdmin,
        invitationId: dirs[1]
      }
    ),
    container
  );
});
