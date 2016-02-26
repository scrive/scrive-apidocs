var React = require("react");
var NewPassword = require("../../../js/new_password").NewPassword;
var $ = require("jquery");

$(function () {
  var linkchangepassword = fromTemplate.linkchangepassword;
  var accessnewaccount = fromTemplate.accessnewaccount;

  if (accessnewaccount) {
    mixpanel.track("View access new account page", {
      branded: true
    });
  }

  $(".global-table-cell").append(new NewPassword({
    linkchangepassword: linkchangepassword,
    accessnewaccount: accessnewaccount
  }).el());
});
