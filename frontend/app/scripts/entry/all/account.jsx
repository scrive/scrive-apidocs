var React = require("react");
var Account = require("../../../js/account/account").Account;
var $ = require("jquery");

$(function () {
  var account = new Account(fromTemplate);

  $(".body-container").append(account.el());

  mixpanel.register({Context : "Account Page"});
  mixpanel.track("View Account Page");
});
