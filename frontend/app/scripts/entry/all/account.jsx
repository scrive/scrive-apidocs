var React = require("react");
var Account = require("../../../js/account/account").Account;
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  var account = new Account(fromTemplate);

  $(".body-container").append(account.el());

  mixpanel.register({Context : "Account Page"});
  Track.track("View Account Page");
});
