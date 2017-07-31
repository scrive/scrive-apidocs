var React = require("react");
var AccountSetup = require("../../../js/account_setup").AccountSetup;
var $ = require("jquery");

$(function () {
  $(".create-account-box").append(new AccountSetup({
    fstname: fromTemplate.fstname,
    sndname: fromTemplate.sndname,
    email: fromTemplate.email,
    company : fromTemplate.company,
    userid : fromTemplate.userid,
    signupmethod : fromTemplate.signupmethod,
    position : fromTemplate.companyPosition,
    phone : fromTemplate.mobile,
    companyAdmin : fromTemplate.companyAdmin
  }).el());

  $(".short-input-section input:first").focus();
});
