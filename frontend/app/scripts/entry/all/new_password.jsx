var React = require("react");
var NewPassword = require("../../../js/new_password").NewPassword;
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  var linkchangepassword = fromTemplate.linkchangepassword;

  $(".global-table-cell").append(new NewPassword({
    linkchangepassword: linkchangepassword
  }).el());
});
