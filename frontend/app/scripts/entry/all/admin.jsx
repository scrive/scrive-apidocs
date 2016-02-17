var Admin = require("../../../js/admin/admin").Admin;
var $ = require("jquery");

$(function () {
  $(".admin").append(new Admin({isAdmin: fromTemplate.isAdmin}).el());
});
