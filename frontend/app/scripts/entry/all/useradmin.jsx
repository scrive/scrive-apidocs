var UserAdmin = require("../../../js/admin/useradmin").UserAdmin;
var $ = require("jquery");

$(function () {
  $(".admin").append(new UserAdmin({userid: fromTemplate.userId}).el());
});
