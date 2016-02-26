var AcceptTOS = require("../../../js/accept_tos").AcceptTOS;
var $ = require("jquery");

$(function () {
  $(".inner").append(new AcceptTOS({}).el());
});
