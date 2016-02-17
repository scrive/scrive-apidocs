var PricePage = require("../../../js/payments").PricePage;
var $ = require("jquery");

$(function () {
  $(".price-plan-page-link").addClass("active");
  PricePage({}).show(".js-price-plan");
  mixpanel.track("View Price Plan Page");
  setTimeout(function() {
    // Race condition between rendering of header and adding class
    $(".price-plan-page-link").addClass("active");
  },100);
});
