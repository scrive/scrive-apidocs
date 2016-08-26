var PricePage = require("../../../js/payments").PricePage;
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  $(".price-plan-page-link").addClass("active");
  PricePage({}).show(".js-price-plan");
  Track.track("View Price Plan Page");
  setTimeout(function() {
    // Race condition between rendering of header and adding class
    $(".price-plan-page-link").addClass("active");
  },100);
});
