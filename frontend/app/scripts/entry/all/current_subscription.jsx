var React = require("react");
var Subscription = require("../../account/subscription");
var $ = require("jquery");

$(function () {
  var subscriptionData = fromCurrentSubscriptionTemplate.subscriptionData;
  var subscriptionUserIsCompanyAdmin = fromCurrentSubscriptionTemplate.subscriptionUserIsCompanyAdmin;
  if (subscriptionData) {
    Subscription.initCurrentSubscription(subscriptionData, subscriptionUserIsCompanyAdmin);
  }
});
