var React = require("react");
var Subscription = require("../../account/subscription");
var $ = require("jquery");

$(function () {
  if (fromCurrentSubscriptionTemplate.subscriptionData) {
    Subscription.initCurrentSubscription(fromCurrentSubscriptionTemplate.subscriptionData);
  }
});
