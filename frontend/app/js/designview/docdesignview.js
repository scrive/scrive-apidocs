var React = require("react");
var $ = require("jquery");

var DesignViewComponent = require("../../scripts/designview/designview");
var Document = require("../documents.js").Document;
var Subscription = require("../../scripts/account/subscription");

var DesignView = exports.DesignView = function(args) {
  var doc = new Document({
    id: args.id
  });

  var subscription = new Subscription({});
  var container = $("<div/>");
  var view = React.render(
    React.createElement(DesignViewComponent, {model: doc, subscription: subscription}), container[0]
  );

  doc.recall();
  subscription.reload();

  this.el = function() {
    return container;
  };

  this.model = function() {
    return model;
  };
};
