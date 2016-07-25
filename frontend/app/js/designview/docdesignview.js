var React = require("react");
var $ = require("jquery");
var DesignViewComponent = require("../../scripts/designview/designview");
var Document = require("../documents.js").Document;

var DesignView = exports.DesignView = function(args) {
  var doc = new Document({
    id: args.id
  });

  var container = $("<div/>");
  var view = React.render(
    React.createElement(DesignViewComponent, {model: doc}), container[0]
  );

  doc.recall();

  this.el = function() {
    return container;
  };

  this.model = function() {
    return model;
  };
};
