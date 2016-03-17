var React = require("react");
var $ = require("jquery");

var Header = require("../../pages/special/header");
var Footer = require("../../pages/special/footer");
var ToStart = require("../../to-start/to-start");
var Document = require("../../../js/documents").Document;

$(function () {
  var doc = new Document({id: fromTemplate.documentId});
  doc.recall(function() {
    var toStartDiv = $("<div />");
    $(".form-container").append(toStartDiv);
    $(".to-start").append(footerDiv);
      React.render(React.createElement(ToStart,{document: doc}), toStartDiv[0]);
  });

  var headerDiv = $("<div/>");
  var component = React.render(React.createElement(Header,{
    linkText: localization.pad.backToList,
    linkOnClick: function() {
      window.location.href = "/to-start";
    }
  }), headerDiv[0]);

  var footerDiv = $("<div/>");
  var component = React.render(React.createElement(Footer,{
  }), footerDiv[0]);

  $(".to-start").prepend(headerDiv);

  mixpanel.register({
    Context : "To Start"
  });
});
