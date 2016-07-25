var React = require("react");
var $ = require("jquery");
var Blocking = require("../../../js/blocking").Blocking;
var DesignView = require("../../../js/designview/docdesignview").DesignView;

$(function () {
  window.BlockingInfo = Blocking();

  var dv = new DesignView({
    id: fromTemplate.documentId
  });

  mixpanel.register({DocumentID: fromTemplate.documentId, Context: "design view"});
  mixpanel.track("View Design View");

  $(".body-container").replaceWith(
    $("<div />").append(window.BlockingInfo.el())
    .append(dv.el())
    .children()
  );

  $(window).resize();
});
