var React = require("react");
var $ = require("jquery");
var DesignView = require("../../../js/designview/docdesignview").DesignView;
var Track = require("../../common/track");

$(function () {

  var dv = new DesignView({
    id: fromTemplate.documentId
  });

  mixpanel.register({DocumentID: fromTemplate.documentId, Context: "design view"});
  Track.track("View Design View");

  $(".body-container").replaceWith(dv.el());

  $(window).resize();
});
