var React = require("react");
var $ = require("jquery");

var NewDocumentWithBPID = require("../../designview/newdocumentwithbpid");

$(function () {
  var $container = $("<div class='newdocumentwithbpid'></div>");
  var view = React.render(
    React.createElement(
      NewDocumentWithBPID, {}
    ),
    $container[0]
  );

  $(".body-container").append($container);
});
