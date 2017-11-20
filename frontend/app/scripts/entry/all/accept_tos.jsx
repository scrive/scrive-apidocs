var $ = require("jquery");

var AcceptTOSView = require("../../account/accepttos");

$(function () {
  var container = document.createElement("div");
  container.className = "short-input-section accept-tos s-accept-tos";

  var view = React.render(React.createElement(AcceptTOSView, {}), container);
  $(".inner").append(container);
});
