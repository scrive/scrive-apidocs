// Various tools to deal with viewports and zoom on mobile devices.
define(["jquery"], function ($) {
  var exports = {};

  // Get the current zoom level (>1 is zoomed in <1 is zoomed out.)
  var zoomLevel = exports.zoomLevel = function () {
    return document.body.clientWidth / window.innerWidth;
  };

  function encodeContent(content) {
    var values = [];
    for (var key in content) {
      values.push(key + "=" + content[key]);
    }

    return values.join(", ");
  }

  // Append a new viewport, content is an object, example {"width": "device-width"}.
  var appendViewport = exports.appendViewport = function (content) {
    $("head").append("<meta name='viewport' content='" + encodeContent(content) + "'>");
  };

  // Replace the current viewports content.
  var replaceViewport = exports.appendViewport = function (content) {
    $("meta[name=viewport]").attr("content", encodeContent(content));
  };

  return exports;
});
