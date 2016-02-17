var $ = require("jquery");
// Various tools to deal with viewports and zoom on mobile devices.
  var exports = {};

  // Get the current zoom level (>1 is zoomed in <1 is zoomed out.)
  var zoomLevel = exports.zoomLevel = function () {
    return (document.body.clientWidth / window.innerWidth) || 1;
  };

  module.exports = exports;
