var jQuery = require("jquery");
// Listen on height changes to an element.
  module.exports = function (el, cb) {
    if (el instanceof jQuery) {
      el = el[0];
    }

    var lastHeight = el.clientHeight;
    var newHeight;
    (function run() {
      newHeight = el.clientHeight;

      if (lastHeight != newHeight) {
        cb();
      }

      lastHeight = newHeight;

      if (el.onElementHeightChangeTimer) {
        clearTimeout(el.onElementHeightChangeTimer);
      }

      el.onElementHeightChangeTimer = setTimeout(run, 200);
    })();
  }
