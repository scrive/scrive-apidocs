var jQuery = require("jquery");
// Determine if an element is in viewport.
  module.exports = function (el) {
    var rect = el.getBoundingClientRect();

    if (el instanceof jQuery) {
      el = el[0];
    }

    return (
      rect.top >= 0 &&
      rect.left >= 0 &&
      rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
      rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
  }
