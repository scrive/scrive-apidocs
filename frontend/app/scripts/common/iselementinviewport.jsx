var jQuery = require("jquery");

// Entire element is in viewport.
exports.entire = function (el) {
  if (el instanceof jQuery) {
    el = el[0];
  }

  var rect = el.getBoundingClientRect();

  return (
    rect.top >= 0 &&
    rect.left >= 0 &&
    rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
    rect.right <= (window.innerWidth || document.documentElement.clientWidth)
  );
};

// Part of element is in viewport.
exports.part = function (el) {
  if (el instanceof jQuery) {
    el = el[0];
  }

  var rect = el.getBoundingClientRect();

  var windowHeight = (window.innerHeight || document.documentElement.clientHeight);
  var windowWidth = (window.innerWidth || document.documentElement.clientWidth);

  var vertInView = (rect.top <= windowHeight) && ((rect.top + rect.height) >= 0);
  var horInView = (rect.left <= windowWidth) && ((rect.left + rect.width) >= 0);

  return (vertInView && horInView);
};
