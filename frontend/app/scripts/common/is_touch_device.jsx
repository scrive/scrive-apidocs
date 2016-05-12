module.exports = function () {
  return "ontouchstart" in window || navigator.maxTouchPoints;
};
