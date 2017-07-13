var $ = require("jquery");

var MakePoint = function (x, y) {
  return {x: x, y: y};
};

var MakeZeroPoint = function () {
  return MakePoint(0, 0);
};

var MakeRect = function (x, y, width, height) {
  return {
    origin: MakePoint(x, y),
    size: {
      height: height,
      width: width
    }
  };
};

var MakeZeroRect = function () {
  return MakeRect(0, 0, 0, 0);
};

var GetViewFrame = function (view) {
  try {
    var $view = $(view.getDOMNode());
    var position = $view.position();

    return MakeRect(
      position.left, position.top, $view.outerWidth(), $view.outerHeight()
    );
  } catch (error) {
    return undefined;
  }
};

var GetRectXMax = function (rect) {
  return rect.origin.x + rect.size.width;
};

var GetRectXMin = function (rect) {
  return rect.origin.x;
};

var GetRectYMax = function (rect) {
  return rect.origin.y + rect.size.height;
};

var GetRectYMin = function (rect) {
  return rect.origin.y;
};

var RelToAbs = function (value, relativeValue) {
  return Math.round(value * relativeValue);
};

var AbsToRel = function (value, relativeValue) {
  return value / relativeValue;
};

module.exports = {
  AbsToRel: AbsToRel,
  GetRectXMax: GetRectXMax,
  GetRectXMin: GetRectXMin,
  GetRectYMax: GetRectYMax,
  GetRectYMin: GetRectYMin,
  GetViewFrame: GetViewFrame,
  MakePoint: MakePoint,
  MakeRect: MakeRect,
  MakeZeroPoint: MakeZeroPoint,
  MakeZeroRect: MakeZeroRect,
  RelToAbs: RelToAbs
};
