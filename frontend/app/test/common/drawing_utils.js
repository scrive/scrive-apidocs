var React = require("react");
var $ = require("jquery");

var DrawingUtils = require("../../scripts/common/drawing_utils");
var util = require("../util");

describe("common/drawing_utils", function () {
  var container = null;

  var renderComponent = function () {
    container = document.createElement("div");

    var component = React.render(
      React.createElement("div", {htmlId: "test"}, ["TEST"]), container
    );

    document.body.appendChild(container);
    return component;
  };

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should make a point", function () {
    var result = DrawingUtils.MakePoint(1, 2);
    assert.equal(result.x, 1);
    assert.equal(result.y, 2);
  });

  it("should make a zero point", function () {
    var result = DrawingUtils.MakeZeroPoint();
    assert.equal(result.x, 0);
    assert.equal(result.y, 0);
  });

  it("should make a rect", function () {
    var result = DrawingUtils.MakeRect(1, 2, 3, 4);
    assert.equal(result.origin.x, 1);
    assert.equal(result.origin.y, 2);
    assert.equal(result.size.width, 3);
    assert.equal(result.size.height, 4);
  });

  it("should make a zero rect", function () {
    var result = DrawingUtils.MakeZeroRect();
    assert.equal(result.origin.x, 0);
    assert.equal(result.origin.y, 0);
    assert.equal(result.size.width, 0);
    assert.equal(result.size.height, 0);
  });

  it("should get a view frame", function () {
    var view = renderComponent();

    var viewFrame = DrawingUtils.GetViewFrame(view);
    assert.isDefined(view);

    var $view = $(view.getDOMNode());
    assert.equal(viewFrame.origin.x, $view.position().left);
    assert.equal(viewFrame.origin.y, $view.position().top);
    assert.equal(viewFrame.size.width, $view.outerWidth());
    assert.equal(viewFrame.size.height, $view.outerHeight());
  });

  it("should handle errors when getting a view frame", function () {
    var view = renderComponent();
    React.unmountComponentAtNode(container);
    container = null;

    var viewFrame = DrawingUtils.GetViewFrame(view);
    assert.isUndefined(viewFrame);
  });

  it("should get max X of a rect", function () {
    var rect = DrawingUtils.MakeRect(1, 2, 3, 4);

    var result = DrawingUtils.GetRectXMax(rect);
    assert.equal(result, 4);
  });

  it("should get min X of a rect", function () {
    var rect = DrawingUtils.MakeRect(1, 2, 3, 4);

    var result = DrawingUtils.GetRectXMin(rect);
    assert.equal(result, 1);
  });

  it("should get max Y of a rect", function () {
    var rect = DrawingUtils.MakeRect(1, 2, 3, 4);

    var result = DrawingUtils.GetRectYMax(rect);
    assert.equal(result, 6);
  });

  it("should get min Y of a rect", function () {
    var rect = DrawingUtils.MakeRect(1, 2, 3, 4);

    var result = DrawingUtils.GetRectYMin(rect);
    assert.equal(result, 2);
  });

  it("should convert relative coord to absolute", function () {
    var result = DrawingUtils.RelToAbs(0.123, 950);
    assert.equal(result, 117);
  });

  it("should convert absolute coord to relative", function () {
    var result = DrawingUtils.AbsToRel(117, 950);
    assert.approximately(result, 0.123, 0.0005);
  });
});
