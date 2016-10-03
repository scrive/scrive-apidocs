import Backbone from "backbone";

/* This is just a state object. While drawing we need to store some information.
    For example two last points in current drawing, so we can draw nice curves or
    information if anything has been drawn at all so far.

    This object DOESN'T store picture, just information needed to continue drawing.

*/

module.exports =  Backbone.Model.extend({
  defaults: function () {
    return {
      empty: false, // If something was drawn on canvas
      drawing: false, // Is drawing in progress
      pointerId: undefined, // unique id of pointer being used for drawing (in multitouch scenarios)
      drawingMethod: undefined, // What tool is used for drawing (mouse or touch)
      drawnAnyLine: false, // Drawn line durring current drawing
      pointerOutside: false, // Mouse left the canvas while still drawing
      x_: undefined, // Previous x
      y_: undefined,  // Previous y
      x: undefined,   // Current x
      y: undefined    // Current y
    };
  },
  initialize: function () {
  },
  empty: function () {
    return this.get("empty");
  },
  setEmpty: function () {
    this.set("empty", true);
  },
  drawingInProgress: function () {
    return this.get("drawing");
  },
  drawingInProgressWithDrawingMethodAndPointerId: function (method, pointerId) {
    return this.get("drawing") && this.get("drawingMethod") === method && this.get("pointerId") === pointerId;
  },
  drawnAnyLine: function () {
    return this.get("drawnAnyLine");
  },
  drawingMethod: function () {
    return this.get("drawingMethod");
  },
  pointerId: function () {
    return this.get("pointerId");
  },
  // All changes that are happending while drawing are silent, since we don't need to rerender
  // view for performance reasons.
  startDrawing: function (drawingMethod, pointerId) {
    this.set({
      drawing: true,
      pointerId: pointerId,
      pointerOutside: false,
      drawingMethod: drawingMethod
    }, {silent: true});

    // FIXME: replace this with something more performant.
    this.trigger("change");
  },
  stopDrawing: function () {
    this.set({
        drawing: false,
        drawingMethod: undefined
    }, {silent: true});
  },
  setStartPoint: function (x, y) {
    this.set({
      empty: false,
      drawnAnyLine: false,
      x_: undefined,
      y_: undefined,
      x: x,
      y: y
    }, {silent: true});
  },
  lineDrawn: function (x_, y_, x, y) {
    this.set({
      empty: false,
      drawnAnyLine: true,
      x_: x_,
      y_: y_,
      x: x,
      y: y
    }, {silent: true});
  },
  x: function () {
    return this.get("x");
  },
  y: function () {
    return this.get("y");
  },
  x_: function () {
    return this.get("x_");
  },
  y_: function () {
    return this.get("y_");
  },
  pointerOutside: function () {
    return this.get("pointerOutside");
  },
  setPointerOutside: function (pointerOutside) {
    this.set("pointerOutside", pointerOutside, {silent: true});
  }
});
