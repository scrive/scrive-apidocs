import Backbone from "backbone";
import _ from "underscore";
import $ from "jquery";
import DrawingUtils from "../drawing/drawingutils";

/* Main drawing component

    It should be created over canvass context and drawing state, with color and line width.
    Example:
      new Drawing({ picture: canvasContext, drawingState: drawingState, color "#7700FF"})

    Events listeners for drawing has to be initialized with initDrawing(HTMLElement).
    Usually we listen on same element (canvas) we are drawing on - but it doesn't have to be like this in general.

    Two functions bindStopDrawingToGlobalHandlers and unbindStopDrawingFromGlobalHandlers should be used
    with onMount/ onUnmount, since leaving drawing area may not trigger event on drawing area.

    If drawing area should be visible all the time, but drawing is not always possible, setActive should be used.

*/

module.exports = Backbone.Model.extend({
  defaults: function () {
    return {
      active: true,
      picture: undefined,
      drawingState: undefined,
      color: "#1B137F",
      lineWidth: 8,
      onStartDrawing: function () {},
      onStopDrawing: function () {}
    };
  },
  initialize: function () {
    this.stopDrawingIfInProgress = _.bind(this.stopDrawingIfInProgress, this);
  },
  picture: function () {
    return this.get("picture");
  },
  drawingState: function () {
    return this.get("drawingState");
  },
  lineWidth: function () {
    return this.get("lineWidth");
  },
  setLineWidth: function (v) {
    this.set("lineWidth", v);
  },
  isActive: function () {
    return this.get("active");
  },
  setActive: function (v) {
    this.set({"active": v});
  },
  startDrawing: function (drawingMethod, pointerId) {
    this.drawingState().startDrawing(drawingMethod, pointerId);
    this.get("onStartDrawing")();
  },
  stopDrawing: function () {
    this.drawingState().stopDrawing();
    this.get("onStopDrawing")();
  },
  drawingColorWithOpacity: function (opacity) {
    return DrawingUtils.colorWithOpacity(this.get("color"), opacity);
  },
  drawDot: function (x, y, color, radius) {
    this.picture().fillStyle = color;
    // Use canvas.arc to draw a circle with the diameter of width
    this.picture().arc(x, y,  radius / 2, 0,  Math.PI * 2, true);
    this.picture().fill();
  },
  drawCircle: function (x, y) {
    this.picture().beginPath();
    var radius = this.lineWidth();
    this.drawDot(x, y, this.drawingColorWithOpacity(1), radius);
    this.picture().closePath();
  },
  drawingtoolDown: function (x, y, drawingMethod, e, eventPointerId) {
    if (!this.drawingState().drawingInProgress() && DrawingUtils.isStartDrawingEvent(drawingMethod, e)) {
      this.drawingState().setStartPoint(x, y);
      if (eventPointerId === undefined) {
        eventPointerId = DrawingUtils.getEventPointerId(e);
      }
      this.startDrawing(drawingMethod, eventPointerId);
      this.picture().beginPath();
      this.picture().moveTo(x, y);
      this.picture().lineWidth = this.lineWidth();
      this.picture().lineCap = "round";
      this.picture().lineJoin = "round";
    }
  },
  drawingtoolOutside: function (x, y, drawingMethod, e) {
    // drawingMethod is always mouse, but initial method could be different (e.g. ms),
    // use the initial method to draw
    if (this.drawingState().drawingInProgress()) {
      this.drawingtoolMove(x, y, this.drawingState().drawingMethod(), e);
      this.drawingtoolMove(x, y, this.drawingState().drawingMethod(), e);
      this.drawingState().setPointerOutside(true);
    }
  },
  drawingtoolInside: function (x, y, drawingMethod, e) {
    if (this.drawingState().drawingInProgress() && this.drawingState().pointerOutside()) {
      drawingMethod = this.drawingState().drawingMethod();
      var eventPointerId = this.drawingState().pointerId();
      this.drawingState().setPointerOutside(false);
      this.stopDrawing();
      // start drawing unconditionally, because if button was depressed
      // drawing was already stopped, and we would not get to this point
      this.drawingtoolDown(x, y, drawingMethod, e, eventPointerId);
    }
  },
  drawingtoolMove: function (x, y, drawingMethod, e) {
    var pointerId = DrawingUtils.getEventPointerId(e);
    // we have to check for pointerOutside, because <=IE10 is retarded,
    // and fires a few mousemove events before firing mouseenter event
    if (this.drawingState().drawingInProgressWithDrawingMethodAndPointerId(drawingMethod, pointerId) &&
        !this.drawingState().pointerOutside()) {
      var x_ = this.drawingState().x();
      var y_ = this.drawingState().y();
      var x__ = this.drawingState().x_();
      var y__ = this.drawingState().y_();

      var moved = function (x1, x2) { return (x1 * 2 + x2 * 1) / 3; };
      if (x__ != undefined && y__ != undefined) {
        this.drawNiceCurve(x__, y__, x_, y_, moved(x_, x), moved(y_, y));
        this.drawNiceLine(moved(x_, x), moved(y_, y), moved(x, x_), moved(y, y_));
      } else {
        this.drawNiceLine(x_, y_, moved(x, x_), moved(y, y_));
      }
      this.drawingState().lineDrawn(moved(x, x_), moved(y, y_), x, y);
    }
  },
  drawingtoolUp: function (x, y, drawingMethod, e) {
    var pointerId = DrawingUtils.getEventPointerId(e);
    if (this.drawingState().drawingInProgressWithDrawingMethodAndPointerId(drawingMethod, pointerId)) {
      this.picture().lineTo(x, y);
      this.picture().closePath();
      if (!this.drawingState().drawnAnyLine()) {
        this.drawCircle(x, y);
      }
      this.stopDrawing();
    }
  },
  drawNiceLine: function (sx, sy, ex, ey) {
    this.picture().closePath();
    this.picture().beginPath();
    this.drawLine(sx, sy, ex, ey, this.lineWidth() + 1, this.drawingColorWithOpacity(0.05), "butt");
    this.drawLine(sx, sy, ex, ey, this.lineWidth(),     this.drawingColorWithOpacity(0.3), "round");
    this.drawLine(sx, sy, ex, ey, this.lineWidth() - 1, this.drawingColorWithOpacity(0.5), "round");
    this.drawLine(sx, sy, ex, ey, this.lineWidth() - 2, this.drawingColorWithOpacity(1), "round");
  },
  drawNiceCurve: function (sx, sy, cx, cy, ex, ey) {
    this.picture().closePath();
    this.picture().beginPath();
    this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() + 1, this.drawingColorWithOpacity(0.05), "butt");
    this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth(),     this.drawingColorWithOpacity(0.3), "round");
    this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() - 1, this.drawingColorWithOpacity(0.5), "round");
    this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() - 2, this.drawingColorWithOpacity(1), "round");
  },
  drawCurve: function (sx, sy, cx, cy, ex, ey, w, c, lc) {
    this.picture().moveTo(sx, sy);
    this.picture().strokeStyle = c;
    this.picture().lineWidth = w;
    this.picture().lineCap = lc;
    this.picture().quadraticCurveTo(cx, cy, ex, ey);
    this.picture().stroke();
  },
  drawLine: function (sx, sy, ex, ey, w, c, lc) {
    this.picture().moveTo(sx, sy);
    this.picture().strokeStyle = c;
    this.picture().lineWidth = w;
    this.picture().lineCap = lc;
    this.picture().lineTo(ex, ey);
    this.picture().stroke();
  },
  initDrawing: function (drawingArea) {
    var self = this;
    var drawing = function (fn, type) {
      return function (e) {
        if (self.isActive()) {
          e.preventDefault();
          e.stopPropagation();
          e.target.style.cursor = "default";
          _.bind(fn, self)(DrawingUtils.xPos(e), DrawingUtils.yPos(e), type, e);
          return false;
        } else {
          e.target.style.cursor = "inherit";
        }
      };
    };

    if ("ontouchstart" in document.documentElement) {
      drawingArea.addEventListener("touchstart", drawing(self.drawingtoolDown, DrawingUtils.TOUCH_METHOD));
      drawingArea.addEventListener("touchmove", drawing(self.drawingtoolMove, DrawingUtils.TOUCH_METHOD));
      drawingArea.addEventListener("touchend", drawing(self.drawingtoolUp, DrawingUtils.TOUCH_METHOD));
    } else if (navigator.msPointerEnabled) {
      drawingArea.addEventListener("MSPointerDown", drawing(self.drawingtoolDown, DrawingUtils.MS_METHOD), true);
      drawingArea.addEventListener("MSPointerMove", drawing(self.drawingtoolMove, DrawingUtils.MS_METHOD), true);
      drawingArea.addEventListener("MSPointerUp", drawing(self.drawingtoolUp, DrawingUtils.MS_METHOD), true);
    }

    $(drawingArea).mousedown(drawing(self.drawingtoolDown, DrawingUtils.MOUSE_METHOD));
    $(drawingArea).mousemove(drawing(self.drawingtoolMove, DrawingUtils.MOUSE_METHOD));
    $(drawingArea).mouseup(drawing(self.drawingtoolUp, DrawingUtils.MOUSE_METHOD));
    $(drawingArea).mouseenter(drawing(self.drawingtoolInside, DrawingUtils.MOUSE_METHOD));
    $(drawingArea).mouseout(drawing(self.drawingtoolOutside, DrawingUtils.MOUSE_METHOD));
  },
  stopDrawingIfInProgress: function () {
    if (this.drawingState().drawingInProgress()) {
      this.stopDrawing();
    }
  },
  bindStopDrawingToGlobalHandlers: function () {
    if ("ontouchstart" in document.documentElement) {
      $(document)[0].addEventListener("touchend", this.stopDrawingIfInProgress);
    } else if (navigator.msPointerEnabled) {
      $(document)[0].addEventListener("MSPointerUp", this.stopDrawingIfInProgress);
    }
    $(document).mouseup(this.stopDrawingIfInProgress);
  },
  unbindStopDrawingFromGlobalHandlers: function () {
    if ("ontouchstart" in document.documentElement) {
      $(document)[0].removeEventListener("touchend", this.stopDrawingIfInProgress);
    } else if (navigator.msPointerEnabled) {
      $(document)[0].removeEventListener("MSPointerUp", this.stopDrawingIfInProgress);
    }
    $(document).off("mouseup", this.stopDrawingIfInProgress);
  }

});
