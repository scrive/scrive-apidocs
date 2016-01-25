/* Content of modal for drawing signature */

define(["legacy_code", "Backbone", "React", "common/button", "common/backbone_mixin", "signview/is_small_view"],
        function (_legacy, Backbone, React, Button, BackboneMixin, isSmallView) {

var MAX_WIDTH = 772;
var LARGEST_WIDTH = 1040;

function isCanvasBlank(canvas) {
  var blank = document.createElement("canvas");
  blank.width = canvas.width;
  blank.height = canvas.height;
  return canvas.toDataURL() == blank.toDataURL();
}

var SignatureDrawerModel = Backbone.Model.extend({
  defaults: function () {
    return {
      preview: false,
      delayStartPreview: 3000,
      animationLength: 4400,
      pauseLength: 5000,
      previewNonce: 0,
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
    _.bindAll(this, "cancelPreview");
    var self = this;
    this.set("empty", !this.value());
    var previewImg = new Image();
    previewImg.src = this.previewSrc();
    previewImg.onload = function () {
      if (self.empty()) {
        self.startPreviewTimer();
      }
    };
  },
  canvasWidth: function () {
    var innerWidth = $("body").innerWidth();
    var innerHeight = $("body").innerHeight();
    var width = innerWidth < MAX_WIDTH ? innerWidth : MAX_WIDTH;
    var height = Math.round(width * this.height() / this.width());
    var fullHeight = height + this.footerHeight();

    if (fullHeight > innerHeight) {
      width = Math.round((innerHeight - this.footerHeight()) * this.width() / this.height());
    }

    return width;
  },
  footerHeight: function () {
    return isSmallView() ? 56 : 100;
  },
  height: function () {
     return this.get("height");
  },
  width: function () {
     return this.get("width");
  },
  field: function () {
    return this.get("field");
  },
  empty: function () {
    return this.get("empty");
  },
  setEmpty: function () {
    this.set("empty", true);
    this.startPreviewTimer();
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
  startPreviewTimer: function () {
    var self = this;
    this.set("previewNonce", Date.now());
    this.previewTimeout = setTimeout(function () {
      self.set({preview: true});
      self.set({animation: true});
      self.startAnimationTimer();
    }, self.get("delayStartPreview"));
  },
  startAnimationTimer: function () {
    var self = this;
    self.animationTimeout = setTimeout(function () {
      self.set("animation", false);
      self.animationTimeout = setTimeout(function () {
        self.set("animation", true);
        if (self.get("preview")) {
          self.startAnimationTimer();
        }
      }, self.get("pauseLength"));
    }, self.get("animationLength"));
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
  },
  value: function () {
    return this.field().value();
  },
  showPreview: function () {
    return this.get("preview");
  },
  showAnimation: function () {
    return this.get("animation");
  },
  previewNonce: function () {
    return this.get("previewNonce");
  },
  previewSrc: function () {
    var nonce = this.previewNonce();
    var isTouchDevice = "ontouchstart" in window || "onmsgesturechange" in window;
    // fix for IE 10.
    if (BrowserInfo.isIE() && $.browser.version >= 10.0) {
      isTouchDevice = !window.navigator.msMaxTouchPoints;
    }
    var src = window.cdnbaseurl + (isTouchDevice ? "/img/sign-preview-hand.gif" : "/img/sign-preview-mouse.gif");
    return src + "?" + nonce;
  },
  cancelPreview: function () {
    clearTimeout(this.previewTimeout);
    clearTimeout(this.animationTimeout);
    if (this.get("preview")) {
      this.set({preview: false, animation: false});
    }
  },
  saveImage: function (canvas, callback) {
    if (this.empty()) {
      this.field().setValue("");
      if (typeof callback === "function") {
        callback();
      }
    } else {
      var field = this.field();
      var height = Math.floor(this.canvasWidth() * this.height() / this.width());
      ImageUtil.addTransparentBGAndSerializeCanvas(canvas, this.canvasWidth(), height, function (imageData) {
        field.setValue(imageData);
        if (typeof callback === "function") {
          callback();
        }
      });
    }
  }
});

return React.createClass({
    _canvasBuffer: null,
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels: function () {
      return [this.state.model];
    },
    propTypes: {
      field: React.PropTypes.object,
      width: React.PropTypes.number,
      height: React.PropTypes.number,
      acceptText: React.PropTypes.string,
      onClose: React.PropTypes.func,
      onAccept:  React.PropTypes.func,
      onStartDrawing: React.PropTypes.func,
      onStopDrawing: React.PropTypes.func
    },
    getInitialState: function () {
      var model =  new SignatureDrawerModel({
        field: this.props.field,
        width:  this.props.width,
        height: this.props.height
      });
      return {
        model: model,
        show: false
      };
    },
    componentDidMount: function () {
      var self = this;
      this._canvasBuffer = document.createElement("canvas");
      this._canvasBuffer.width = 772;
      this._canvasBuffer.height = 303;
      var picture =  this.refs.canvas.getDOMNode().getContext("2d");
      var canvasWidth = this.state.model.canvasWidth();
      if (this.state.model.value() && this.state.model.value() != "") {
        var img = new Image();
        img.type = "image/png";
        img.src =  this.state.model.value();
        var imageHeight = canvasWidth * this.state.model.height() / this.state.model.width();
        picture.drawImage(img, 0, 0, canvasWidth, imageHeight);
      }
      this.setState({picture: picture, show: true});
      this.initDrawing();
      $(window).on("resize", this.handleResize);
      $(window).on("gestureend", this.handleResize);
      if ("ontouchstart" in document.documentElement) {
        $(document)[0].addEventListener("touchend", this.mouseupCallback);
      } else if (navigator.msPointerEnabled) {
        $(document)[0].addEventListener("MSPointerUp", this.mouseupCallback);
      }
      $(document).mouseup(self.mouseupCallback);
      this.dumpPictureToBuffer();
    },
    componentWillUnmount: function () {
      $(window).off("resize", this.handleResize);
      $(window).off("gestureend", this.handleResize);
      if ("ontouchstart" in document.documentElement) {
        $(document)[0].removeEventListener("touchend", this.mouseupCallback);
      } else if (navigator.msPointerEnabled) {
        $(document)[0].removeEventListener("MSPointerUp", this.mouseupCallback);
      }
      $(document).off("mouseup", this.mouseupCallback);
    },
    handleResize: function (e) {
      this.forceUpdate();
    },
    componentDidUpdate: function () {
      var canvas = this.refs.canvas;
      var buffer = this._canvasBuffer;

      if (canvas && isCanvasBlank(canvas.getDOMNode()) && !isCanvasBlank(buffer)) {
        this.restorePictureFromBuffer();
      }
    },
    startDrawing: function (drawingMethod, pointerId) {
      this.state.model.startDrawing(drawingMethod, pointerId);
      this.props.onStartDrawing();
    },
    clearBuffer: function () {
      var ctx = this._canvasBuffer.getContext("2d");
      ctx.clearRect(0, 0, this._canvasBuffer.width, this._canvasBuffer.height);
    },
    dumpPictureToBuffer: function () {
      var ctx = this._canvasBuffer.getContext("2d");
      this.clearBuffer();
      ctx.drawImage(this.refs.canvas.getDOMNode(), 0, 0, this._canvasBuffer.width, this._canvasBuffer.height);
    },
    restorePictureFromBuffer: function () {
      var canvas = this.refs.canvas;

      if (canvas) {
        var width = canvas.props.width;
        var height = canvas.props.height;
        this.state.picture.drawImage(this._canvasBuffer, 0, 0, width, height);
      }
    },
    stopDrawing: function () {
      this.saveImage();
      this.state.model.stopDrawing();
      this.props.onStopDrawing();
      this.dumpPictureToBuffer();
    },
    mouseupCallback: function () {
      if (this.state.model.drawingInProgress()) {
        this.stopDrawing();
      }
    },
    initDrawing: function () {
      var self = this;
      var drawingArea = this.refs.drawingArea.getDOMNode();
      var drawing = function (fn, type) {
        return function (e) {
          e.preventDefault();
          e.stopPropagation();
          e.target.style.cursor = "default";
          fn(self.xPos(e), self.yPos(e), type, e);
          return false;
        };
      };

      if ("ontouchstart" in document.documentElement) {
        drawingArea.addEventListener("touchstart", drawing(self.drawingtoolDown, "touch"));
        drawingArea.addEventListener("touchmove", drawing(self.drawingtoolMove, "touch"));
        drawingArea.addEventListener("touchend", drawing(self.drawingtoolUp, "touch"));
      } else if (navigator.msPointerEnabled) {
        drawingArea.addEventListener("MSPointerDown", drawing(self.drawingtoolDown, "ms"), true);
        drawingArea.addEventListener("MSPointerMove", drawing(self.drawingtoolMove, "ms"), true);
        drawingArea.addEventListener("MSPointerUp", drawing(self.drawingtoolUp, "ms"), true);
      }

      $(drawingArea).mousedown(drawing(self.drawingtoolDown, "mouse"));
      $(drawingArea).mousemove(drawing(self.drawingtoolMove, "mouse"));
      $(drawingArea).mouseup(drawing(self.drawingtoolUp, "mouse"));
      $(drawingArea).mouseenter(drawing(self.drawingtoolInside, "mouse"));
      $(drawingArea).mouseout(drawing(self.drawingtoolOutside, "mouse"));
    },
    xPos: function (e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) {
        e = e.changedTouches[0];
      }
      var canvasLeft = $(this.refs.drawingArea.getDOMNode()).offset().left;
      /*
       * There is a problem with modern IE on touch devices and using jQuery's offset:
       * http://connect.microsoft.com/IE/feedback/details/768781/ie10-window-pageyoffset-incorrect-value-when-page-zoomed-breaks-jquery-etc
       * http://bugs.jquery.com/ticket/14742 ('cantfix') since browser don't expose zoom information through an API
       */
      if (BrowserInfo.isIETouch()) {
        /* pageXOffset (used in jquery offset()) changes when zoomed on MS Touch devices,
         * whereas scrollLeft is constant. */
        canvasLeft -= window.pageXOffset;
        canvasLeft += document.documentElement.scrollLeft;
      } else if (BrowserInfo.isAndroid() && BrowserInfo.isChrome()) {
        /* Similar issue on android, but only for XOffset and only when zoomed.
         * Note that Chrome on Android has many other issues when zoomed, and they are not solveable right now,
         * since offsetY computation is broken on this platform.
         */
        canvasLeft -= window.pageXOffset;
      }
      return e.pageX - canvasLeft;
    },
    yPos: function (e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) {
        e = e.changedTouches[0];
      }
      var canvasTop = $(this.refs.drawingArea.getDOMNode()).offset().top;
      if (BrowserInfo.isIETouch()) {
        // Same fallback as for xPos
        canvasTop -= window.pageYOffset;
        canvasTop += document.documentElement.scrollTop;
      }
      return e.pageY - canvasTop;
    },
    eventPointerId: function (e) {
      if (e.pointerId !== undefined) {
        return e.pointerId;
      } else if (e.changedTouches !== undefined) {
        return e.changedTouches[0].identifier;
      } else {
        return undefined;
      }
    },
    isLeftMouseButton: function (drawingMethod, e) {
      if (drawingMethod !== "mouse" && drawingMethod !== "ms") {
        // other drawing methods don't have buttons
        return true;
      } else if (!BrowserInfo.isIE9orLower() && e.buttons !== undefined) {
        // 1 is "left" click
        return e.buttons === 1;
      } else if ((BrowserInfo.isSafari() || BrowserInfo.isIE9orLower()) && e.which !== undefined) {
        // 1 is "left" click
        return e.which === 1;
      } else {
        // this is for other broken browsers - default mode
        // if we don't know which button was pressed,
        // unfortunately we have to assume this was left button click
        // even if it was right and we will draw with right button :(
        return true;
      }
    },
    lineWidth: function () {
      return 8;
    },
    colorForRGBA: function (opacity) {
      return "rgba(27,19,127," + opacity + ")";
    },
    drawDot: function (x, y, color, radius) {
      this.state.picture.fillStyle = color;
      // Use canvas.arc to draw a circle with the diameter of width
      this.state.picture.arc(x, y,  radius / 2, 0,  Math.PI * 2, true);
      this.state.picture.fill();
    },
    drawCircle: function (x, y) {
      this.state.picture.beginPath();
      var radius = this.lineWidth();
      this.drawDot(x, y, this.colorForRGBA(1), radius);
      this.state.picture.closePath();
    },
    drawingtoolDown: function (x, y, drawingMethod, e, eventPointerId) {
      if (!this.state.model.drawingInProgress() && this.isLeftMouseButton(drawingMethod, e)) {
        this.state.model.cancelPreview();

        this.state.model.setStartPoint(x, y);
        if (eventPointerId === undefined) {
          eventPointerId = this.eventPointerId(e);
        }
        this.startDrawing(drawingMethod, eventPointerId);

        this.state.picture.beginPath();
        this.state.picture.moveTo(x, y);
        this.state.picture.lineWidth = this.lineWidth();
        this.state.picture.lineCap = "round";
        this.state.picture.lineJoin = "round";
      }
    },
    drawingtoolOutside: function (x, y, drawingMethod, e) {
      // drawingMethod is always mouse, but initial method could be different (e.g. ms),
      // use the initial method to draw
      if (this.state.model.drawingInProgress()) {
        this.drawingtoolMove(x, y, this.state.model.get("drawingMethod"), e);
        this.drawingtoolMove(x, y, this.state.model.get("drawingMethod"), e);
        this.state.model.setPointerOutside(true);
      }
    },
    drawingtoolInside: function (x, y, drawingMethod, e) {
      if (this.state.model.drawingInProgress() && this.state.model.pointerOutside()) {
        drawingMethod = this.state.model.get("drawingMethod");
        var eventPointerId = this.state.model.get("pointerId");
        this.state.model.setPointerOutside(false);
        this.stopDrawing();
        // start drawing unconditionally, because if button was depressed
        // drawing was already stopped, and we would not get to this point
        this.drawingtoolDown(x, y, drawingMethod, e, eventPointerId);
      }
    },
    drawingtoolMove: function (x, y, drawingMethod, e) {
      // we have to check for pointerOutside, because <=IE10 is retarded,
      // and fires a few mousemove events before firing mouseenter event
      if (this.state.model.drawingInProgressWithDrawingMethodAndPointerId(drawingMethod, this.eventPointerId(e)) &&
          !this.state.model.pointerOutside()) {
        var x_ = this.state.model.x();
        var y_ = this.state.model.y();
        var x__ = this.state.model.x_();
        var y__ = this.state.model.y_();

        var moved = function (x1, x2) { return (x1 * 2 + x2 * 1) / 3; };
        if (x__ != undefined && y__ != undefined) {
          this.drawNiceCurve(x__, y__, x_, y_, moved(x_, x), moved(y_, y));
          this.drawNiceLine(moved(x_, x), moved(y_, y), moved(x, x_), moved(y, y_));
        } else {
          this.drawNiceLine(x_, y_, moved(x, x_), moved(y, y_));
        }
        this.state.model.lineDrawn(moved(x, x_), moved(y, y_), x, y);
      }
    },
    drawingtoolUp: function (x, y, drawingMethod, e) {
      if (this.state.model.drawingInProgressWithDrawingMethodAndPointerId(drawingMethod, this.eventPointerId(e))) {
        this.state.picture.lineTo(x, y);
        this.state.picture.closePath();
        if (!this.state.model.drawnAnyLine()) {
          this.drawCircle(x, y);
        }
        this.stopDrawing();
      }
    },
    drawNiceLine: function (sx, sy, ex, ey) {
      this.state.picture.closePath();
      this.state.picture.beginPath();
      this.drawLine(sx, sy, ex, ey, this.lineWidth() + 1, this.colorForRGBA(0.05), "butt");
      this.drawLine(sx, sy, ex, ey, this.lineWidth(),     this.colorForRGBA(0.3), "round");
      this.drawLine(sx, sy, ex, ey, this.lineWidth() - 1, this.colorForRGBA(0.5), "round");
      this.drawLine(sx, sy, ex, ey, this.lineWidth() - 2, this.colorForRGBA(1), "round");
    },
    drawNiceCurve: function (sx, sy, cx, cy, ex, ey) {
      this.state.picture.closePath();
      this.state.picture.beginPath();
      this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() + 1, this.colorForRGBA(0.05), "butt");
      this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth(),     this.colorForRGBA(0.3), "round");
      this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() - 1, this.colorForRGBA(0.5), "round");
      this.drawCurve(sx, sy, cx, cy, ex, ey, this.lineWidth() - 2, this.colorForRGBA(1), "round");
    },
    drawCurve: function (sx, sy, cx, cy, ex, ey, w, c, lc) {
      this.state.picture.moveTo(sx, sy);
      this.state.picture.strokeStyle = c;
      this.state.picture.lineWidth = w;
      this.state.picture.lineCap = lc;
      this.state.picture.quadraticCurveTo(cx, cy, ex, ey);
      this.state.picture.stroke();
    },
    drawLine: function (sx, sy, ex, ey, w, c, lc) {
      this.state.picture.moveTo(sx, sy);
      this.state.picture.strokeStyle = c;
      this.state.picture.lineWidth = w;
      this.state.picture.lineCap = lc;
      this.state.picture.lineTo(ex, ey);
      this.state.picture.stroke();
    },
    previewSrc: function () {
      var nonce = this.state.model.previewNonce();
      var isTouchDevice = "ontouchstart" in window || "onmsgesturechange" in window;
      // fix for IE 10.
      if (BrowserInfo.isIE() && $.browser.version >= 10.0) {
        isTouchDevice = window.navigator.msMaxTouchPoints;
      }
      var src = isTouchDevice ? "/img/sign-preview-hand.gif" : "/img/sign-preview-mouse.gif";
      return src + "?" + nonce;
    },
    saveImage: function (callback) {
      this.state.model.saveImage(this.refs.canvas.getDOMNode(), callback);
    },
    clear: function () {
      var model = this.state.model;
      var canvasWidth = model.canvasWidth();
      var drawingCanvasHeight = Math.round(canvasWidth * model.height() / model.width());
      this.state.picture.clearRect(0, 0, canvasWidth, drawingCanvasHeight);
      this.clearBuffer();
      model.setEmpty();
      this.saveImage();
    },
    render: function () {
      var self = this;
      var model = this.state.model;
      var text = localization.signviewDrawSignatureHere;
      var canvasWidth = model.canvasWidth();

      var bodyWidth = $("body").innerWidth();
      var bodyHeight = $("body").innerHeight();

      var canvasHeight = Math.round(canvasWidth * model.height() / model.width());
      var contentHeight = canvasHeight + model.footerHeight();

      var left = (bodyWidth - canvasWidth) / 2;
      var top = (bodyHeight - contentHeight) / 2;

      left = left < 0 ? 0 : left;
      top = top < 0 ? 0 : top;

      var contentStyle = {left: left + "px", width: canvasWidth + "px"};
      if (bodyWidth < LARGEST_WIDTH || contentHeight >= bodyHeight) {
        contentStyle.bottom = 0;
      } else {
        contentStyle.top = top;
      }

      if (canvasWidth >= bodyWidth) {
        contentStyle.left = 0;
        contentStyle.bottom = 0;
      }

      var backgroundStyle = {
        width: canvasWidth + "px",
        height: canvasHeight + "px"
      };

      var instructionStyle = {
        opacity: model.empty() && !model.showAnimation() ? "1" : "0"
      };

      return (
        <div style={contentStyle} className="content">
          <div style={backgroundStyle} ref="drawingArea" className="drawing-area">
            <div style={backgroundStyle} className="background">
              <img
                ref="preview"
                className="preview"
                style={{display: model.showPreview() ? "block" : "none"}}
                src={model.previewSrc()}
              />
              <div className="instruction">
                <h1 style={instructionStyle}>{text}</h1>
                <hr />
              </div>
            </div>
            <canvas
              ref="canvas"
              className="canvas"
              width={canvasWidth}
              height={canvasHeight}
            />
          </div>
          <div>
            <div className="footer">
              <Button
                type="action"
                text={self.props.acceptText}
                className={"next " + (model.empty() ? "inactive" : "")}
                onClick={function () {
                  if (!model.empty()) {
                    self.saveImage(function () {
                      self.setState({show: false});
                      self.props.onAccept();
                    });
                  }
                }}
              />
              <Button
                className="transparent-button cancel-clear"
                text={model.empty() ? localization.cancel : localization.pad.cleanImage}
                onClick={function () {
                  if (model.empty()) {
                    self.setState({show: false});
                    self.props.onClose();
                  } else {
                    self.clear();
                  }
                }}
              />
              <div className="clearfix" />
            </div>
          </div>
        </div>
      );
    }
});

});
