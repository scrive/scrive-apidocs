/* Content of modal for drawing signature */

define(["legacy_code", "Backbone", "React", "common/button", "common/backbone_mixin"],
        function (_legacy, Backbone, React, Button, BackboneMixin) {

/* SignatureDrawerModel holds reference to field and dimentions, as well as some
 * temporary data connected with drawing
 *
 * Drawing data includes things like - current point position (x,y), previous point position (x_,y_)
 * information if anything was drawn (not detect if image is empty), information if any line was
 * drawn in current session, information if drawing is in progress and what 'tool' is used for that.
 *
 * Drawing data is stored here to limit direct access to it. Only functions that can modify drawing data are:
 *
 * startDrawing(drawingMethod)  - Start drawing with drawingMethod
 * stopDrawing()                - Stop drawing
 * setStartPoint(x,y)           - Started drawing at point (x,y)
 * lineDrawn(x_,y_,x,y)         - Curve was drawn till point (x,y). Middle of curve was (x_,y_).
 * setEmpty()                   - Used after "clear" operation
 *
 * All functions modifying drawing data may be silent, since rerendering of view is not needed.
 *
 * NOTE: We are not drawing lines, but curves. This is why x_,y_ are there and are position of middle of last curve.
 *
 */
var SignatureDrawerModel = Backbone.Model.extend({
  defaults: function () {
    return {
      preview: false,
      delayStartPreview: 3000,
      empty: false, // If something was drawn on canvas
      drawing: false, // Is drawing in progress
      drawingMethod: undefined, // What tool is used for drawing (mouse or touch)
      drawnAnyLine: false, // Drawn line durring current drawing
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
    if (self.empty()) {
      this.previewTimeout = setTimeout(function () {
        self.set({preview: true});
      }, self.get("delayStartPreview"));
    }
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
  },
  drawingInProgress: function () {
    return this.get("drawing");
  },
  drawingInProgressWithDrawingMethod: function (method) {
    return this.get("drawing") && this.get("drawingMethod") == method;
  },
  drawnAnyLine: function () {
    return this.get("drawnAnyLine");
  },
  // All changes that are happending while drawing are silent, since we don't need to rerender
  // view for performance reasons.
  startDrawing: function (drawingMethod) {
    this.set({
      drawing: true,
      drawingMethod: drawingMethod
    }, {silent: true});
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
  value: function () {
    return this.field().value();
  },
  showPreview: function () {
    return this.get("preview");
  },
  cancelPreview: function () {
    clearTimeout(this.previewTimeout);
    if (this.get("preview")) {
      this.set("preview", false);
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
      var height = Math.floor(820 * this.height() / this.width());
      ImageUtil.addTransparentBGAndSerializeCanvas(canvas, 820, height, function (imageData) {
        field.setValue(imageData);
        if (typeof callback === "function") {
          callback();
        }
      });
    }
  }
});

return React.createClass({
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
        model: model
      };
    },
    componentDidMount: function () {
      var picture =  this.refs.canvas.getDOMNode().getContext("2d");
      if (this.state.model.value() && this.state.model.value() != "") {
        var img = new Image();
        img.type = "image/png";
        img.src =  this.state.model.value();
        picture.drawImage(img, 0, 0, 820, 820 * this.state.model.height() / this.state.model.width());
      }
      this.setState({picture: picture});
      this.initDrawing();
    },
    startDrawing: function (drawingMethod) {
      this.state.model.startDrawing(drawingMethod);
      this.props.onStartDrawing();
    },
    stopDrawing: function () {
      this.state.model.stopDrawing();
      this.props.onStopDrawing();
    },
    initDrawing: function () {
      var self = this;
      var drawingArea = this.refs.drawingArea.getDOMNode();
      var drawing = function (fn) {
        return function (type) {
          return function (e) {
            e.preventDefault();
            e.stopPropagation();
            e.target.style.cursor = "default";
            fn(type, e);
            return false;
          };
        };
      };

      var methods = {
        start: drawing(function (type, e) {
          self.drawingtoolDown(self.xPos(e), self.yPos(e), type);
        }),
        move: drawing(function (type, e) {
          self.drawingtoolMove(self.xPos(e), self.yPos(e), type);
        }),
        end: drawing(function (type, e) {
          self.drawingtoolUp(self.xPos(e), self.yPos(e), type);
        })
      };

      if ("ontouchstart" in document.documentElement) {
        drawingArea.addEventListener("touchstart", methods.start("touch"));
        drawingArea.addEventListener("touchmove", methods.move("touch"));
        drawingArea.addEventListener("touchend", methods.end("touch"));
      } else if (navigator.msPointerEnabled) {
        drawingArea.addEventListener("MSPointerDown", methods.start("ms"), true);
        drawingArea.addEventListener("MSPointerMove", methods.move("ms"), true);
        drawingArea.addEventListener("MSPointerUp", methods.end("ms"), true);
      }

      $(drawingArea).mousedown(methods.start("mouse"));
      $(drawingArea).mousemove(methods.move("mouse"));
      $(drawingArea).mouseup(methods.end("mouse"));
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
    drawingtoolDown: function (x, y, drawingMethod) {
      if (!this.state.model.drawingInProgress()) {
        this.state.model.cancelPreview();

        this.state.model.setStartPoint(x, y);
        this.startDrawing(drawingMethod);

        this.state.picture.beginPath();
        this.state.picture.moveTo(x, y);
        this.state.picture.lineWidth = this.lineWidth();
        this.state.picture.lineCap = "round";
        this.state.picture.lineJoin = "round";
      }
    },
    drawingtoolMove: function (x, y, drawingMethod) {

      if (this.state.model.drawingInProgressWithDrawingMethod(drawingMethod)) {

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
    drawingtoolUp: function (x, y, drawingMethod) {
      if (this.state.model.drawingInProgressWithDrawingMethod(drawingMethod)) {
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
      var isTouchDevice = "ontouchstart" in window || "onmsgesturechange" in window;
      // fix for IE 10.
      if (BrowserInfo.isIE() && $.browser.version >= 10.0) {
        isTouchDevice = !window.navigator.msMaxTouchPoints;
      }
      return !isTouchDevice ? "/img/sign-preview-hand.gif" : "/img/sign-preview-mouse.gif";
    },
    previewStyle: function () {
      var model = this.state.model;
      var ratio  = model.height() / model.width();
      var height = 820 * ratio;
      if (height > 323) {
        return {
          width: "820px",
          height: "323px",
          marginTop: ((height - 323) / 2) + "px",
          display: (model.showPreview() ? "block" : "none")
        };
      } else {
        return {
          width: (820 / ratio) + "px",
          height: height + "px",
          marginRight: ((820 - (820 / ratio)) / 2) + "px",
          display: (model.showPreview() ? "block" : "none")
        };
      }
    },
    saveImage: function (callback) {
      this.state.model.saveImage(this.refs.canvas.getDOMNode(), callback);
    },
    clear: function () {
      this.state.picture.clearRect(0, 0, 820, 820 * this.state.model.height() / this.state.model.width());
      this.state.model.setEmpty();
    },
    render: function () {
      var self = this;
      var model = this.state.model;
      return (
        <div>
          <div className="header" style={{textAlign:"left", margin: "25px 39px 25px 39px"}}>
            <div style={{fontSize:"28px", lineHeight: "32px"}}>
              {localization.pad.drawSignatureBoxHeader}
            </div>
            <div/>
            <a className='modal-close' onClick={function () {self.props.onClose()}}/>
          </div>
          <div className="signatureDrawingBoxWrapper" style={{width: "820px", borderColor: "#7A94B8"}}>
            <div
              ref="drawingArea"
              className="signatureDrawingBox"
              style={{height: (820 * model.height() / model.width()) + "px"}}
            >
              <canvas
                ref="canvas"
                className="signatureCanvas"
                width={820}
                height={820 * model.height() / model.width()}
                style={{
                  width:"820px",
                  height: (820 * model.height() / model.width()) + "px",
                  display: (model.showPreview() ? "none" : "block")
                }}
              />
              <img
                ref="preview"
                className="signatureCanvas"
                src={self.previewSrc()}
                style={self.previewStyle()}
              />
            </div>
          </div>
          <div>
            <div  className="modal-footer">
              <Button
                type="action"
                size="small"
                className="bottom-button accept-button"
                text={self.props.acceptText}
                onClick={function () {
                  self.saveImage(function () {
                    self.props.onAccept();
                  });
                }}
              />
              <label
                className="delete"
                style={{float:"left", marginRight:"20px", lineHeight: "40px"}}
                onClick={function () {
                  self.props.onClose();
                }}
              >
                {localization.cancel}
              </label>
              <Button
                size="small"
                style={{float:"left", marginTop: "-2px;"}}
                text={localization.pad.cleanImage}
                onClick={function () {
                  self.clear();
                }}
              />
            </div>
          </div>
        </div>
      );
    }
});

});
