var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var BackboneMixin = require("../../common/backbone_mixin");
var ViewSize = require("../viewsize");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;
var ImageUtil = require("../../../js/utils/image.js").ImageUtil;
var DrawingState = require("../drawing/drawingstate");
var DrawingUtils = require("../drawing/drawingutils");
var Drawing = require("../drawing/drawing");

/* Content of modal for drawing signature */

var MAX_WIDTH = 772;
var LARGEST_WIDTH = 1040;

var SignatureDrawerModel = Backbone.Model.extend({
  defaults: function () {
    return {
      preview: false,
      delayStartPreview: 3000,
      animationLength: 4400,
      pauseLength: 5000,
      previewNonce: 0
    };
  },
  initialize: function () {
    _.bindAll(this, "cancelPreview");
    var self = this;
    this.set("drawingstate", new DrawingState({empty: !this.value()}));
    this.listenTo(this.drawingState(), "change", function () { self.trigger("change"); });
    var previewImg = new Image();
    previewImg.src = this.previewSrc();
    previewImg.onload = function () {
      if (self.drawingState().empty()) {
        self.startPreviewTimer();
      }
    };
  },
  drawingState: function () {
    return this.get("drawingstate");
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
    return ViewSize.isSmall() ? 56 : 100;
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
    console.log("Clearing timeout");
    clearTimeout(this.previewTimeout);
    clearTimeout(this.animationTimeout);
    if (this.get("preview")) {
      this.set({preview: false, animation: false});
    }
  },
  saveImage: function (canvas, callback) {
    if (this.drawingState().empty()) {
      this.field().setValue("");
      if (typeof callback === "function") {
        callback();
      }
    } else {
      var field = this.field();
      var height = Math.floor(this.canvasWidth() * this.height() / this.width());
      ImageUtil.addTransparentBGAndSerializeCanvas(canvas, this.canvasWidth(), height, 1, function (imageData) {
        field.setValue(imageData);
        if (typeof callback === "function") {
          callback();
        }
      });
    }
  }
});

module.exports = React.createClass({
    canvas: undefined,
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
      onAccept: React.PropTypes.func,
      onStartDrawing: React.PropTypes.func,
      onStopDrawing: React.PropTypes.func
    },
    getInitialState: function () {
      var model =  new SignatureDrawerModel({
        field: this.props.field,
        width: this.props.width,
        height: this.props.height
      });
      return {
        model: model,
        show: false
      };
    },
    componentDidMount: function () {
      var self = this;
      var canvasBox = this.refs.canvasBox;
      var canvasWidth = this.canvasWidth();
      var canvasHeight =  this.canvasHeight();
      this.canvas = $("<canvas class='canvas'/>").attr("width", canvasWidth).attr("height", canvasHeight);
      $(canvasBox.getDOMNode()).append(this.canvas);
      var picture = this.canvas[0].getContext("2d");
      if (this.state.model.value() && this.state.model.value() != "") {
        var img = new Image();
        img.type = "image/png";
        img.onload = function () {
          self.canvas[0].getContext("2d").drawImage(img, 0, 0, canvasWidth, canvasHeight);
        };
        img.src =  this.state.model.value();
      }
      this.drawing = new Drawing({
        picture: picture,
        drawingState: this.state.model.drawingState(),
        color: "#1B137F",
        lineWidth: 8,
        onStartDrawing: self.startDrawing,
        onStopDrawing: self.stopDrawing
      });

      this.setState({picture: picture, show: true});
      this.drawing.initDrawing(this.refs.drawingArea.getDOMNode());
      $(window).on("resize", this.handleResize);
      $(window).on("gestureend", this.handleResize);
      this.drawing.bindStopDrawingToGlobalHandlers();
    },
    componentWillUnmount: function () {
      $(window).off("resize", this.handleResize);
      $(window).off("gestureend", this.handleResize);
      this.drawing.unbindStopDrawingFromGlobalHandlers();
    },
    handleResize: function (e) {
      if (this.isMounted()) {
        this.forceUpdate();
      }
    },
    componentDidUpdate: function () {
      var self = this;
      var canvas = this.canvas;
      if (canvas.width() != this.canvasWidth() || canvas.height() != this.canvasHeight()) {
        canvas.attr("width", this.canvasWidth()).attr("height", this.canvasHeight());
        var img = new Image();
        img.type = "image/png";
        img.onload = function () {
          canvas[0].getContext("2d").drawImage(img, 0, 0, self.canvasWidth(), self.canvasHeight());
        };
        img.src =  this.state.model.value();
      }
    },
    startDrawing: function (drawingMethod, pointerId) {
      this.state.model.cancelPreview();
      this.props.onStartDrawing();
    },
    stopDrawing: function () {
      this.saveImage();
      this.props.onStopDrawing();
    },
    previewSrc: function () {
      var nonce = this.state.model.previewNonce();
      var isTouchDevice = "ontouchstart" in window || "onmsgesturechange" in window;
      // fix for IE 10.
      if (BrowserInfo.isIE() && $.browser.version >= 10.0) {
        isTouchDevice = window.navigator.msMaxTouchPoints;
      }
      var src = window.cdnbaseurl + (isTouchDevice ? "/img/sign-preview-hand.gif" : "/img/sign-preview-mouse.gif");
      return src + "?" + nonce;
    },
    saveImage: function (callback) {
      this.state.model.saveImage(this.canvas[0], callback);
    },
    clear: function () {
      var model = this.state.model;
      var canvasWidth = model.canvasWidth();
      var drawingCanvasHeight = Math.round(canvasWidth * model.height() / model.width());
      this.state.picture.clearRect(0, 0, canvasWidth, drawingCanvasHeight);
      model.drawingState().setEmpty();
      model.startPreviewTimer();
      this.saveImage();
    },
    canvasWidth: function () {
      return this.state.model.canvasWidth();
    },
    canvasHeight: function () {
      return Math.round(this.state.model.canvasWidth() * this.state.model.height() / this.state.model.width());
    },
    render: function () {
      var self = this;
      var model = this.state.model;
      var text = localization.signviewDrawSignatureHere;
      var canvasWidth = this.canvasWidth();
      var canvasHeight = this.canvasHeight();

      var bodyWidth = $("body").innerWidth();
      var bodyHeight = $("body").innerHeight();

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
        opacity: model.drawingState().empty() && !model.showAnimation() ? "1" : "0"
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
            <div
              ref="canvasBox"
              style={{
                width: canvasWidth + "px",
                height: canvasHeight + "px"
              }}
            />
          </div>
          <div>
            <div className="footer">
              <Button
                type="action"
                text={self.props.acceptText}
                className={"next " + (model.drawingState().empty() ? "inactive" : "")}
                onClick={function () {
                  if (!model.drawingState().empty()) {
                    self.saveImage(function () {
                      self.setState({show: false});
                      self.props.onAccept();
                    });
                  }
                }}
              />
              <Button
                className="transparent-button cancel-clear"
                text={model.drawingState().empty() ? localization.cancel : localization.pad.cleanImage}
                onClick={function () {
                  if (model.drawingState().empty()) {
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
