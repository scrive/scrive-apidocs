import Backbone from "backbone";
import $ from "jquery";
import {BrowserInfo} from "../../../js/utils/browserinfo.js";
import DrawingUtils from "../drawing/drawingutils";
import DrawingState from "../drawing/drawingstate";
import Drawing from "../drawing/drawing";

import {toLessInteropLoader} from '../../common/less_utils.jsx';
import vars_ from '!less-vars-loader!../../../less/signview/vars.less';
const vars = toLessInteropLoader(vars_);

const OPACITY_RATE = vars.highlightingCanvasOpacity;
const DRAWING_COLOR = "#00DED9";
const LINE_WIDTH_BASE = 6;
const CANVAS_WIDTH_BASE = 950;

module.exports = Backbone.Model.extend({
  defaults: function () {
    return {
      canvas: undefined,
      onNewHighlight: function () {},
      baseImageURL: undefined
    };
  },
  initialize: function () {
    var self = this;
    var canvas = self.canvas();
    var picture = canvas.getContext("2d");
    var drawingState = new DrawingState({empty: (self.baseImageURL() == undefined)});
    var drawing =  new Drawing({
      picture: picture,
      drawingState: drawingState,
      color: DRAWING_COLOR,
      lineWidth: LINE_WIDTH_BASE,
      onStartDrawing: function () { self.startDrawing(); },
      onStopDrawing: function () { self.stopDrawing(); }
    });
    drawing.initDrawing(canvas);
    this.set({
      "picture": picture,
      "drawingstate": drawingState,
      "drawing": drawing
    });
    if (self.baseImageURL()) {
      self.downloadAndDrawBaseImage(this.baseImageURL());
    }
  },
  canvas: function () {
    return this.get("canvas");
  },
  picture: function () {
    return this.get("picture");
  },
  drawingstate: function () {
    return this.get("drawingstate");
  },
  drawing: function () {
    return this.get("drawing");
  },
  baseImageURL: function () {
    return this.get("baseImageURL");
  },
  setBaseImageURL: function (v) {
    this.set("baseImageURL", v);
  },
  startDrawing: function () {
    this.drawing().setLineWidth(LINE_WIDTH_BASE * $(this.canvas()).width() / CANVAS_WIDTH_BASE);
  },
  stopDrawing: function () {
    this.get("onNewHighlight")();
  },
  activate: function () {
    this.drawing().setActive(true);
  },
  deactivate: function () {
    this.drawing().setActive(false);
  },
  downloadAndDrawBaseImage: function () {
    var img = new Image();
    var picture = this.picture();
    var canvas = this.canvas();
    img.onload = () => {
      picture.clearRect(0, 0, $(canvas).width(), $(canvas).height());
      picture.fillStyle = "rgba(0, 0, 0, 0)";
      picture.fillRect(0, 0, $(canvas).width(), $(canvas).height());
      picture.drawImage(img, 0, 0, $(canvas).width(), $(canvas).height());
      var imageData = picture.getImageData(0, 0, $(canvas).width(), $(canvas).height());
      var data = imageData.data;
      for (var i = 0; i < data.length; i += 4) {
        data[i + 3] = data[i + 3] / OPACITY_RATE;
      }
      picture.putImageData(imageData, 0, 0);
    };
    img.src = this.baseImageURL();
    },
    clear: function () {
      this.picture().clearRect(0, 0, $(this.canvas()).width(), $(this.canvas()).height());
      this.set({"baseImageURL": undefined});
    },
    bindStopDrawingToGlobalHandlers: function () {
      this.drawing().bindStopDrawingToGlobalHandlers();
    },
    unbindStopDrawingFromGlobalHandlers: function () {
      this.drawing().unbindStopDrawingFromGlobalHandlers();
    }
});
