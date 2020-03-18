import _ from "underscore";
import Backbone from "backbone";
import React from "react";
import $ from "jquery";
import "jquery.documentsize";
import classNames from "classnames";
import {File} from "../../../js/files.js";
import {Document} from "../../../js/documents.js";
import {Submit} from "../../../js/submits.js";
import {ImageUtil} from "../../../js/utils/image.js";
import viewSize from "../viewsize";

import isTouchDevice from "../../common/is_touch_device";
import BackboneMixin from "../../common/backbone_mixin";
import Controls from "./controls";
import ErrorModal from "../errormodal";
import FilePageView from "./filepageview";
import Instructions from "../instructionsview/instructions";

import vars from "../../../less/signview/vars.less";


const ZOOM_STEPS = 4;
const MAX_ZOOM_PIXELS = 2000;
const MAX_PIXEL_WIDTH = 2000;

function zoomPointMiddle () {
  return {x: $.windowWidth() / 2, y: $.windowHeight() / 2};
}

const ZOOM_STYLE = {
  POINT: 1,
  CENTER: 0
};


module.exports = React.createClass({
  _lastHeight: 0,
  _lastScrollLeft: 0,

  displayName: "FileView",

  propTypes: {
    model: React.PropTypes.instanceOf(File).isRequired,
    signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
    dimControls: React.PropTypes.bool.isRequired,
    pixelWidth: React.PropTypes.number.isRequired,
    onStartHighlighting: React.PropTypes.func.isRequired,
    onStopHighlighting: React.PropTypes.func.isRequired
  },

  contextTypes: {
    document: React.PropTypes.instanceOf(Document),
    hideArrow: React.PropTypes.func,
    showArrow: React.PropTypes.func
  },

  mixins: [BackboneMixin.BackboneMixin, React.addons.PureRenderMixin],

  getBackboneModels: function () {
    return [this.props.model];
  },

  getInitialState: function () {
    return {
      images: [],
      zoom: 1,
      zoomStep: 0,
      zoomPoint: {x: 0, y: 0},
      zoomStyle: ZOOM_STYLE.POINT,
      grabbing: false,
      highlighting: false,
      removingHighlighting: false,
      currentPixelWidth: this.props.pixelWidth
    };
  },

  componentWillMount: function () {
    this.props.model.view = this;
    $(window).on("resize", this.handleResize);
  },

  componentWillUnmount: function () {
    this.props.model.view = null;
    $(window).off("resize", this.handleResize);
    $(document).off("keypress", this.handleKeyboardHighlighting);
    clearTimeout(self.resizeTimeout);
  },

  handleResize: function () {
    var self = this;
    clearTimeout(self.resizeTimeout);
    self.resizeTimeout = setTimeout(function () {
      // wait a bit, so window width is updated after orientation change in wkwebview
      self.maybeResetZoom();
      self.forceUpdate();
    }, 100);
  },

  componentDidMount: function () {
    const node = this.refs.pages.getDOMNode();

    node.style.webkitOverflowScrolling = "touch";

    $(document).on("keypress", this.handleKeyboardHighlighting);
  },

  handleKeyboardHighlighting: function (event) {
    if (event.key === "h") {
      if (typeof event.target === "object" && typeof event.target.tagName === "string"
          && event.target.tagName.toLowerCase() === "input") {
        // event bubbled from filling input field, skip it
      } else {
        const doc = this.context.document;
        const currentSig = doc.currentSignatory();
        const currentSigCanHighlight = currentSig.canSign() && currentSig.allowsHighlighting();
        if (currentSigCanHighlight && !this.state.highlighting) {
          this.handleStartHighlight();
        } else if (currentSigCanHighlight && this.state.highlighting) {
          this.handleCancelHighlighting();
        }
      }
    }
  },

  componentWillUpdate: function (nextProps, nextState) {
    const node = this.refs.pages.getDOMNode();
    this._lastHeight = $(node).height();
    this._lastScrollLeft = this.$scroller().scrollLeft();
    if (this.state.zoom !== nextState.zoom) {
      node.style.webkitOverflowScrolling = "auto";
    }
  },

  componentDidUpdate: function (prevProps, prevState) {
    const node = this.refs.pages.getDOMNode();

    var signview = this.props.signview;

    this._windowWidth = this.standardWidth();

    if (this.props.model.pages().length !== this.state.images.length) {
      this.updateImages();
    } else {
      if (this.state.zoom !== prevState.zoom || !_.isMatch(this.state.zoomPoint, prevState.zoomPoint)) {
        node.style.webkitOverflowScrolling = "touch";
        this.scrollZoomX(this.state.zoomPoint.x, prevState.zoom, this.state.zoom);
        this.scrollZoomY(this.state.zoomPoint.y, prevState.zoom, this.state.zoom);
      }

      if (prevState.highlighting && !this.state.highlighting) {
        this.context.showArrow();
      }

      if (!prevState.highlighting && this.state.highlighting) {
        this.context.hideArrow();
      }

      if (prevState.removingHighlighting && !this.state.removingHighlighting) {
        this.context.showArrow();
      }

      if (!prevState.removingHighlighting && this.state.removingHighlighting) {
        this.context.hideArrow();
      }
    }
  },

  // LOADING IMAGES
  ready: function () {
    var model = this.props.model;
    var ready = model.ready() && model.pages().length > 0 &&
      this.state.images.length === model.pages().length &&
      _.all(this.state.images, function (img) { return img.complete; });
    return ready;
  },

  readyFirstPage: function () {
    return this.state.images.length > 0 && this.state.images[0].complete;
  },

  updateImages: function () {
    var self = this;
    var file = self.props.model;
    var fileid = file.fileid();
    var pixelWidth = this.state.currentPixelWidth;

    _.each(self.state.images, function (img) {
      img.removeEventListener("load", self.handleLoad);
    });

    var images = _.map(file.pages(), function (page, index) {
      var pagelink = "/pages/" + fileid  + "/" + page.number() + file.queryPart({"pixelwidth": pixelWidth});
      var img = new Image();
      var callback = function () {
        if (!img.complete) {
          setTimeout(callback, 100);
        } else {
          self.handleLoad(index);
        }
      };
      img.addEventListener("load", callback);
      img.src = pagelink;
      img.addEventListener("error", function () {
        var fakeXHR = {getResponseHeader: () => {return undefined;},
                       status: 0};
        var doc = file.document();
        var details = {"Document ID": doc.documentid(),
                       "Signatory ID": doc.currentSignatory().signatoryid()};
        new ErrorModal(fakeXHR, details);
      });
      return img;
    });

    self.setState({images: images});
  },

  handleLoad: function (index) {
    if (index === 0) {
      this.props.model.trigger("FirstPageReady");
    }

    if (this.ready()) {
      this.forceUpdate();
      this.props.model.trigger("view:ready");
      this.props.model.trigger("change");
      this.props.signview.trigger("change");
    }
  },


  // ZOOMING
  scrollZoomX: function (x, before, after) {
    var lastWidth = this.standardWidth() * before;
    var newWidth = this.standardWidth() * after;
    var widthExtra = newWidth - lastWidth;
    var scrollLeft = this._lastScrollLeft;
    var zoomPointX = scrollLeft + x;
    var docPointX = zoomPointX / lastWidth;
    var offsetX = docPointX * widthExtra;

    if (this.state.zoomStyle === ZOOM_STYLE.POINT) {
      this.$scroller().scrollLeft(scrollLeft + offsetX);
    } else if (this.state.zoomStyle === ZOOM_STYLE.CENTER) {
      this.$scroller().scrollLeft(scrollLeft - ($.windowWidth() / 2) + x + offsetX);
    }

    if (this.state.zoomStep === 0) {
      this.$scroller().scrollLeft(0);
    }
  },

  scrollZoomY: function (y, before, after) {
    var node = this.refs.pages.getDOMNode();
    var offset = $(node).offset();
    var scrollTop = $(window).scrollTop();
    var newHeight = $(node).height();
    var lastHeight = this._lastHeight;
    var heightExtra = newHeight - lastHeight;
    var zoomPointY =  Math.min(lastHeight, Math.max(0, (scrollTop + y) - offset.top));
    var docPointY = zoomPointY / lastHeight;
    var offsetY = docPointY * heightExtra;

    if (this.state.zoomStyle === ZOOM_STYLE.POINT) {
      $(window).scrollTop(scrollTop + offsetY);
    } else if (this.state.zoomStyle === ZOOM_STYLE.CENTER) {
      $(window).scrollTop(scrollTop - ($.windowHeight() / 4) + y + offsetY);
    }
  },

  handleMouseDown: function (e) {
    if (this.canBeGrabbed()) {
      this._clickX = e.pageX;
      this._clickY = e.pageY;
      this.setState({grabbing: true});
    }
  },

  $scroller: function () {
    if (this.refs.scroller) {
      return $(this.refs.scroller.getDOMNode());
    }

    return $(window);
  },

  handleMouseUp: function () {
    this.setState({grabbing: false});
  },

  handleMouseLeave: function () {
    this.setState({grabbing: false});
  },

  handleMouseMove: function (e) {
    if (this.state.grabbing) {
      var diffX = this._clickX - e.pageX;
      var diffY = this._clickY - e.pageY;
      this.$scroller().scrollLeft(this.$scroller().scrollLeft() + diffX);
      $(window).scrollTop($(window).scrollTop() + diffY);
      this._clickX -= diffX; // Compensate due to scrolling div.
    }
  },

  canBeGrabbed: function () {
    return !this.state.grabbing;
  },

  maybeResetZoom: function () {
    // ratio of width change
    var delta = Math.abs((this.standardWidth() - this._windowWidth) / this._windowWidth);
    // if width changed by more than 10% it was probably an orientation change
    // so let's reset zoom level
    if (delta > 0.1 && delta < 0.9) {
      this.setState({zoom: 1, zoomStep: 0});
    } else if (!this.isViewportSize() && this.state.zoom !== 1) {
      this.setState({zoom: 1, zoomStep: 0});
    } else if (this.state.zoom * this.standardWidth() > MAX_ZOOM_PIXELS) {
      this.setState({zoom: MAX_ZOOM_PIXELS / this.standardWidth()});
    }
  },

  zoomStepSize: function () {
    const diff = MAX_ZOOM_PIXELS - this.standardWidth();
    const step = (diff / ZOOM_STEPS) / this.standardWidth();
    return parseFloat(step.toFixed(4));
  },

  zoomStepFromZoom: function (zoom) {
    var step = 1;
    for (var i = 0; i <= ZOOM_STEPS; i++) {
      if (zoom < step) {
        return i;
      }
      step += this.zoomStepSize();
    }

    return ZOOM_STEPS;
  },

  width: function () {
    return Math.round(this.standardWidth() * this.state.zoom);
  },

  standardWidth: function () {
    if (this.isViewportSize()) {
      return $.windowWidth();
    } else {
      return vars.signviewLargeView;
    }
  },

  isViewportSize: function () {
    return $.windowWidth() < vars.signviewLargeView;
  },

  zoomToPoint: function (zoomPoint, zoom) {
    var zoomStep = this.zoomStepFromZoom(zoom);
    zoom = 1 + (zoomStep * this.zoomStepSize()); // zoom to closes zoom step.
    if (zoom < this.state.zoom) {
      zoom = this.state.zoom;
      zoomStep = this.state.zoomStep;
    }
    this.setState({zoomStyle: ZOOM_STYLE.CENTER, zoom, zoomPoint, zoomStep});
  },

  handleZoomIn: function () {
    const nextWidth = this.standardWidth() * (this.state.zoom + this.zoomStepSize());
    const nextZoomStep = this.state.zoomStep + 1;
    const newState = {zoomStyle: ZOOM_STYLE.POINT, zoomStep: nextZoomStep, zoomPoint: zoomPointMiddle()};
    if (nextWidth < MAX_ZOOM_PIXELS) {
      newState.zoom = this.state.zoom + this.zoomStepSize();
    } else {
      newState.zoom = MAX_ZOOM_PIXELS / this.standardWidth();
    }
    if (this.state.currentPixelWidth != MAX_PIXEL_WIDTH && newState.zoomStep >= ZOOM_STEPS - 1) {
      newState.currentPixelWidth = MAX_PIXEL_WIDTH;
      this.setState(newState, function () {
        this.updateImages();
      });
    } else {
      this.setState(newState);
    }
  },

  handleZoomOut: function () {
    const nextZoomStep = this.state.zoomStep - 1;
    const nextZoom = this.state.zoom - this.zoomStepSize();
    const newState = {zoomStyle: ZOOM_STYLE.POINT, zoomStep: nextZoomStep, zoomPoint: zoomPointMiddle()};
    if (nextZoom > 1) {
      newState.zoom = nextZoom;
    } else {
      newState.zoom = 1;
    }
    this.setState(newState);
  },

  // HIGHLIGHTING
  handleStartHighlight: function () {
    this.setState({highlighting: true});
    this.props.onStartHighlighting();
  },

  handleNewHighlight: function (pageno, canvas) {
    var doc = this.context.document;
    var sig = doc.currentSignatory();
    if (sig.highlightedPages().length === 0) {
      sig.markThatWillGetHighlighedPageSoon();
    }
    ImageUtil.addTransparentBGAndSerializeCanvas(
      canvas, $(canvas).width(),
      $(canvas).height(), vars.highlightingCanvasOpacity,
      function (imageData) {
        doc.setHighlight(pageno, imageData).send();
      }
    );
  },

  handleCancelHighlighting: function () {
    this.setState({highlighting: false});
    this.props.onStopHighlighting();
  },

  handleStartRemovingHighlighting: function () {
    this.setState({removingHighlighting: true});
  },

  handleCancelRemoveHighlighting: function () {
    this.setState({removingHighlighting: false});
  },

  handleRemoveHighlighting: function (pageno) {
    this.context.document.currentSignatory().removeHighlightedPage(pageno);
    this.context.document.setHighlight(pageno, null).send();
    this.setState({removingHighlighting: false});
  },

  blinkControls: function () {
    this.refs.controls.blink();
  },

  render: function () {
    const self = this;
    const viewportWidth = $.windowWidth();
    const file = this.props.model;
    const doc = this.context.document;
    const currentSig = doc.currentSignatory();
    const currentSigCanHighlight = currentSig.canSign() && currentSig.allowsHighlighting();
    const currentSigHasHighlight = currentSig.highlightedPages().length > 0 || currentSig.willGetHighlighedPageSoon();

    const authorName = doc.author() && doc.author().name() ? doc.author().name() : "Author";
    const ready = file.ready();

    const sectionClass = classNames("document-pages", {
      "grab": this.canBeGrabbed(),
      "grabbing": this.state.grabbing
    });

    const scrollerStyle = {
      width: $.windowWidth(),
      overflowX: (isTouchDevice() && !this.state.highlighting) ? "scroll" : "hidden"
    };

    return (
      <div>
        <Controls
          ref="controls"
          highlightingMode={this.state.highlighting}
          removingHighlightingMode={this.state.removingHighlighting}
          zoom={this.state.zoom}
          dim={this.props.dimControls}
          canZoomIn={this.state.zoomStep < ZOOM_STEPS && this.isViewportSize()}
          canZoomOut={this.state.zoomStep > 0}
          canHighlight={currentSigCanHighlight}
          canRemoveHighlighting={currentSigCanHighlight && currentSigHasHighlight}
          onZoomIn={this.handleZoomIn}
          onZoomOut={this.handleZoomOut}
          onStartHighlighting={this.handleStartHighlight}
          onCancelHighlighting={this.handleCancelHighlighting}
          onStartRemovingHighlighting={this.handleStartRemovingHighlighting}
          onCancelRemoveHighlighting={this.handleCancelRemoveHighlighting}
        />
        <div ref="scroller" className="scroller" style={scrollerStyle}>
          <div
            className={sectionClass}
            onMouseDown={this.handleMouseDown}
            onMouseUp={this.handleMouseUp}
            onMouseLeave={this.handleMouseLeave}
            onMouseMove={this.handleMouseMove}
            style={{width: this.width()}}
          >
            <Instructions model={doc} />
            <div className="place-for-arrows" />
            <div ref="pages">
              {/* else */ ready &&
                <div>
                  {_.map(file.pages(), function (page, index) {
                    if (self.state.images[index]) {
                      return (
                          <FilePageView
                          key={"file_" + index}
                          filepage={page}
                          document={self.context.document}
                          signview={self.props.signview}
                          image={self.state.images[index]}
                          width={self.width()}
                          highlightingMode={self.state.highlighting}
                          removingHighlightingMode={self.state.removingHighlighting}
                          onNewHighlight={self.handleNewHighlight}
                          onRemoveHighlighting={self.handleRemoveHighlighting}
                        />
                      );
                    }
                  })}
                </div>
              }
            </div>
          </div>
        </div>
      </div>
    );
  }
});
