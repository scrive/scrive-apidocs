import React from "react";
import ZoomControls from "./zoomcontrols";
import vars from "../../../less/signview/vars.less";
import isTouchDevice from "../../common/is_touch_device";
import _ from "underscore";

const ZOOM_STEPS = 4;
const MAX_ZOOM_PIXELS = 2000;
const MAX_WIDTH_SHOW_CONTROLS = 1200;

function zoomPointMiddle() {
  return {x: $(window).width() / 2, y: $(window).height() / 2};
}

const ZOOM_STYLE = {
  POINT: 1,
  CENTER: 0
};

module.exports = React.createClass({
  _lastHeight: 0,
  _lastScrollLeft: 0,

  displayName: "PageViewer",

  propTypes: {
    ready: React.PropTypes.bool.isRequired,
    showOverlay: React.PropTypes.bool.isRequired,
    showArrow: React.PropTypes.bool.isRequired
  },

  mixins: [React.addons.PureRenderMixin],

  getInitialState: function () {
    return {zoom: 1, zoomStep: 0, zoomPoint: {x: 0, y: 0}, zoomStyle: ZOOM_STYLE.POINT, grabbing: false};
  },

  componentWillMount: function () {
    $(window).on("resize", this.maybeResetZoom);
  },

  componentDidMount: function () {
    const node = this.getDOMNode();

    node.style.webkitOverflowScrolling = "touch";

    node.addEventListener("gestureend", (e) => {
      this.refs.zoomControls.blink(3);
    }, false);

    node.addEventListener("touchstart", (e) => {
      if (e.touches.length > 1) {
        this.refs.zoomControls.blink(3);
      }
    }, false);
  },

  componentWillUpdate: function (nextProps, nextState) {
    const node = this.getDOMNode();
    this._lastHeight = $(node).height();
    this._lastScrollLeft = this.$scroller().scrollLeft();
    if (this.state.zoom !== nextState.zoom) {
      node.style.webkitOverflowScrolling = "auto";
    }
  },

  componentDidUpdate: function (prevProps, prevState) {
    const node = this.getDOMNode();

    if (this.state.zoom !== prevState.zoom || !_.isMatch(this.state.zoomPoint, prevState.zoomPoint)) {
      node.style.webkitOverflowScrolling = "touch";
      this.scrollZoomX(this.state.zoomPoint.x, prevState.zoom, this.state.zoom);
      this.scrollZoomY(this.state.zoomPoint.y, prevState.zoom, this.state.zoom);
    }
  },

  maybeResetZoom: function () {
    if (!this.isViewportSize() && this.state.zoom !== 1) {
      this.setState({zoom: 1});
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
      return $(window).width();
    } else {
      return vars.signviewLargeView;
    }
  },

  isViewportSize: function () {
    return $(window).width() < vars.signviewLargeView;
  },

  zoomToPoint: function (zoomPoint, zoom) {
    const zoomStep = this.zoomStepFromZoom(zoom);
    zoom = 1 + (zoomStep * this.zoomStepSize()); // zoom to closes zoom step.
    if (zoom < this.state.zoom) {
      zoom = this.state.zoom;
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
    this.setState(newState);
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

  scrollZoomX: function (x, before, after) {
    let lastWidth = this.standardWidth() * before;
    let newWidth = this.standardWidth() * after;
    let widthExtra = newWidth - lastWidth;
    let scrollLeft = this._lastScrollLeft;
    let zoomPointX = scrollLeft + x;
    let docPointX = zoomPointX / lastWidth;
    let offsetX = docPointX * widthExtra;

    if (this.state.zoomStyle === ZOOM_STYLE.POINT) {
      this.$scroller().scrollLeft(scrollLeft + offsetX);
    } else if (this.state.zoomStyle === ZOOM_STYLE.CENTER) {
      this.$scroller().scrollLeft(scrollLeft - ($(window).width() / 2) + x + offsetX);
    }
  },

  scrollZoomY: function (y, before, after) {
    let node = this.getDOMNode();
    let offset = $(node).offset();
    let scrollTop = $(window).scrollTop();
    let newHeight = $(node).height();
    let lastHeight = this._lastHeight;
    let heightExtra = newHeight - lastHeight;
    let zoomPointY =  Math.min(lastHeight, Math.max(0, (scrollTop + y) - offset.top));
    let docPointY = zoomPointY / lastHeight;
    let offsetY = docPointY * heightExtra;

    if (this.state.zoomStyle === ZOOM_STYLE.POINT) {
      $(window).scrollTop(scrollTop + offsetY);
    } else if (this.state.zoomStyle === ZOOM_STYLE.CENTER) {
      $(window).scrollTop(scrollTop - ($(window).height() / 4) + y + offsetY);
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
      let diffX = this._clickX - e.pageX;
      let diffY = this._clickY - e.pageY;
      this.$scroller().scrollLeft(this.$scroller().scrollLeft() + diffX);
      $(window).scrollTop($(window).scrollTop() + diffY);
      this._clickX -= diffX; // Compensate due to scrolling div.
    }
  },

  canBeGrabbed: function () {
    return !this.state.grabbing && this.width() > $(window).width();
  },

  renderPages: function () {
    return React.Children.map(this.props.children, (child) => {
      return React.addons.cloneWithProps(child, {
        width: this.width()
      });
    });
  },

  render: function () {
    const viewportWidth = $(window).width();
    const showZoomControls = !this.props.showOverlay && this.props.showArrow
      && (viewportWidth < MAX_WIDTH_SHOW_CONTROLS || isTouchDevice())

    const sectionClass = classNames({
      "grab": this.canBeGrabbed(),
      "grabbing": this.state.grabbing,
      "document-pages": this.props.ready
    });

    const scrollerStyle = {
      width: $(window).width(),
      overflowX: isTouchDevice() ? "scroll" : "hidden"
    };

    return (
      <div ref="scroller" className="scroller" style={scrollerStyle}>
        <div
          className={sectionClass}
          onMouseDown={this.handleMouseDown}
          onMouseUp={this.handleMouseUp}
          onMouseLeave={this.handleMouseLeave}
          onMouseMove={this.handleMouseMove}
          style={{width: this.width()}}
        >
          <ZoomControls
            ref="zoomControls"
            show={showZoomControls}
            zoom={this.state.zoom}
            canZoomIn={this.state.zoomStep < ZOOM_STEPS}
            canZoomOut={this.state.zoomStep > 0}
            onZoomIn={this.handleZoomIn}
            onZoomOut={this.handleZoomOut}
          />

          {/* if */ !this.props.ready &&
            <div className="col-xs-12 center">
              <div classsName="waiting4data" />
            </div>
          }
          {/* else */ this.props.ready &&
            this.renderPages()
          }
        </div>
      </div>
    );
  }
});
