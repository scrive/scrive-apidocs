import React from "react";
import ZoomControls from "./zoomcontrols";
import vars from "../../../less/signview/vars.less";
import isTouchDevice from "../../common/is_touch_device";

const ZOOM_STEPS = 4;
const MAX_ZOOM_PIXELS = 2000;

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
    return {zoom: 1, zoomStep: 0, grabbing: false};
  },

  componentWillMount: function () {
    $(window).on("resize", () => {
      this.maybeResetZoom();
    });
  },

  componentDidMount: function () {
    const node = this.getDOMNode();

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
  },

  componentDidUpdate: function (prevProps, prevState) {
    if (this.state.zoom !== prevState.zoom) {
      this.scrollZoomX(prevState.zoom, this.state.zoom);
      this.scrollZoomY(prevState.zoom, this.state.zoom);
      this.forceRepaint();
    }
  },

  maybeResetZoom: function () {
    if (!this.isViewportSize() && this.state.zoom !== 1) {
      this.setState({zoom: 1});
    } else if (this.state.zoom * this.standardWidth() > MAX_ZOOM_PIXELS) {
      this.setState({zoom:  MAX_ZOOM_PIXELS / this.standardWidth()});
    }
  },

  zoomStep: function () {
    const diff = MAX_ZOOM_PIXELS - this.standardWidth();
    const step = (diff / ZOOM_STEPS) / this.standardWidth();
    return parseFloat(step.toFixed(4));
  },

  // Fixes issues on iPhone with element not updating after zooming.
  forceRepaint: function () {
    let node = this.getDOMNode();
    node.style.cssText += ";-webkit-transform:rotateZ(0deg)";
    node.offsetHeight;
    node.style.cssText += ";-webkit-transform:none";
  },

  width: function () {
    return this.standardWidth() * this.state.zoom;
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

  handleZoomIn: function () {
    const nextWidth = this.standardWidth() * (this.state.zoom + this.zoomStep());
    const nextZoomStep = this.state.zoomStep + 1;
    if (nextWidth < MAX_ZOOM_PIXELS) {
      this.setState({zoom: this.state.zoom + this.zoomStep(), zoomStep: nextZoomStep});
    } else {
      this.setState({zoom: MAX_ZOOM_PIXELS / this.standardWidth(), zoomStep: nextZoomStep});
    }
  },

  handleZoomOut: function () {
    const nextZoom = this.state.zoom - this.zoomStep();
    const nextZoomStep = this.state.zoomStep - 1;
    if (nextZoom > 1) {
      this.setState({zoom: this.state.zoom - this.zoomStep(), zoomStep: nextZoomStep});
    } else {
      this.setState({zoom: 1, zoomStep: nextZoomStep});
    }
  },

  scrollZoomX: function (before, after) {
    let lastWidth = this.standardWidth() * before;
    let newWidth = this.standardWidth() * after;
    let viewportWidth = $(window).width();
    let zoomOut = before > after;

    let widthExtra = newWidth - lastWidth;
    let scrollLeft = this._lastScrollLeft;
    let zoomPointX = scrollLeft + (viewportWidth / 2);
    let docPointX = zoomPointX / lastWidth;
    let offsetX = docPointX * widthExtra;

    this.$scroller().scrollLeft(scrollLeft + offsetX);
  },

  scrollZoomY: function (before, after) {
    let node = this.getDOMNode();
    let offset = $(node).offset();
    let viewportHeight = $(window).height();
    let scrollTop = $(window).scrollTop();
    let newHeight = $(node).height();
    let lastHeight = this._lastHeight;
    let heightExtra = newHeight - lastHeight;
    let middleViewport = viewportHeight / 2;
    let zoomPointY =  Math.min(lastHeight, Math.max(0, (scrollTop + middleViewport) - offset.top));
    let docPointY = zoomPointY / lastHeight;
    let offsetY = docPointY * heightExtra;

    $(window).scrollTop(scrollTop + offsetY);
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
    const showZoomControls = !this.props.showOverlay && this.props.showArrow && this.isViewportSize();

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
