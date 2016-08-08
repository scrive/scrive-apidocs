import React from "react";
import $ from "jquery";
import "jquery.documentsize";
import BlinkMixin from "../../common/blink_mixin";
import classNames from "classnames";

const defaultErd = require("element-resize-detector")();

module.exports = React.createClass({
  displayName: "ZoomControls",

  mixins: [React.addons.PureRenderMixin, BlinkMixin],

  propTypes: {
    show: React.PropTypes.bool.isRequired,
    canZoomIn: React.PropTypes.bool.isRequired,
    canZoomOut: React.PropTypes.bool.isRequired,
    onZoomIn: React.PropTypes.func.isRequired,
    onZoomOut: React.PropTypes.func.isRequired
  },

  getInitialState: function () {
    return {};
  },

  componentDidMount: function () {
    const $el = $(this.getDOMNode());
    const $parent = $el.parent();
    const $scroller = $el.closest(".scroller");
    $(window).on("scroll", this.update);
    $(window).on("resize", this.update);
    if ($scroller.length > 0) {
      defaultErd.listenTo($scroller[0], this.update);
    }
    $scroller.on("scroll", this.update);
    this.update();
  },

  componentWillUnmount: function () {
    const $el = $(this.getDOMNode());
    const $parent = $el.parent();
    const $scroller = $el.closest(".scroller");
    $(window).off("scroll", this.update);
    $(window).off("resize", this.update);
    if ($scroller.length > 0) {
      defaultErd.removeListener($scroller[0], this.update);
    }
    $scroller.off("scroll", this.update);
  },

  update: function () {
    this.setState(this.computePosition());
  },

  height: function () {
    return 51 * 2;
  },

  computePosition: function () {
    const $parent = $(this.getDOMNode()).parent();

    const viewportWidth = $.windowWidth();
    const viewportHeight = $.windowHeight();

    const parentWidth = $parent.outerWidth();
    const parentHeight = $parent.height();
    const parentOffset = $parent.offset();
    const parentTop = parentOffset.top;
    const parentLeft = parentOffset.left;
    const parentBottom = parentTop + parentHeight;
    const parentRight = parentLeft + parentWidth;

    const scrollTop = $(window).scrollTop();
    const scrollLeft = $(window).scrollLeft();
    const scrollRight = scrollLeft + viewportWidth;

    const height = this.height()

    if (scrollTop < parentTop) {
      return {
        position: "absolute",
        right: Math.max(0, parentRight - scrollRight),
        bottom: null,
        top: 49 // half size of zoom controls minus borders and shadow.
      }
    } else if (scrollTop + height > parentBottom) {
      return {
        position: "absolute",
        right: Math.max(0, parentRight - scrollRight),
        bottom: 0,
        top: null
      }
    } else {
      return {
        position: "fixed",
        right: parentWidth < viewportWidth ? (viewportWidth - parentWidth) / 2 : 0,
        bottom: viewportHeight - height,
        top: null
      }
    }
  },

  render: function () {
    const {position, display, top, bottom, right} = this.state;
    const {show, canZoomIn, canZoomOut} = this.props;

    const divStyle = {
      position: position,
      visibility: show ? "" : "hidden",
      opacity: this.isHiddenByBlink() && "0",
      display: display,
      top: top,
      bottom: bottom,
      right: right
    };

    const divClass = classNames({
      "zoom-controls": true,
      "hidden": this.isHiddenByBlink()
    });

    return (
      <div style={divStyle} className={divClass}>
        <button
          className="zoom-in"
          // IE9 can't correctly compute backgroundPosition in getComputedStyle if it's not directly applied to the
          // element, this is important for html2canvas.
          style={{backgroundPosition: "50% 50%"}}
          onTouchStart={() => { return; }}
          onClick={(e) => {
            this.cancelBlink();
            this.props.onZoomIn(e);
          }}
          disabled={!canZoomIn}
        />
        <button
          className="zoom-out"
          style={{backgroundPosition: "50% 50%"}}
          onTouchStart={() => { return; }}
          onClick={(e) => {
            this.cancelBlink();
            this.props.onZoomOut(e)
          }}
          disabled={!canZoomOut}
        />
      </div>
    );
  }
});
