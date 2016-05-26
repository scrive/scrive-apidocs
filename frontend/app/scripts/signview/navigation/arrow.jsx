import $ from "jquery";
import React from "react";
import Task from "./task";
import isElementInViewport from "../../common/iselementinviewport";
import scrollToElement from "../../common/scrolltoelement";
import BlinkMixin from "../../common/blink_mixin";
import classNames from "classnames";

import arrowVars from "../../../less/signview/arrows.less";

const defaultErd = require("element-resize-detector")();

const ARROW = {NONE: -1, UP: 0, DOWN: 1, LEFT: 2, RIGHT: 3};

function transformWithPrefixes(style, value) {
  style.transform = value;
  style.msTransform = value;
  style.MozTransform = value;
  style.WebkitTransform = value;
}

function scrollerFromTask(task) {
  return $(task.el()).closest(".scroller");
}

module.exports = React.createClass({
  displayName: "Arrow",

  mixins: [React.addons.PureRenderMixin, BlinkMixin],

  propTypes: {
    task: React.PropTypes.instanceOf(Task).isRequired,
    show: React.PropTypes.bool.isRequired
  },

  componentWillMount: function () {
    this.update();
  },

  componentDidMount: function () {
    const $signview = $(".signview");
    $(window).on("scroll", this.handleScroll);
    $(window).on("resize", this.update);
    if ($signview.length > 0) {
      defaultErd.listenTo($signview[0], this.update);
    }
    this.listenToTask(this.props.task);
  },

  componentWillUnmount: function () {
    const $signview = $(".signview");
    $(window).off("scroll", this.handleScroll);
    $(window).off("resize", this.update);
    if ($signview.length > 0) {
      defaultErd.removeListener($signview[0], this.update);
    }
    this.stopListenToTask(this.props.task);
  },

  componentWillUpdate: function (nextProps) {
    if (this.props.task !== nextProps.task) {
      this.stopListenToTask(this.props.task);
      this.listenToTask(nextProps.task);
    }
  },

  listenToTask: function (task) {
    defaultErd.listenTo(task.el()[0], this.update);

    if (task.isFieldTask()) {
      scrollerFromTask(task).on("scroll", this.handleScroll);
    }
  },

  stopListenToTask: function (task) {
    defaultErd.removeListener(task.el()[0], this.update);

    if (task.isFieldTask()) {
      scrollerFromTask(task).off("scroll", this.handleScroll);
    }
  },

  componentDidUpdate: function (prevProps, prevState) {
    if (this.props.task !== prevProps.task) {
      this.update();
      this.blink();
    }

    if (this.state.type !== prevState.type) {
      this.blink();
    }
  },

  update: function () {
    let newState = this.sizeAndPositionOfElement();
    newState.type = this.computeArrowType();
    newState.angle = this.computeAngle();
    newState.scale = this.computeScale();
    this.setState(newState);
  },

  scroll: function () {
    scrollToElement(this.props.task.el(), () => {
      this.props.task.onActivate();
    });
  },

  goto: function () {
    if (this.state.type === ARROW.UP || this.state.type === ARROW.DOWN) {
      this.scroll();
    } else {
      this.blink();
    }
  },

  computeScale: function () {
    const BASE_FILEVIEW_WIDTH = 1040;
    const $pages = scrollerFromTask(this.props.task).find(".document-pages");

    if ($pages) {
      return $pages.width() / BASE_FILEVIEW_WIDTH;
    } else {
      return 0;
    }
  },

  computeAngle: function () {
    const $el = this.props.task.el();
    const offset = $el.offset();
    const size = arrowVars.scrollArrowSize;

    const scrollMiddle = $(window).scrollLeft() + ($(window).width() / 2);
    const scrollTop = $(window).scrollTop();
    const scrollBottom = scrollTop + $(window).height();
    const type = scrollTop < offset.top ? ARROW.DOWN : ARROW.UP;

    const x1 = scrollMiddle;
    const y1 = type == ARROW.DOWN ? scrollBottom - size : scrollTop + size;

    const x2 = offset.left + ($el.width() / 2);
    const y2 = offset.top + ($el.height() / 2);

    const angle = Math.atan2(y2 - y1, x2 - x1);

    return angle + (Math.PI / 2);
  },

  sizeAndPositionOfElement: function () {
    const $el = this.props.task.el();
    const width = $el.outerWidth();
    const height = $el.outerHeight();
    const offset = $el.offset();

    return {
      width: width,
      height: height,
      top: offset.top,
      bottom: offset.top + height,
      left: offset.left,
      right: offset.left + width
    };
  },

  computeArrowDirection: function () {
    const task = this.props.task;
    const top = task.el().offset().top;
    const scrollTop = $(window).scrollTop();

    if (scrollTop < top) {
      return ARROW.DOWN;
    } else {
      return ARROW.UP;
    }
  },

  computerArrowSide: function () {
    const task = this.props.task;
    const margin = 10;
    const base = task.tipSide() !== "right" ? ARROW.LEFT : ARROW.RIGHT;
    const viewportWidth = $(window).width();
    const viewportHeight = $(window).height();
    const elOffset = this.sizeAndPositionOfElement();
    const elMiddle = elOffset.top + (elOffset.height / 2);
    const scrollTop = $(window).scrollTop();
    const scrollBottom = scrollTop + viewportHeight;
    const scrollLeft = $(window).scrollLeft();
    const scrollRight = scrollLeft + viewportWidth;
    const leftVisible = scrollLeft < elOffset.left && elOffset.left - margin > 0;
    const rightVisible = elOffset.right < scrollRight && elOffset.right + margin < viewportWidth;
    const middleVisible =  elMiddle < scrollBottom && elMiddle > scrollTop;

    if ((rightVisible || leftVisible) && middleVisible) {
      if (base === ARROW.RIGHT) {
        return rightVisible ? base : ARROW.LEFT;
      } else {
        return leftVisible ? base : ARROW.RIGHT;
      }
    } else {
      return this.computeArrowDirection();
    }
  },

  computeArrowType: function () {
    const task = this.props.task;

    if (isElementInViewport.part(task.el())) {
      if (task.isOverlayTask()) {
        return ARROW.NONE;
      } else {
        return this.computerArrowSide();
      }
    } else {
      return this.computeArrowDirection();
    }
  },

  handleScroll: function () {
    this.update();
    this.props.task.onScrollWhenActive();
  },

  handleClick: function () {
    const {type} = this.state;

    if (type === ARROW.LEFT || type === ARROW.RIGHT) {
      this.props.task.onArrowClick();
    } else {
      this.scroll();
    }
  },

  render: function () {
    const {task, show} = this.props;
    const {type, left, right, top, height, angle, scale} = this.state;

    const arrowClass = classNames({
      "hidden": this.isHiddenByBlink(),
      "scroll-arrow": type === ARROW.UP || type === ARROW.DOWN,
      "action-arrow": type === ARROW.LEFT || type === ARROW.RIGHT,
      "up": type === ARROW.UP,
      "down": type === ARROW.DOWN,
      "left": type === ARROW.LEFT,
      "right": type === ARROW.RIGHT
    });

    let arrowStyle = {};
    if (type === ARROW.NONE) {
      arrowStyle = {display: "none"};
    } else if (type === ARROW.LEFT) {
      arrowStyle = {left: left + "px", top: top + (height / 2)};
    } else if (type === ARROW.RIGHT) {
      arrowStyle = {
        left: right + "px",
        top: top + (height / 2),
        display: right > window.innerWidth ? "none" : ""
      };
    } else if ((type === ARROW.UP || ARROW.DOWN) && task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "rotate(" + angle + "rad)");
    } else if (type === ARROW.DOWN && !task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "rotate(" + Math.PI + "rad)");
    }

    if ((type === ARROW.RIGHT || type === ARROW.LEFT) && task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "scale(" + Math.max(1, scale) + ")");
    }

    if (type === ARROW.RIGHT && task.isFieldTask()) {
      arrowStyle.marginLeft = Math.min(1, scale) * arrowVars.actionArrowRightMargin;
    }

    if (type === ARROW.LEFT && task.isFieldTask()) {
      arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowLeftMargin;
    }

    return (
      <div style={{display: show ? "block" : "none"}} className="arrows" onClick={this.handleClick}>
        <div className={arrowClass} style={arrowStyle} />
      </div>
    );
  }
});
