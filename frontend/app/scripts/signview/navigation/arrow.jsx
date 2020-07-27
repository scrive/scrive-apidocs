import $ from "jquery";
import React from "react";
import Task from "./task";
import isElementInViewport from "../../common/iselementinviewport";
import scrollToElement from "../../common/scrolltoelement";
import classNames from "classnames";
import _ from "underscore";

import ArrowDown from "../../icons/arrow-down.svg";
import ArrowRight from "../../icons/arrow-right.svg";
import BlinkGroupModel from "./blinkgroupmodel";

import {toLessInteropLoader} from '../../common/less_utils.jsx';
import arrowVars_ from '!less-vars-loader!../../../less/signview/arrows.less';
const arrowVars = toLessInteropLoader(arrowVars_);

const defaultErd = require("element-resize-detector")({"strategy": "scroll"});

import Transporter from "../../common/transporter";

const ARROW = {NONE: -1, UP: 0, DOWN: 1, LEFT: 2, RIGHT: 3};

const BLINK_GROUPS = [];

function transformWithPrefixes (style, value) {
  const oldValue = style.transform;
  let newValue = value;

  if (oldValue) {
    newValue += " " + oldValue;
  }

  style.transform = newValue;
  style.msTransform = newValue;
  style.MozTransform = newValue;
  style.WebkitTransform = newValue;
}

function scrollerFromTask (task) {
  return $(task.el()).closest(".scroller");
}

module.exports = React.createClass({
  _mountNode: null,

  displayName: "Arrow",

  mixins: [React.addons.PureRenderMixin],

  propTypes: {
    task: React.PropTypes.instanceOf(Task).isRequired,
    show: React.PropTypes.bool.isRequired,
    first: React.PropTypes.bool
  },

  getInitialState: function () {
    return {
      isHiddenByBlink: false
    };
  },

  componentWillMount: function () {
    this.registerBlinkGroup();
    this.computeMountNode(this.props.task);
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
    this.deregisterBlinkGroup();
  },

  componentWillUpdate: function (nextProps) {
    if (this.props.task !== nextProps.task) {
      this.deregisterBlinkGroup();
      this.computeMountNode(nextProps.task);
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
      this.registerBlinkGroup();
      this.update();
      this.blink();
    }

    if (this.state.type !== prevState.type) {
      this.blink();
    }
  },

  update: function () {
    const task = this.props.task;
    let newState = this.sizeAndPositionOfElement(task.isFieldTask());
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

  computeMountNode: function (task) {
    let $node = $("#default-place-for-arrows");

    if (task.isFieldTask()) {
      $node = scrollerFromTask(task).find(".place-for-arrows");
    }

    this._mountNode = $node.first();
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

    return angle + ((3 * Math.PI) / 2);
  },

  sizeAndPositionOfElement: function (relative) {
    const task = this.props.task;
    const $el = task.el();
    const width = $el.outerWidth();
    const height = $el.outerHeight();
    let offset = $el.offset();

    if (relative && $(this._mountNode).offset() != undefined) {
      offset.top = offset.top - $(this._mountNode).offset().top;
      offset.left = offset.left - $(this._mountNode).offset().left;

      if (this.props.task.field().isRadioGroup()) {
        offset.left = offset.left + this.props.task.margin();
      }
    }

    if (task.isConsentQuestion()) {
      offset.left = offset.left + task.margin();
    }

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

  computeArrowSide: function () {
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
      } else if (task.isConsentQuestion()) {
        return ARROW.LEFT;
      } else {
        return this.computeArrowSide();
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

  blinkGroup: function () {
    return _.first(_.filter(BLINK_GROUPS, (item) => {
      return item.get("groupId") == this.blinkGroupId();
    }));
  },

  registerBlinkGroup: function () {
    let blinkGroup = this.blinkGroup();
    if (!blinkGroup) {
      blinkGroup = new BlinkGroupModel({groupId: this.blinkGroupId()});
      BLINK_GROUPS.push(blinkGroup);
    }

    blinkGroup.on("change:counter", this.onBlinkGroupCounterChange);
  },

  deregisterBlinkGroup: function () {
    let blinkGroup = this.blinkGroup();
    if (blinkGroup) {
      blinkGroup.on("change:counter", this.onBlinkGroupCounterChange);
    }
  },

  shouldBlink: function () {
    const type = this.state.type;
    const first = this.props.first;
    let result = true;

    if (!_.isUndefined(first) && (type === ARROW.DOWN || type === ARROW.UP)) {
      result = first;
    }

    return result;
  },

  blinkGroupId: function () {
    let task = this.props.task;
    return (task.field() || task.consentQuestion() || task).cid;
  },

  onBlinkGroupCounterChange: function () {
    var blinkGroup = this.blinkGroup();

    if (blinkGroup) {
      this.setState({
        isHiddenByBlink: (
          this.shouldBlink() && blinkGroup &&
          blinkGroup.get("counter") % 2 !== 0
        )
      });
    }
  },

  blink: function () {
    let blinkGroup = this.blinkGroup();
    if (blinkGroup) {
      blinkGroup.blink();
    }
  },

  cancelBlink: function () {
    let blinkGroup = this.blinkGroup();
    if (blinkGroup) {
      blinkGroup.cancelBlink();
    }
  },

  render: function () {
    const task = this.props.task;
    let show = this.props.show;
    const {type, left, right, top, height, angle, scale} = this.state;

    const arrowClass = classNames({
      "hidden": this.state.isHiddenByBlink,
      "scroll-arrow": type === ARROW.UP || type === ARROW.DOWN,
      "action-arrow": type === ARROW.LEFT || type === ARROW.RIGHT,
      "up": type === ARROW.UP,
      "down": type === ARROW.DOWN,
      "left": type === ARROW.LEFT,
      "right": type === ARROW.RIGHT
    });

    var arrowStyle = {};
    if (type === ARROW.NONE) {
      arrowStyle = {display: "none"};
    } else if (type === ARROW.LEFT) {
      arrowStyle = {left: left + "px", top: top + (height / 2)};
    } else if (type === ARROW.RIGHT) {
      arrowStyle = {
        left: right + "px",
        top: top + (height / 2)
      };
    } else if ((type === ARROW.UP || ARROW.DOWN) && task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "rotate(" + angle + "rad)");
    } else if (type === ARROW.UP && !task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "rotate(" + Math.PI + "rad)");
    }

    if ((type === ARROW.RIGHT || type === ARROW.LEFT) && task.isFieldTask()) {
      transformWithPrefixes(arrowStyle, "scale(" + Math.max(1, scale) + ")");
    }

    if (type === ARROW.RIGHT && task.isFieldTask()) {
      arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowRightMargin;
      if (task.field().isCheckbox() && task.field().placements()[0].wrel() < 0.02) {
        arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowSmallRightMargin;
      }
      if (task.field().isRadioGroup()) {
        arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowSmallRightMargin;
      }
    }

    if (type === ARROW.LEFT && task.isFieldTask()) {
      arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowLeftMargin;
      if (task.field().isCheckbox() && task.field().placements()[0].wrel() < 0.02) {
        arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowSmallRightMargin;
      }
      if (task.field().isRadioGroup()) {
        arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowSmallRightMargin;
      }
    }

    if (type == ARROW.LEFT && task.isConsentQuestion()
        && this.sizeAndPositionOfElement().left > 10) {
      transformWithPrefixes(arrowStyle, "scale(0.6)");
    }

    if (type === ARROW.LEFT) {
      transformWithPrefixes(arrowStyle, "rotate(180deg)");
    }

    if ((type === ARROW.LEFT || type === ARROW.RIGHT) && task.isRequiredAuthorAttachmentTask()) {
      arrowStyle.marginLeft = Math.max(1, scale) * arrowVars.actionArrowSmallRightMargin;
    }

    let first = (_.isUndefined(this.props.first) ? true : this.props.first);
    if ((type === ARROW.DOWN || type === ARROW.UP) && !first) {
      show = false;
    }

    return (
      <Transporter node={this._mountNode}>
        <div style={{display: show ? "block" : "none"}} className="arrows" onClick={this.handleClick}>
          {(type === ARROW.DOWN || type === ARROW.UP) &&
            <ArrowDown className={arrowClass} style={arrowStyle} />
          }
          {!(type === ARROW.DOWN || type === ARROW.UP) &&
            <ArrowRight className={arrowClass} style={arrowStyle} />
          }
        </div>
      </Transporter>
    );
  }
});
