import React from "react";
import _ from "underscore";
import $ from "jquery";
import classNames from "classnames";
import isTouchDevice from "../../common/is_touch_device";
import {AuthorAttachment} from "../../../js/authorattachment";
import AuthorAttachmentFilePage from "./authorattachmentfilepage";

import vars_ from '!less-vars-loader!../../../less/signview/vars.less';
import {toLessInteropLoader} from '../../common/less_utils.jsx';
const vars = toLessInteropLoader(vars_);

module.exports = React.createClass({

displayName: "AuthorAttachmentFileViewer",

  propTypes: {
    attachment: React.PropTypes.instanceOf(AuthorAttachment).isRequired,
    ready: React.PropTypes.bool.isRequired
  },

  mixins: [React.addons.PureRenderMixin],

  getInitialState: function () {
    return {
      grabbing: false
    };
  },

  width: function () {
    if (this.isViewportSize()) {
      return $.windowWidth();
    } else {
      return vars.signviewLargeView;
    }
  },

  isViewportSize: function () {
    return $.windowWidth() < vars.signviewLargeView;
  },

  componentWillMount: function () {
    $(window).on("resize", this.handleResize);
  },

  componentWillUnmount: function () {
    $(window).off("resize", this.handleResize);
  },

  handleResize: function () {
    this.forceUpdate();
  },

  canBeGrabbed: function () {
    return !this.state.grabbing;
  },

  handleMouseDown: function (e) {
    if (this.canBeGrabbed()) {
      this._clickY = e.pageY;
      this.setState({grabbing: true});
    }
  },

  handleMouseUp: function () {
    this.setState({grabbing: false});
  },

  handleMouseLeave: function () {
    this.setState({grabbing: false});
  },

  handleMouseMove: function (e) {
    if (this.state.grabbing) {
      var diffY = this._clickY - e.pageY;
      $(window).scrollTop($(window).scrollTop() + diffY);
    }
  },

  render: function () {
    const attachment = this.props.attachment;
    const width = this.width();

    const sectionClass = classNames("document-pages", {
      "grab": this.canBeGrabbed(),
      "grabbing": this.state.grabbing
    });

    const scrollerStyle = {
      width: $.windowWidth(),
      overflowX: isTouchDevice() ? "scroll" : "hidden"
    };

    return (
      <div>
        <div ref="scroller" className="scroller" style={scrollerStyle}>
          <div
            className={sectionClass}
            onMouseDown={this.handleMouseDown}
            onMouseUp={this.handleMouseUp}
            onMouseLeave={this.handleMouseLeave}
            onMouseMove={this.handleMouseMove}
            style={{width: width}}
          >
            <div ref="pages">
              {/* if */ this.props.ready &&
                <div>
                  {_.map(_.range(attachment.pages()), function (p) {
                  return (
                    <AuthorAttachmentFilePage
                      key={p + 1}
                      number={p + 1}
                      imageSrc={attachment.pageUrl(p + 1)}
                      width={width}
                    />
                  );
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
