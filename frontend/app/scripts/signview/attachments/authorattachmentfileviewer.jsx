import React from "react";
import _ from "underscore";
import $ from "jquery";
import {AuthorAttachment} from "../../../js/authorattachment";
import AuthorAttachmentFilePage from "./authorattachmentfilepage";

import vars from "../../../less/signview/vars.less";

module.exports = React.createClass({

displayName: "AuthorAttachmentFileViewer",

  propTypes: {
    attachment: React.PropTypes.instanceOf(AuthorAttachment).isRequired,
    ready: React.PropTypes.bool.isRequired
  },

  mixins: [React.addons.PureRenderMixin],

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

  render: function () {
    const attachment = this.props.attachment;
    const width = this.width();

    return (
      <div>
        <div className={"document-pages"} style={{width: width}}>
          {/* if */ this.props.ready &&
            <div ref="pages">
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
    );
  }
});
