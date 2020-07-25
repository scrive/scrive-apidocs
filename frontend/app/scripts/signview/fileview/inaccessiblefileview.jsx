import Backbone from "backbone";
import React from "react";
import $ from "jquery";
import classNames from "classnames";
import {Document} from "../../../js/documents.js";
import HtmlTextWithSubstitution from "../../common/htmltextwithsubstitution";
import Instructions from "../instructionsview/instructions";

import {toLessInteropLoader} from '../../common/less_utils.jsx';
import vars_ from '!less-vars-loader!../../../less/signview/vars.less';
const vars = toLessInteropLoader(vars_);

module.exports = React.createClass({
  displayName: "InaccessibleFileView",

  contextTypes: {
    document: React.PropTypes.instanceOf(Document)
  },

  mixins: [React.addons.PureRenderMixin],

  componentWillMount: function () {
    $(window).on("resize", this.handleResize);
  },

  componentWillUnmount: function () {
    $(window).off("resize", this.handleResize);
    clearTimeout(this.resizeTimeout);
  },

  handleResize: function () {
    const self = this;
    clearTimeout(self.resizeTimeout);
    self.resizeTimeout = setTimeout(function () {
      // wait a bit, so window width is updated after orientation change in wkwebview
      self.forceUpdate();
    }, 100);
  },

  statusText: function () {
    const doc = this.context.document;

    if (doc.canceled()) {
      return localization.docsignview.controlsStatus.viewingCanceled;
    } else if (doc.timedout()) {
      return localization.docsignview.controlsStatus.viewingTimedout;
    } else if (doc.rejected()) {
      return localization.docsignview.controlsStatus.viewingRejected;
    } else if (doc.isSigning()) {
      return localization.docsignview.controlsStatus.viewingSigning;
    } else {
      return "<span class='put-document-title-here'/>";
    }
  },

  render: function () {
    const doc = this.context.document;

    const authorNameText = doc.author().name() || localization.docsignview.controlsStatus.authornamePlaceholder;
    const width = ($.windowWidth() < vars.signviewLargeView) ? $.windowWidth() : vars.signviewLargeView;

    return (
      <div>
        <div style={{width: width}} className="controls controls-relative">
          <div className="status">
            <HtmlTextWithSubstitution
              secureText={this.statusText()}
              subs={{
                ".put-document-title-here": doc.title(),
                ".put-author-name-here": authorNameText
              }}
            />
          </div>
        </div>
        <div className="document-pages" style={{width: width}}>
          <Instructions model={doc} />
        </div>
      </div>
    );
  }
});
