import React from "react";
import $ from "jquery";
import "jquery.documentsize";
import classNames from "classnames";
import {BrowserInfo} from "../../../js/utils/browserinfo.js";
import {Document} from "../../../js/documents";
import viewSize from "../viewsize";
import HtmlTextWithSubstitution from "../../common/htmltextwithsubstitution";
import BlinkMixin from "../../common/blink_mixin";

const defaultErd = require("element-resize-detector")();

import ZoomInIcon from "../../icons/zoom_in_icon.svg";
import ZoomOutIcon from "../../icons/zoom_out_icon.svg";
import HighlightIcon from "../../icons/highlight_icon.svg";
import ClearHighlightIcon from "../../icons/clear_highlight_icon.svg";
import DownloadDocumentButton from "./downloaddocumentbutton";

import {toLessInteropLoader} from '../../common/less_utils.jsx';
import vars_ from '!less-vars-loader!../../../less/signview/vars.less';
const vars = toLessInteropLoader(vars_);

module.exports = React.createClass({
  displayName: "SignviewControls",

  mixins: [React.addons.PureRenderMixin, BlinkMixin],

  propTypes: {
    highlightingMode: React.PropTypes.bool.isRequired,
    removingHighlightingMode: React.PropTypes.bool.isRequired,
    canZoomIn: React.PropTypes.bool.isRequired,
    canZoomOut: React.PropTypes.bool.isRequired,
    dim: React.PropTypes.bool.isRequired,
    canHighlight: React.PropTypes.bool.isRequired,
    canRemoveHighlighting: React.PropTypes.bool.isRequired,
    onZoomIn: React.PropTypes.func.isRequired,
    onZoomOut: React.PropTypes.func.isRequired,
    onStartHighlighting: React.PropTypes.func.isRequired,
    onCancelHighlighting: React.PropTypes.func.isRequired,
    onCancelRemoveHighlighting: React.PropTypes.func.isRequired,
    onStartRemovingHighlighting: React.PropTypes.func.isRequired
  },

  getInitialState: function () {
    return {className: "controls-hidden"};
  },

  contextTypes: {
    document: React.PropTypes.instanceOf(Document)
  },

  componentDidMount: function () {
    const $el = $(this.getDOMNode());
    const $parent = $el.parent();
    const $scroller = $el.closest(".scroller");
    $(window).on("scroll", this.update);
    $(window).on("resize", this.handleResize);
    if ($scroller.length > 0) {
      defaultErd.listenTo($scroller[0], this.update);
    }
    $scroller.on("scroll", this.update);
    this.update();
  },

  handleResize: function () {
    var self = this;
    clearTimeout(self.resizeTimeout);
    self.resizeTimeout = setTimeout(function () {
      // wait a bit, so window width is updated after orientation change in wkwebview
      self.update();
    }, 100);
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
    clearTimeout(self.resizeTimeout);
  },

  update: function () {
    if ($(".document-pages").size() != 0) {
      this.setState(this.position());
    } else if (this.isMounted()) {
      this.updateTimeout = setTimeout(this.update, 100);
    }
  },

  position: function () {
    const $pages = $(".document-pages");
    const $header = $(".header");
    const viewportWidth = $.windowWidth();
    const pagesWidth = $pages.outerWidth();
    const pagesOffset = $pages.offset();
    const pagesLeft = pagesOffset.left;
    const pagesRight = pagesLeft + pagesWidth;

    const scrollTop = $(window).scrollTop();
    const scrollLeft = $(window).scrollLeft();
    const scrollRight = scrollLeft + viewportWidth;
    const height = vars.signviewControlsHeight;
    const headerHeight = $header.height();
    const visiblePages = pagesRight - scrollLeft;
    const width = Math.min(viewportWidth, vars.signviewLargeView, visiblePages, pagesWidth);

    var topOffset = $pages.offset().top - height;
    var bottomOffset = $pages.offset().top + $pages.height();

    const dimClassName = {"controls-dim": this.props.dim};

    if (this.useSticky()) {
      return {
        className: classNames("controls-sticky", dimClassName),
        left: Math.max(pagesLeft, 0),
        width: width,
        top: 0,
        hasPlaceholder: false
      };
    } else if (scrollTop < topOffset) {
      return {
        className: classNames("controls-relative", dimClassName),
        left: "auto",
        width: width,
        top: 0,
        hasPlaceholder: false
      };
    } else if (scrollTop < topOffset + height) {
      return {
        className: classNames("controls-absolute", dimClassName),
        left: Math.max(pagesLeft, 0),
        width: width,
        top: (scrollTop - topOffset),
        hasPlaceholder: true
      };
    } else if (scrollTop > bottomOffset - height) {
      return {
        className: classNames("controls-absolute", dimClassName),
        left: Math.max(pagesLeft, 0),
        width: width,
        top: bottomOffset -  height - headerHeight,
        hasPlaceholder: true
      };
    } else {
      return {
        className: classNames("controls-fixed", dimClassName),
        left: Math.max(pagesLeft, 0),
        width: width,
        top: 0,
        hasPlaceholder: true
      };
    }
  },

  useSticky: function () {
    // We want to use sticky on iPads for Avis and only safari has proper support.
    return BrowserInfo.isSafari();
  },

  statusText: function () {
    const doc = this.context.document;

    if (viewSize.isSmall()) {
      if (this.props.highlightingMode || this.props.removingHighlightingMode) {
        return "<span class='put-author-name-here'/>";
      } else {
        return "<span class='put-document-title-here'/>";
      }
    } else {
      if (this.props.highlightingMode) {
        return localization.docsignview.controlsStatus.highlighting;
      } else if (this.props.removingHighlightingMode) {
        return localization.docsignview.controlsStatus.clearingHighlight;
      } else if (doc.closed()) {
        return localization.docsignview.controlsStatus.viewingClosed;
      } else if (doc.canceled()) {
        return localization.docsignview.controlsStatus.viewingCanceled;
      } else if (doc.timedout()) {
        return localization.docsignview.controlsStatus.viewingTimedout;
      } else if (doc.rejected()) {
        return localization.docsignview.controlsStatus.viewingRejected;
      } else if (doc.isSigning()) {
        return localization.docsignview.controlsStatus.viewingSigning;
      } else if (doc.isApproving()) {
        return localization.docsignview.controlsStatus.viewingApproving;
      } else if (doc.pending()) {
        return localization.docsignview.controlsStatus.viewingPending;
      } else {
        return "<span class='put-document-title-here'/>";
      }
    }
  },

  showDownloadButton: function () {
    const doc = this.context.document;
    return !this.props.highlightingMode && !this.props.removingHighlightingMode
      && (doc.showpdfdownload() || doc.closed());
  },

  renderControls: function () {
    const doc = this.context.document;
    const {top, left, width, className} = this.state;
    const {canZoomIn, canZoomOut} = this.props;
    const {highlightingMode, removingHighlightingMode, canHighlight, canRemoveHighlighting} = this.props;
    const authorNameText = doc.author().name() || localization.docsignview.controlsStatus.authornamePlaceholder;
    const divStyle = {
      top: top,
      left: left,
      width: width
    };

    var numberOfButtonsOnTheRight = 0;
    if (highlightingMode || removingHighlightingMode) {
      numberOfButtonsOnTheRight = 1;
    } else {
      if (canRemoveHighlighting) {
        numberOfButtonsOnTheRight++;
      }
      if (canHighlight) {
        numberOfButtonsOnTheRight++;
      }
      if (canZoomIn || canZoomOut) {
        numberOfButtonsOnTheRight += 2;
      }
    }
    const usedWidth = numberOfButtonsOnTheRight * vars.signviewControlsWidth + (this.showDownloadButton() ? 84 : 0);
    const statusStyle = {width: "calc(100% - " + usedWidth + "px)"};

    const buttonsClass = classNames("buttons right", {
      "hidden": this.isHiddenByBlink() || doc.isUnavailableForSignOrApprove()
    });

    return (
      <div style={divStyle} className={classNames("controls", className)}>
        {/* if */ this.showDownloadButton() &&
          <div className="buttons left">
            <DownloadDocumentButton document={this.context.document} />
          </div>
        }
        <div className="status" style={statusStyle}>
            <HtmlTextWithSubstitution
              secureText={this.statusText()}
              subs={{
                ".put-document-title-here": doc.title(),
                ".put-author-name-here": authorNameText
              }}
            />
        </div>
        <div className={buttonsClass}>
          {/* if */ !highlightingMode && !removingHighlightingMode &&
            <span>
              {/* if */ canRemoveHighlighting &&
                <button
                  onTouchStart={() => { return; }}
                  onClick={(e) => {
                    this.props.onStartRemovingHighlighting(e);
                  }}
                >
                  <ClearHighlightIcon/>
                </button>
              }
              {/* if */ canHighlight &&
                <button
                  onTouchStart={() => { return; }}
                  onClick={(e) => {
                    this.props.onStartHighlighting(e);
                  }}
                >
                  <HighlightIcon/>
                </button>
              }
              {/* if */ (canZoomIn || canZoomOut) &&
                <button
                  onTouchStart={() => { return; }}
                  onClick={(e) => {
                    this.props.onZoomIn(e);
                  }}
                  disabled={!canZoomIn}
                >
                  <ZoomInIcon/>
                </button>
              }
              {/* if */ (canZoomIn || canZoomOut)  &&
                <button
                  onTouchStart={() => { return; }}
                  onClick={(e) => {
                    this.props.onZoomOut(e);
                  }}
                  disabled={!canZoomOut}
                >
                  <ZoomOutIcon/>
                </button>
              }

            </span>
          }
          {/* else */ (highlightingMode || removingHighlightingMode) &&
            <button
              className="cancel"
              onTouchStart={() => { return; }}
              onClick={(e) => {
                if (highlightingMode) {
                  this.props.onCancelHighlighting(e);
                }

                if (removingHighlightingMode) {
                  this.props.onCancelRemoveHighlighting(e);
                }
              }}
            >
              {localization.docsignview.doneHighlighting}
            </button>
          }
        </div>
        <div className="clearfix" />
      </div>
    );
  },
  render: function () {
    if (this.state.hasPlaceholder) {
      return (
        <div>
          <div className="controls-placeholder"/>
          {this.renderControls()}
        </div>
      );
    } else {
      return this.renderControls();
    }
  }
});
