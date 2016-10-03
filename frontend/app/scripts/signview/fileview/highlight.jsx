import React from "react";
import classNames from "classnames";
import Button from "../../common/button";
import HtmlTextWithSubstitution from "../../common/htmltextwithsubstitution";
import Highlighter from "./highlighter";

import ClearHighlightNowIcon from "../../icons/clear_highlight_now_icon.svg";

module.exports = React.createClass({
  _hl: undefined,

  displayName: "Highlight",

  propTypes: {
    active: React.PropTypes.bool.isRequired,
    removingHighlightingMode: React.PropTypes.bool.isRequired,
    onNewHighlight: React.PropTypes.func.isRequired,
    baseImageURL: React.PropTypes.string,
    onRemoveHighlighting: React.PropTypes.func.isRequired,
    width: React.PropTypes.number.isRequired,
    height: React.PropTypes.number.isRequired
  },
  componentDidMount: function () {
    this._hl = new Highlighter({
      canvas: this.refs.canvas.getDOMNode(),
      onNewHighlight: () => this.props.onNewHighlight(this.refs.canvas.getDOMNode()),
      baseImageURL: this.props.baseImageURL
    });

    if (this.props.active) {
      this._hl.activate();
    }
    this._hl.bindStopDrawingToGlobalHandlers();
  },
  componentWillUnmount: function () {
    this._hl.unbindStopDrawingFromGlobalHandlers();
  },
  componentDidUpdate: function (prevProps) {
    if (prevProps.width !== this.props.width || prevProps.height !== this.props.height) {
      this._hl.restore();
    }

    if (this.props.active) {
      this._hl.activate();
    } else {
      this._hl.deactivate();
    }

    if (prevProps.baseImageURL != this.props.baseImageURL) {
      if (this.props.baseImageURL) {
        this._hl.setBaseImageURL(this.props.baseImageURL);
      } else {
        this._hl.clear();
      }
    }
  },

  handleRemoveHighlighting: function () {
    this.props.onRemoveHighlighting();
  },

  render: function () {

    var clearHighlightStyle = {
      display: this.props.removingHighlightingMode ? "block" : "none",
      width: this.props.width + "px",
      height: this.props.height + "px"
    };

    return (
      <span>
        <canvas
          ref="canvas"
          className={classNames("highlight-canvas", {"active": this.props.active})}
          width={this.props.width}
          height={this.props.height}
        />
        <div className="highlight-clear" style={clearHighlightStyle}>
          <div onClick={this.handleRemoveHighlighting} className="highlight-clear-button">
            <div className="clear-icon">
              <ClearHighlightNowIcon />
            </div>
            <div className='text'>
              <HtmlTextWithSubstitution
                secureText={localization.docsignview.clearHighlightingHere}
              />
            </div>
          </div>
        </div>
      </span>
    );
  }
});
