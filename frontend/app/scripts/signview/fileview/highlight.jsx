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
    this.initCanvas();
    this._hl = new Highlighter({
      canvas: this._canvas,
      onNewHighlight: () => this.props.onNewHighlight(this._canvas),
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
      this.updateCanvas();
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
  initCanvas: function () {
    this._canvas = $("<canvas class='highlight-canvas'/>")[0];
    $(this._canvas).height(this.props.height);
    $(this._canvas).width(this.props.width);
    $(this._canvas).attr("height", this.props.height);
    $(this._canvas).attr("width", this.props.width);
    $(this._canvas).toggleClass("active", this.props.active);
    $(this.refs.placeForCanvas.getDOMNode()).append($(this._canvas));
  },
  updateCanvas: function () {
    var self = this;
    if (this.isMounted() && this._canvas) {
      var tmpImage = new Image();
      tmpImage.type = "image/png";
      tmpImage.onload = function () {
        setTimeout(function () {
          // In IE10 image may not be fully loaded. We need to deloay drawing it by few miliseconds
          if (self.isMounted() && self._canvas) {
            self._canvas.getContext("2d").drawImage(tmpImage, 0, 0, self.props.width, self.props.height);
          }
        }, 100);
      };
      tmpImage.src = this._canvas.toDataURL("image/png", 1);
      $(this._canvas).height(this.props.height);
      $(this._canvas).width(this.props.width);
      $(this._canvas).attr("height", this.props.height);
      $(this._canvas).attr("width", this.props.width);
      $(this._canvas).toggleClass("active", this.props.active);
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
        <span ref="placeForCanvas"/>
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
