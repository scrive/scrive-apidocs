var classNames = require("classnames");
var React = require("react");

var TooltipTop = require("../../icons/tooltip-top.svg");

import {toLessInteropLoader} from "../../common/less_utils.jsx";
import tooltipVars_ from "!@hon2a/less-vars-loader!../../../less/signview/placementtooltip.less";
const tooltipVars = toLessInteropLoader(tooltipVars_);

var PlacementTooltipView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    message: React.PropTypes.string.isRequired,
    scale: React.PropTypes.number.isRequired,
    visible: React.PropTypes.bool.isRequired
  },
  getInitialState: function () {
    return {
      left: 0,
      top: 0,
      tipLeft: 0
    };
  },
  setLayout: function (newLeft, newTop, newTipLeft) {
    this.setState({
      left: newLeft || 0,
      top: newTop || 0,
      tipLeft: newTipLeft || 0
    });
  },
  onMouseDown: function (event) {
    event.stopPropagation();
    event.preventDefault();
  },
  render: function () {
    var className = classNames("placement-tooltip", {
      visible: this.props.visible
    });

    var borderWidth = (
      tooltipVars.placementTooltipBorderWidth * this.props.scale
    );

    var style = {
      borderWidth: Math.max(1, Math.round(borderWidth)),
      left: this.state.left,
      top: this.state.top
    };

    var textStyle = {
      fontSize: tooltipVars.placementTooltipFontSize * this.props.scale,
      maxWidth: tooltipVars.placementTooltipMaxWidth * this.props.scale,
      minWidth: tooltipVars.placementTooltipMinWidth * this.props.scale,
      padding: tooltipVars.placementTooltipPadding * this.props.scale
    };

    var tipHeight = Math.ceil(
      tooltipVars.placementTooltipTopHeight * this.props.scale
    );
    var tipStyle = {
      height: tipHeight,
      left: this.state.tipLeft,
      top: -1 * tipHeight,
      width: tooltipVars.placementTooltipTopWidth * this.props.scale
    };

    return (
      <div onMouseDown={this.onMouseDown} className={className} style={style}>
        <p style={textStyle}>{this.props.message}</p>
        <TooltipTop className="top" style={tipStyle} />
      </div>
    );
  }
});

module.exports = PlacementTooltipView;
