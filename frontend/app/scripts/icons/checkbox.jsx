var React = require("react");

var CheckboxLargeActiveCheckedIcon = require("./checkboxes/checkbox_large_active_checked.svg");
var CheckboxLargeCheckedIcon = require("./checkboxes/checkbox_large_checked.svg");
var CheckboxLargeNotCheckedIcon = require("./checkboxes/checkbox_large_not_checked.svg");
var CheckboxMediumActiveCheckedIcon = require("./checkboxes/checkbox_medium_active_checked.svg");
var CheckboxMediumCheckedIcon = require("./checkboxes/checkbox_medium_checked.svg");
var CheckboxMediumNotCheckedIcon = require("./checkboxes/checkbox_medium_not_checked.svg");
var CheckboxSmallActiveCheckedIcon = require("./checkboxes/checkbox_small_active_checked.svg");
var CheckboxSmallCheckedIcon = require("./checkboxes/checkbox_small_checked.svg");
var CheckboxSmallNotCheckedIcon = require("./checkboxes/checkbox_small_not_checked.svg");
var FieldPlacementGlobal = require("../../js/fieldplacementglobal.js").FieldPlacementGlobal;

module.exports =  React.createClass({
  propTypes: {
    pageWidth: React.PropTypes.number.isRequired,
    wrel: React.PropTypes.number.isRequired,
    active: React.PropTypes.bool.isRequired,
    checked: React.PropTypes.bool.isRequired
  },

  isSmall: function() {
    return !this.isMedium() && !this.isLarge();
  },

  isMedium: function() {
    return Math.abs(this.props.wrel - FieldPlacementGlobal.mediumCheckboxRatio) < 0.001;
  },

  isLarge: function() {
    return Math.abs(this.props.wrel - FieldPlacementGlobal.largeCheckboxRatio) < 0.001;
  },

  render: function () {
    var size = Math.round(this.props.wrel * this.props.pageWidth);
    var style = { width : size, height: size}

    if (this.isLarge()) {
      if (this.props.checked && this.props.active) {
        return (<CheckboxLargeActiveCheckedIcon className="svg-checked-active-icon" style={style} />);
      } else if (this.props.checked && !this.props.active) {
        return (<CheckboxLargeCheckedIcon className="svg-checked-icon" style={style} />);
      } else {
        return (<CheckboxLargeNotCheckedIcon className="svg-not-checked-icon" style={style} />);
      }
    }
    else if (this.isMedium()) {
      if (this.props.checked && this.props.active) {
        return (<CheckboxMediumActiveCheckedIcon className="svg-checked-active-icon" style={style} />);
      } else if (this.props.checked && !this.props.active) {
        return (<CheckboxMediumCheckedIcon className="svg-checked-icon" style={style} />);
      } else {
        return (<CheckboxMediumNotCheckedIcon className="svg-not-checked-icon" style={style} />);
      }
    }
    else if (this.isSmall()) {
      if (this.props.checked && this.props.active) {
        return (<CheckboxSmallActiveCheckedIcon className="svg-checked-active-icon" style={style} />);
      } else if (this.props.checked && !this.props.active) {
        return (<CheckboxSmallCheckedIcon className="svg-checked-icon" style style={style} />);
      } else {
        return (<CheckboxSmallNotCheckedIcon className="svg-not-checked-icon" style={style} />);
      }
    }
  }
});
