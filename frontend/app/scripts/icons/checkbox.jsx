var ClassNames = require("classnames");
var React = require("react");

var FieldPlacementGlobal = require("../../js/fieldplacementglobal.js").FieldPlacementGlobal;

module.exports = React.createClass({
  displayName: "Checkbox",
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    pageWidth: React.PropTypes.number.isRequired,
    wrel: React.PropTypes.number.isRequired,
    active: React.PropTypes.bool.isRequired,
    checked: React.PropTypes.bool.isRequired
  },
  isSmall: function () {
    return !this.isMedium() && !this.isLarge();
  },
  isMedium: function () {
    return Math.abs(this.props.wrel - FieldPlacementGlobal.mediumCheckboxRatio) < 0.001;
  },
  isLarge: function () {
    return Math.abs(this.props.wrel - FieldPlacementGlobal.largeCheckboxRatio) < 0.001;
  },
  render: function () {
    var size = Math.round(this.props.wrel * this.props.pageWidth);

    var inactive = !this.props.active;
    var wrapperClassName = ClassNames("checkbox-wrapper", {
      small: this.isSmall(),
      medium: this.isMedium(),
      large: this.isLarge(),
      inactive: inactive,
      checked: this.props.checked
    });

    var spriteWidth = size;
    var spriteHeight = size * 4;

    var spriteX = 0;
    var spriteY = 0;

    if (this.props.checked) {
      spriteY = -1 * size;
    }

    if (inactive) {
      if (!this.props.checked) {
        spriteY = -2 * size;
      } else {
        spriteY = -3 * size;
      }
    }

    var wrapperStyle = {
      width: size,
      height: size
    };

    var checkboxStyle = {
      backgroundSize: spriteWidth + "px" + " " + spriteHeight + "px",
      backgroundPosition: spriteX + "px" + " " + spriteY + "px",
      height: wrapperStyle.height,
      width: wrapperStyle.width
    };

    return (
      <div className={wrapperClassName} style={wrapperStyle}>
        <div style={checkboxStyle}></div>
      </div>
    );
  }
});
