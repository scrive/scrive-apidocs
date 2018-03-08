var ClassNames = require("classnames");
var React = require("react");

var FieldPlacementGlobal = require("../../js/fieldplacementglobal.js").FieldPlacementGlobal;

module.exports = React.createClass({
  displayName: "Radiobutton",
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    pageWidth: React.PropTypes.number.isRequired,
    wrel: React.PropTypes.number.isRequired,
    active: React.PropTypes.bool.isRequired,
    selected: React.PropTypes.bool.isRequired,
    inline: React.PropTypes.bool
  },
  isSize: function (size) {
    return Math.abs(this.props.wrel - size) < 0.001;
  },
  isSmall: function () {
    return !this.isMedium() && !this.isLarge();
  },
  isMedium: function () {
    return this.isSize(FieldPlacementGlobal.mediumRadiobuttonRatio);
  },
  isLarge: function () {
    return this.isSize(FieldPlacementGlobal.largeRadiobuttonRatio);
  },
  render: function () {
    var inactive = !this.props.active;
    var wrapperClassName = ClassNames("radiobutton-wrapper", {
      small: this.isSmall(),
      medium: this.isMedium(),
      large: this.isLarge(),
      inactive: inactive,
      selected: this.props.selected,
      inline: this.props.inline
    });

    var buttonSize = Math.round(
      FieldPlacementGlobal.smallRadiobuttonRatio * this.props.pageWidth
    );

    if (this.isMedium()) {
      buttonSize = Math.round(
        FieldPlacementGlobal.mediumRadiobuttonRatio * this.props.pageWidth
      );
    } else if (this.isLarge()) {
      buttonSize = Math.round(
        FieldPlacementGlobal.largeRadiobuttonRatio * this.props.pageWidth
      );
    }

    var spriteWidth = buttonSize;
    var spriteHeight = buttonSize * 4;

    var spriteX = 0;
    var spriteY = 0;

    if (this.props.selected) {
      spriteY = -1 * buttonSize;
    }

    if (inactive) {
      if (!this.props.selected) {
        spriteY = -2 * buttonSize;
      } else {
        spriteY = -3 * buttonSize;
      }
    }

    var wrapperStyle = {
      width: buttonSize,
      height: buttonSize
    };

    var radiobuttonStyle = {
      backgroundSize: spriteWidth + "px" + " " + spriteHeight + "px",
      backgroundPosition: spriteX + "px" + " " + spriteY + "px",
      height: buttonSize,
      left: 0,
      top: 0,
      width: buttonSize
    };

    return (
      <div className={wrapperClassName} style={wrapperStyle}>
        <div className="radiobutton" style={radiobuttonStyle}></div>
      </div>
    );
  }
});
