var React = require("react");
var classNames = require("classnames");

var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
    pageWidth: React.PropTypes.number.isRequired,
    pageHeight: React.PropTypes.number.isRequired
  },
  fieldStyle: function () {
    var pageWidth = this.props.pageWidth;
    var pageHeight = this.props.pageHeight;

    return {
      height: this.props.model.hrel() * pageHeight,
      left: Math.round(this.props.model.xrel() * pageWidth) - FieldPlacementGlobal.placementBorder,
      top: Math.round(this.props.model.yrel() * pageHeight) - FieldPlacementGlobal.placementBorder,
      width: this.props.model.wrel() * pageWidth
    };
  },
  render: function () {
    var checkboxClassName = classNames("placedcheckbox-noactive", {
      "checked": this.props.model.field().isChecked()
    });

    return (
      <div className="placedfield" style={this.fieldStyle()}>
        <div className={checkboxClassName}></div>
      </div>
    );
  }
});
