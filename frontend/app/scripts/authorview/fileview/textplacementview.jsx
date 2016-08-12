var React = require("react");

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

    var fieldStyle = {
      fontSize: Math.floor(this.props.model.fsrel() * pageWidth),
      left: Math.floor(this.props.model.xrel() * pageWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
      top: Math.floor(this.props.model.yrel() * pageHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset
    };

    if (!this.props.model.field().value()) {
      fieldStyle.display = "none";
    }

    return fieldStyle;
  },
  render: function () {
    return (
      <div className="placedfield" style={this.fieldStyle()}>
        <div className="placedfield-placement-wrapper">
          <div style={{padding: FieldPlacementGlobal.textPlacementSpacingString}}>
            {this.props.model.field().nicetext()}
          </div>
        </div>
      </div>
    );
  }
});
