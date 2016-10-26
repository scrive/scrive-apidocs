var React = require("react");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var Checkbox = require("../../icons/checkbox");

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
    pageWidth: React.PropTypes.number.isRequired,
    pageHeight: React.PropTypes.number.isRequired
  },
  fieldStyle: function () {
    var pageWidth = this.props.pageWidth;
    var pageHeight = this.props.pageHeight;
    var size = Math.round(this.props.model.wrel() * pageWidth);
    return {
      left: Math.round(this.props.model.xrel() * pageWidth),
      top: Math.round(this.props.model.yrel() * pageHeight),
      height: size,
      width: size
    };
  },
  render: function () {
    var pageWidth = this.props.pageWidth;
    var size = Math.round(this.props.model.wrel() * pageWidth);

    return (
      <div className="placedfield placedcheckbox" style={this.fieldStyle()}>
        <Checkbox
          wrel={this.props.model.wrel()}
          pageWidth={pageWidth}
          checked={this.props.model.field().isChecked()}
          active={false}
        />
      </div>
    );
  }
});
