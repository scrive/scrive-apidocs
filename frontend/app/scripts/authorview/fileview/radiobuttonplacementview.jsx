var React = require("react");

var DrawingUtils = require("../../common/drawing_utils");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var RadioButton = require("../../icons/radiobutton");

var RadioButtonPlacementView = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
    pageHeight: React.PropTypes.number.isRequired,
    pageWidth: React.PropTypes.number.isRequired
  },
  fieldStyle: function () {
    var size = DrawingUtils.RelToAbs(
      this.props.model.wrel(), this.props.pageWidth
    );

    return {
      left: DrawingUtils.RelToAbs(
        this.props.model.xrel(), this.props.pageWidth
      ),
      top: DrawingUtils.RelToAbs(
        this.props.model.yrel(), this.props.pageHeight
      ),
      height: size,
      width: size
    };
  },
  render: function () {
    var selected = this.props.model.field().isRadioButtonSelected(
      this.props.model
    );

    return (
      <div className="placedfield placedradiobutton" style={this.fieldStyle()}>
        <RadioButton
          active={false}
          pageWidth={this.props.pageWidth}
          selected={selected}
          wrel={this.props.model.wrel()}
        />
      </div>
    );
  }
});

module.exports = RadioButtonPlacementView;
