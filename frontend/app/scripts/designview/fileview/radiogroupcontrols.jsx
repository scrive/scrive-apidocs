var classNames = require("classnames");
var React = require("react");
var $ = require("jquery");

var DraggableMixin = require("../editdocument/draggablemixin");
var DrawingUtils = require("../../common/drawing_utils");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var IconAddRadioButton = require("../../icons/add-radio-button.svg");
var RadioButton = require("../../icons/radiobutton");

var BackgroundView = React.createClass({
  propTypes: {
    height: React.PropTypes.number.isRequired,
    visible: React.PropTypes.bool.isRequired,
    width: React.PropTypes.number.isRequired,
    x: React.PropTypes.number.isRequired,
    y: React.PropTypes.number.isRequired
  },
  render: function () {
    var style = {
      display: (this.props.visible ? "block" : "none"),
      height: this.props.height,
      left: this.props.x,
      top: this.props.y,
      width: this.props.width
    };

    return (
      <div className="background" style={style}></div>
    );
  }
});

var RadioButtonView = React.createClass({
  mixins: [DraggableMixin],
  propTypes: {
    pageHeight: React.PropTypes.number.isRequired,
    pageWidth: React.PropTypes.number.isRequired,
    placement: React.PropTypes.instanceOf(FieldPlacement).isRequired,
    closeAllTypeSetters: React.PropTypes.func.isRequired,
    hideCoordinateAxes: React.PropTypes.func.isRequired,
    moveCoordinateAxes: React.PropTypes.func.isRequired,
    showCoordinateAxes: React.PropTypes.func.isRequired,
    onDrag: React.PropTypes.func.isRequired,
    onDragStart: React.PropTypes.func.isRequired,
    onDropOnPage: React.PropTypes.func.isRequired,
    onDropOutside: React.PropTypes.func.isRequired
  },
  initDraggable: function () {
    var self = this;

    self.initializeDraggable({
      el: $(self.getDOMNode()),
      verticalOffset: 0,
      xAxisOffset: FieldPlacementGlobal.radioButtonWrapperPadding,
      yAxisOffset: FieldPlacementGlobal.radioButtonWrapperPadding,
      dropXOffset: FieldPlacementGlobal.radioButtonWrapperPadding,
      dropYOffset: FieldPlacementGlobal.radioButtonWrapperPadding,
      onStart: function () {
        self.props.onDragStart();
      },
      onDropOnPage: function (page, x, y, pageW, pageH) {
        self.props.onDropOnPage(
          self.props.placement, page, x, y, pageW, pageH
        );
      },
      onDropOutside: function () {
        self.props.onDropOutside(self.props.placement);
      }
    });

    $(self.getDOMNode()).on("drag", this.onDrag);
  },
  componentDidMount: function () {
    this.initDraggable();
  },
  onDrag: function (event, ui) {
    this.props.onDrag(this.props.placement, event, ui);
  },
  render: function () {
    var size = DrawingUtils.RelToAbs(
      this.props.placement.wrel(), this.props.pageWidth
    );

    var wrapperPadding = FieldPlacementGlobal.radioButtonWrapperPadding;

    var style = {
      height: size,
      left: DrawingUtils.RelToAbs(
        this.props.placement.xrel(), this.props.pageWidth
      ) - wrapperPadding,
      padding: wrapperPadding,
      top: DrawingUtils.RelToAbs(
        this.props.placement.yrel(), this.props.pageHeight
      ) - wrapperPadding,
      width: size
    };

    var className = classNames("control", {
      highlighted: this.props.placement.highlighted()
    });

    return (
      <div className={className} style={style}>
        <RadioButton
          active={true}
          selected={this.props.placement.field().isRadioButtonSelected(this.props.placement)}
          pageWidth={this.props.pageWidth}
          wrel={this.props.placement.wrel()}
        />
      </div>
    );
  }
});

var AddButton = React.createClass({
  mixins: [React.PureRenderMixin],
  propTypes: {
    width: React.PropTypes.number.isRequired,
    x: React.PropTypes.number.isRequired,
    y: React.PropTypes.number.isRequired,
    onClick: React.PropTypes.func.isRequired
  },
  render: function () {
    var style = {
      height: this.props.width,
      left: this.props.x,
      top: this.props.y,
      visibility: (this.props.visible ? "visible" : "hidden"),
      width: this.props.width
    };

    return (
      <a className="control add-button" style={style} onClick={this.props.onClick}>
        <IconAddRadioButton className="icon" />
      </a>
    );
  }
});

module.exports = {
  AddButton: AddButton,
  BackgroundView: BackgroundView,
  RadioButtonView: RadioButtonView
};
