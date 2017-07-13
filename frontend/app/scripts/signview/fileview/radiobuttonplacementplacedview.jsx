var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var PlacementMixin = require("./placement_mixin");
var RadioButton = require("../../icons/radiobutton");
var Task = require("../navigation/task");
var TaskMixin = require("../navigation/task_mixin");

var UNDERLAY_PADDING_RATIO = 0.009615; // 10px @ 1040px page width

var RadioButtonPlacementPlacedView = React.createClass({
  mixins: [PlacementMixin, TaskMixin],
  isActive: function () {
    var field = this.props.model.field();
    var doc = field.signatory().document();

    return (
      field.signatory() == doc.currentSignatory() &&
      doc.currentSignatoryCanSign()
    );
  },
  hasSelection: function () {
    return this.props.model.field().readyForSign();
  },
  createTasks: function () {
    var self = this;
    var placement = self.props.model;
    var field = placement.field();

    if (!field.signatory().current()) {
      return;
    }

    var task = new Task({
      type: "field",
      field: field,
      isComplete: function () {
        return placement.field().readyForSign();
      },
      el: $(self.getDOMNode()),
      margin: 6,
      onArrowClick: function () {
        self.onClick();
      },
      tipSide: placement.tip()
    });

    return [task];
  },
  onMouseDown: function (event) {
    event.stopPropagation();
    event.preventDefault();
  },
  onClick: function () {
    if (this.isActive()) {
      this.props.model.field().setSelectedRadioButton(this.props.model);
    }
  },
  render: function () {
    var current = this.isActive();
    var field = this.props.model.field();

    var className = classNames("placedfield", "js-radiobutton", {
      "to-fill-now": current,
      "obligatory": field.obligatory()
    });

    var width = Math.round(this.width());

    var style = {
      cursor: current ? "pointer" : "default",
      top: Math.round(this.top()),
      left: Math.round(this.left()),
      width: width,
      height: width
    };

    var underlayPadding = Math.round(
      this.props.pageWidth * UNDERLAY_PADDING_RATIO
    );

    var underlayWidth = width + underlayPadding;
    var underlayOffset = Math.round((underlayWidth - width) / 2.0) + 1;

    var underlayStyle = {
      height: underlayWidth,
      left: -1 * underlayOffset,
      top: -1 * underlayOffset,
      width: underlayWidth
    };

    var clickableLeft = Math.round(
      -0.5 * FieldPlacementGlobal.radioButtonClickableAreaWidth + 0.5 * width
    );

    var clickableStyle = {
      height: FieldPlacementGlobal.radioButtonClickableAreaWidth,
      left: clickableLeft,
      top: clickableLeft,
      width: FieldPlacementGlobal.radioButtonClickableAreaHeight
    };

    return (
      <div
        className={className}
        style={style}
        onMouseDown={this.onMouseDown}
        onClick={this.onClick}
      >
        {this.isActive() &&
          <div className="clickable" style={clickableStyle}></div>
        }

        {(this.isActive() && !this.hasSelection()) &&
          <div className="underlay" style={underlayStyle}><span></span></div>
        }

        <RadioButton
          active={current}
          pageWidth={this.props.pageWidth}
          selected={field.isRadioButtonSelected(this.props.model)}
          wrel={this.props.model.wrel()}
        />
      </div>
    );
  }
});

module.exports = RadioButtonPlacementPlacedView;
