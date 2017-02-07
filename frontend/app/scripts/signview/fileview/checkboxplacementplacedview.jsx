var React = require("react");
var PlacementMixin = require("./placement_mixin");
var TaskMixin = require("../navigation/task_mixin");
var Task = require("../navigation/task");
var $ = require("jquery");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var classNames = require("classnames");

var Checkbox = require("./signviewcheckbox");

  module.exports = React.createClass({
    mixins: [PlacementMixin, TaskMixin],

    createTasks: function () {
      var self = this;
      var placement = self.props.model;
      var field = placement.field();

      if (!field.signatory().current()) {
        return;
      }

      return [new Task({
        type: "field",
        field: field,
        isComplete: function () {
          return placement.field().readyForSign();
        },
        el: $(self.getDOMNode()),
        margin: 5,
        onArrowClick: function () {
          self.toggleCheck();
        },
        tipSide: placement.tip()
      })];
    },

    toggleCheck: function () {
      var field = this.props.model.field();
      var doc = field.signatory().document();
      var current = field.signatory() == doc.currentSignatory() &&
        doc.currentSignatoryCanSign();

      if (current) {
        field.setChecked(!field.isChecked());
      }
    },

    onMouseDown: function (event) {
      event.stopPropagation();
      event.preventDefault();
    },

    render: function () {
      var field = this.props.model.field();
      var doc = field.signatory().document();
      var current = field.signatory() == doc.currentSignatory() &&
        doc.currentSignatoryCanSign();
      var checked = field.isChecked();

      var divClass = classNames({
        "placedfield": true,
        "js-checkbox": true,
        "to-fill-now": current,
        "obligatory": field.obligatory()
      });

      var width = Math.round(this.width());
      var top = Math.round(this.top());
      var left = Math.round(this.left());

      var divStyle = {
        cursor: current ? "pointer" : "",
        top: top,
        left: left,
        width: width,
        height: width
      };

      var boxStyle = {
        width: width,
        height: width
      };

      return (
        <div onMouseDown={this.onMouseDown} onClick={this.toggleCheck} className={divClass} style={divStyle}>
          <div className="placedcheckbox" style={boxStyle} >
            <div
              className="checkboxClickableArea"
              style={{
                width: FieldPlacementGlobal.checkboxClickableAreaWidth,
                height: FieldPlacementGlobal.checkboxClickableAreaHeight,
                marginTop: Math.round(-0.5 * FieldPlacementGlobal.checkboxClickableAreaWidth + 0.5 * this.width()),
                marginLeft: Math.round((-0.5 * FieldPlacementGlobal.checkboxClickableAreaHeight  + 0.5 * this.width()))
              }}
            />
            <Checkbox
              wrel={this.props.model.wrel()}
              pageWidth={this.props.pageWidth}
              checked={field.isChecked()}
              active={current}
            />
          </div>
        </div>
      );
    }
  });
