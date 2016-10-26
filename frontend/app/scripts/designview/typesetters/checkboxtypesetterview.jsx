var React = require("react");
var Checkbox = require("../../common/checkbox");
var TypeSetterMixin = require("./typesettermixin");
var SignatorySelector = require("./signatoryselector");
var Obligatory = require("./obligatory");
var More = require("./more");
var Anchor = require("./anchor");
var Done = require("./done");
var Remove = require("./remove");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var CheckboxSizeSelector = require("./checkboxsizeselector");


  module.exports = React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.checkboxTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.checkboxTypeSetterHorizontalOffset,

    handlePrecheck: function (checked) {
      this.props.model.field().setChecked(checked);
    },

    handleSelect: function (s) {
      var field = this.props.model.field();
      if (s.author()) {
        field.setChecked(true);
      } else {
        field.setChecked(false);
      }
    },

    renderBody: function () {
      var field = this.props.model.field();

      return (
        <span>
          <SignatorySelector field={field} onSelect={this.handleSelect} />
          <CheckboxSizeSelector model={this.props.model} />
          <div className="fieldTypeSetter-option checkbox-box">
            <Checkbox
              label={localization.designview.checkboxes.prechecked}
              checked={field.isChecked()}
              onChange={this.handlePrecheck}
            />
          </div>
          <Obligatory field={field} />
          <More>
            <Anchor model={this.props.model} />
          </More>
          <Done field={field} onDone={this.done} />
          <Remove model={this.props.model} />
        </span>
      );
    }
  });
