var React = require("react");
var _ = require("underscore");

var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var Select = require("../../common/select");

var RadioButtonSizeSelector = React.createClass({
  isSizeSelected: function (size) {
    return Math.abs(this.props.model.wrel() - size) < 0.001;
  },
  smallSizeSelected: function () {
    return !this.mediumSizeSelected() && !this.largeSizeSelected();
  },
  mediumSizeSelected: function () {
    return this.isSizeSelected(FieldPlacementGlobal.mediumRadiobuttonRatio);
  },
  largeSizeSelected: function () {
    return this.isSizeSelected(FieldPlacementGlobal.largeRadiobuttonRatio);
  },
  handleChange: function (newSize) {
    _.each(this.props.model.field().placements(), function (item) {
      item.set(
        {
          wrel: newSize,
          hrel: 0,
          fsrel: 0
        },
        {
          silent: true
        }
      );
    });

    this.props.model.field().trigger("change");
  },
  sizeOptions: function () {
    var model = this.props.model;
    var self = this;

    var options = [
      {
        name: localization.checkboxSize.small,
        selected: this.smallSizeSelected(),
        onSelect: function () {
          self.handleChange(FieldPlacementGlobal.smallRadiobuttonRatio);
        }
      },
      {
        name: localization.checkboxSize.medium,
        selected: this.mediumSizeSelected(),
        onSelect: function () {
          self.handleChange(FieldPlacementGlobal.mediumRadiobuttonRatio);
        }
      },
      {
        name: localization.checkboxSize.large,
        selected: this.largeSizeSelected(),
        onSelect: function () {
          self.handleChange(FieldPlacementGlobal.largeRadiobuttonRatio);
        }
      }
    ];

    return options;
  },

  render: function () {
    var model = this.props.model;
    var field = model.field();
    var sig = field.signatory();
    var page = sig.document().mainfile().page(model.get("page"));

    if (page == undefined || page.width() == undefined) {
      return (
        <span />
      );
    }

    return (
      <div className="subtitle">
        {localization.checkboxSize.name}
        <div className="fieldTypeSetter-subtitle-select">
          <Select
            options={this.sizeOptions()}
            width={218}
            className="typesetter-obligatory-option"
          />
        </div>
      </div>
    );
  }
});

module.exports = RadioButtonSizeSelector;
