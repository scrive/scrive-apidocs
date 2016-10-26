var _ = require("underscore");
var React = require("react");
var Select = require("../../common/select");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

  module.exports = React.createClass({
    smallSizeSelected: function () {
      return !this.mediumSizeSelected() && !this.largeSizeSelected();
    },
    mediumSizeSelected: function () {
      return Math.abs(this.props.model.wrel() - FieldPlacementGlobal.mediumCheckboxRatio) < 0.001;
    },
    largeSizeSelected: function () {
      return Math.abs(this.props.model.wrel() - FieldPlacementGlobal.largeCheckboxRatio) < 0.001;
    },
    sizeOptions: function () {
      var model = this.props.model;

      var options = [
        {
          name: localization.checkboxSize.small,
          selected: this.smallSizeSelected(),
          onSelect: function () { model.setCheckboxRel(FieldPlacementGlobal.smallCheckboxRatio); }
        },
        {
          name: localization.checkboxSize.medium,
          selected: this.mediumSizeSelected(),
          onSelect: function () { model.setCheckboxRel(FieldPlacementGlobal.mediumCheckboxRatio); }
        },
        {
          name: localization.checkboxSize.large,
          selected: this.largeSizeSelected(),
          onSelect: function () { model.setCheckboxRel(FieldPlacementGlobal.largeCheckboxRatio); }
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
