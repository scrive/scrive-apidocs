var _ = require("underscore");
var React = require("react");
var Select = require("../../common/select");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

  module.exports = React.createClass({
    currSize: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));
      return model.fsrel() * page.width();
    },
    smallFontSelected: function () {
      return Math.abs(this.currSize() - FieldPlacementGlobal.fontSizeSmall) < 1;
    },
    normalFontSelected: function () {
      return Math.abs(this.currSize() - FieldPlacementGlobal.fontSizeNormal) < 1;
    },
    largeFontSelected: function () {
      return Math.abs(this.currSize() - FieldPlacementGlobal.fontSizeLarge) < 1;
    },
    hugeFontSelected: function () {
      return Math.abs(this.currSize() - FieldPlacementGlobal.fontSizeHuge) < 1;
    },
    customFontSelected: function () {
      return !this.smallFontSelected() && !this.normalFontSelected() &&
             !this.largeFontSelected() && !this.hugeFontSelected();
    },
    fontOptions: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      var options = [
        {
          name: localization.fontSize.small,
          selected: this.smallFontSelected(),
          onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeSmall / page.width()); }
        },
        {
          name: localization.fontSize.normal,
          selected: this.normalFontSelected(),
          onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeNormal / page.width()); }
        },
        {
          name: localization.fontSize.big,
          selected: this.largeFontSelected(),
          onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeLarge / page.width()); }
        },
        {
          name: localization.fontSize.large,
          selected: this.hugeFontSelected(),
          onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeHuge / page.width()); }
        }
      ];
      if (this.customFontSelected()) {
        options.unshift({
          name: localization.fontSize.custom,
          selected: true,
          disabled: true
        })
      }
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
          {localization.fontSize.name}
          <div className="fieldTypeSetter-subtitle-select">
            <Select
              options={this.fontOptions()}
              width={218}
              className="typesetter-obligatory-option"
            />
          </div>
        </div>
      );
    }
  });
