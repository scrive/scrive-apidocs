/** @jsx React.DOM */

define(["Underscore", "React", "common/select", "legacy_code"],
  function (_, React, Select) {
  return React.createClass({
    fontSelected: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      var currSize = model.fsrel() * page.width();

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeSmall) < 1) {
        return localization.fontSize.small;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeNormal) < 1) {
        return localization.fontSize.normal;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeLarge) < 1) {
        return localization.fontSize.big;
      }

      if (Math.abs(currSize - FieldPlacementGlobal.fontSizeHuge) < 1) {
        return localization.fontSize.large;
      }

      return localization.fontSize.custom;
    },

    fontOptions: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(model.get("page"));

      return [
        {name: localization.fontSize.small,
         style: {fontSize: FieldPlacementGlobal.fontSizeSmall + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeSmall / page.width()); }},
        {name: localization.fontSize.normal,
         style: {fontSize: FieldPlacementGlobal.fontSizeNormal + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeNormal / page.width()); }},
        {name: localization.fontSize.big,
         style: {fontSize: FieldPlacementGlobal.fontSizeLarge + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeLarge / page.width()); }},
        {name: localization.fontSize.large,
         style: {fontSize: FieldPlacementGlobal.fontSizeHuge + "px"},
         onSelect: function () { model.setFSRel(FieldPlacementGlobal.fontSizeHuge / page.width()); }}
      ];
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
            <Select.Select
              name={this.fontSelected()}
              options={this.fontOptions()}
              optionsWidth="218px"
              textWidth={191}
              cssClass="typesetter-obligatory-option"
              style={{fontSize: "16px"}}
            />
          </div>
        </div>
      );
    }
  });
});
