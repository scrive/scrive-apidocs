/** @jsx React.DOM */

var imports = ["React", "designview/typesetters/typesettermixin",
               "designview/typesetters/signatoryselector",
               "designview/typesetters/obligatory", "designview/typesetters/done",
               "legacy_code"];

define(imports, function (React, TypeSetterMixin, SignatorySelector, Obligatory, Done) {

  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.signatureTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.signatureTypeSetterHorizontalOffset,

    renderTitle: function () {
      return localization.designview.signatureBoxSettings;
    },

    renderBody: function () {
      var self = this;
      var field = self.props.model.field();

      return (
        <span>
          <SignatorySelector
            className="signature-field-placement-setter-field-selector"
            textWidth={191}
            optionsWidth="218px"
            field={field}
          />
          <Obligatory field={field} />
          <Done field={field} onDone={this.done} />
        </span>
      );
    }
  });

});
