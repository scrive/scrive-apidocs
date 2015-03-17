/** @jsx React.DOM */

var imports = ["React", "designview/typesetters/typesettermixin",
               "designview/typesetters/signatoryselector",
               "designview/typesetters/obligatory", "designview/typesetters/done",
               "designview/typesetters/remove", "legacy_code"];

define(imports, function (React, TypeSetterMixin, SignatorySelector, Obligatory, Done, Remove) {

  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.signatureTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.signatureTypeSetterHorizontalOffset,

    renderBody: function () {
      var field = this.props.model.field();

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
          <Remove model={this.props.model} onRemove={this.clear} />
        </span>
      );
    }
  });

});
