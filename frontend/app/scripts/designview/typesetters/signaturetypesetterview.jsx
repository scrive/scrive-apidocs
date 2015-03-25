/** @jsx React.DOM */

define(["React", "designview/typesetters/typesettermixin",
        "designview/typesetters/signatoryselector",
        "designview/typesetters/obligatory", "designview/typesetters/done",
        "designview/typesetters/remove", "legacy_code"],
  function (React, TypeSetterMixin, SignatorySelector, Obligatory, Done, Remove) {

  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.signatureTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.signatureTypeSetterHorizontalOffset,

    renderBody: function () {
      var field = this.props.model.field();

      return (
        <span>
          <SignatorySelector field={field} />
          <Obligatory field={field} />
          <Done field={field} onDone={this.done} />
          <Remove model={this.props.model} onRemove={this.clear} />
        </span>
      );
    }
  });

});
