/** @jsx React.DOM */

define(["React", "designview/typesetters/typesettermixin",
        "designview/typesetters/signatoryselector",
        "designview/typesetters/obligatory", "designview/typesetters/more",
        "designview/typesetters/anchor", "designview/typesetters/done",
        "designview/typesetters/remove", "legacy_code"],
  function (React, TypeSetterMixin, SignatorySelector, Obligatory, More, Anchor, Done, Remove) {

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
          <More>
            <Anchor model={this.props.model} />
          </More>
          <Done field={field} onDone={this.done} />
          <Remove model={this.props.model} onRemove={this.clear} />
        </span>
      );
    }
  });

});
