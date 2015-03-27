/** @jsx React.DOM */

define(["React", "common/checkbox", "designview/typesetters/typesettermixin",
        "designview/typesetters/signatoryselector",
        "designview/typesetters/obligatory", "designview/typesetters/more",
        "designview/typesetters/anchor", "designview/typesetters/done",
        "designview/typesetters/remove", "legacy_code"],
  function (React, Checkbox, TypeSetterMixin, SignatorySelector, Obligatory, More, Anchor, Done, Remove) {

  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.checkboxTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.checkboxTypeSetterHorizontalOffset,

    handlePrecheck: function (checked) {
      var field = this.props.model.field();
      field.setValue(checked ? "checked" : "");
    },

    handleSelect: function (s) {
      var field = this.props.model.field();
      if (s.author()) {
        field.setValue("checked");
      } else {
        field.setValue("");
      }
    },

    renderBody: function () {
      var field = this.props.model.field();

      return (
        <span>
          <SignatorySelector field={field} onSelect={this.handleSelect} />
          <div className="fieldTypeSetter-option checkbox-box">
            <Checkbox
              label={localization.designview.checkboxes.prechecked}
              checked={field.value() != undefined && field.value() != ""}
              onChange={this.handlePrecheck}
            />
          </div>
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
