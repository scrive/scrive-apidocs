/** @jsx React.DOM */

define(["Underscore", "React", "common/button", "common/select",
        "designview/typesetters/typesettermixin", "designview/typesetters/done",
        "designview/typesetters/remove", "designview/typesetters/signatoryselector",
        "designview/typesetters/more", "designview/typesetters/fieldselector",
        "designview/typesetters/fontselector", "legacy_code"],
  function (_, React, Button, Select, TypeSetterMixin, Done, Remove, SignatorySelector,
            More, FieldSelector, FontSelector) {
  return React.createClass({
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.textTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.textTypeSetterArrowOffset,

    obligatorySelected: function () {
      var model = this.props.model;
      var field = model.field();

      if (field.isAuthorUnchangeableField()) {
        return localization.designview.mandatoryForSender;
      }

      if (field.isOptional()) {
        return localization.designview.optionalField;
      }

      if (field.shouldbefilledbysender()) {
        return localization.designview.mandatoryForSender;
      }

      return localization.designview.mandatoryForRecipient;
    },

    obligatoryOptions: function () {
      var model = this.props.model;
      var field = model.field();

      var options = [];

      if (field.isAuthorUnchangeableField()) {
        return options;
      }

      if (!field.shouldbefilledbysender()) {
        options.push({
          name: localization.designview.mandatoryForSender,
          onSelect: function () {
            field.makeObligatory();
            field.setShouldBeFilledBySender(true);
            field.authorObligatory = "sender";
            field.addedByMe = false;
          }
        });
      }

      if ((field.shouldbefilledbysender() || field.isOptional()) && field.canBeSetByRecipent()) {
        options.push({
          name: localization.designview.mandatoryForRecipient,
          onSelect: function () {
            field.makeObligatory();
            field.setShouldBeFilledBySender(false);
            field.authorObligatory = "signatory";
            field.addedByMe = false;
          }
        });
      }

      if (!field.isOptional() && field.canBeOptional()) {
        options.push({
          name: localization.designview.optionalField,
          onSelect: function () {
            field.makeOptional();
            field.setShouldBeFilledBySender(false);
            field.authorObligatory = "optional";
            field.addedByMe = false;
          }
        });
      }

      return options;
    },

    handleDone: function () {
      var model = this.props.model;
      var field = model.field();
      mixpanel.track("Click save inline field");
      field.makeReady();
      model.cleanTypeSetter();
      model.trigger("change:step");
    },

    handleSelectSig: function (sig) {
      var model = this.props.model;
      var field = model.field();
      var existingField = sig.field(field.name(), field.type());

      this.refs.fieldSelector.selector();

      if (existingField) {
        return model.changeField(existingField);
      }

      model.changeField(sig.field("fstname", "standard"));
    },

    handleSave: function (text) {
      if (this.rename(text)) {
        this.refs.fieldSelector.selector();
      }
    },

    renderTitle: function () {
      return null; // no title in text type setter.
    },

    renderBody: function () {
      var model = this.props.model;
      var field = model.field();

      var options = this.obligatoryOptions();

      return (
        <span>
          <SignatorySelector field={field} useDefaultBehavior={false} onSelect={this.handleSelectSig} />
          <FieldSelector ref="fieldSelector" model={model} onSave={this.handleSave} />
          <div className="subtitle">
            {localization.designview.optionalMandatory}
            <div className="fieldTypeSetter-subtitle-select">
              <Select.Select
                name={this.obligatorySelected()}
                options={options}
                optionsWidth={"218px"}
                textWidth={191}
                cssClass={"typesetter-obligatory-option"}
                style={{fontSize: "16px"}}
                inactive={options.length === 0}
              />
            </div>
          </div>
          <More>
            <FontSelector model={model} />
          </More>
          <Done field={field} onDone={this.handleDone} />
          <Remove model={model} onRemove={this.clear} />
        </span>
      );
    }
  });
});
