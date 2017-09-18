var _ = require("underscore");
var React = require("react");
var Button = require("../../common/button");
var Select = require("../../common/select");
var Track = require("../../common/track");
var TypeSetterMixin = require("./typesettermixin");
var Done = require("./done");
var Remove = require("./remove");
var SignatorySelector = require("./signatoryselector");
var More = require("./more");
var FieldSelector = require("./fieldselector");
var FontSelector = require("./fontselector");
var Anchor = require("./anchor");
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;
var ValidationSelector = require("./validationselector");

  module.exports = React.createClass({
    displayName: "TextTypesetterView",
    mixins: [TypeSetterMixin],

    verticalOffset: FieldPlacementGlobal.textTypeSetterVerticalOffset,

    horizontalOffset: FieldPlacementGlobal.textTypeSetterArrowOffset,

    obligatoryOptions: function () {
      var model = this.props.model;
      var field = model.field();

      var options = [];

      options.push({
        name: localization.designview.mandatoryForSender,
        selected: (!field.isOptional() && field.shouldbefilledbysender()) || field.isAuthorUnchangeableField(),
        onSelect: function () {
          field.authorObligatory = "sender";
          field.addedByMe = false;
          field.setObligatoryAndShouldBeFilledBySender(true, true);
        }
      });

      if (field.canBeSetByRecipent() && !field.isAuthorUnchangeableField()) {
        options.push({
          name: localization.designview.mandatoryForRecipient,
          selected: !field.isOptional() && !field.shouldbefilledbysender(),
          onSelect: function () {
            field.authorObligatory = "signatory";
            field.addedByMe = false;
            field.setObligatoryAndShouldBeFilledBySender(true, false);
          }
        });
      }

      if (field.canBeOptional() && !field.isAuthorUnchangeableField()) {
        options.push({
          name: localization.designview.optionalField,
          selected: field.isOptional() && !field.shouldbefilledbysender(),
          onSelect: function () {
            field.authorObligatory = "optional";
            field.addedByMe = false;
            field.setObligatoryAndShouldBeFilledBySender(false, false);
          }
        });
      }

      return options;
    },

    handleDone: function () {
      var model = this.props.model;
      var field = model.field();
      Track.track("Click save inline field");
      field.makeReady();
      model.trigger("change");
    },

    handleSelectSig: function (sig) {
      var model = this.props.model;
      var field = model.field();
      var existingField = sig.field(field.name(), field.type(), field.order());

      this.onNameChange("");
      this.refs.fieldSelector.selector();

      if (existingField) {
        return model.changeField(existingField);
      } else if (sig.fstnameField()) {
        model.changeField(sig.fstnameField());
      } else if (sig.fields().length > 0) {
        model.changeField(sig.fields()[0]);
      }
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
          <FieldSelector
            ref="fieldSelector"
            model={model}
            onSave={this.handleSave}
            onNameChange={this.onNameChange}
          />
          <div className="subtitle">
            {localization.designview.optionalMandatory}
            <div className="fieldTypeSetter-subtitle-select">
              <Select
                options={options}
                width={218}
                className={"typesetter-obligatory-option"}
                inactive={options.length === 0}
              />
            </div>
          </div>
          <More>
            <FontSelector model={model} />
            {field.isCustom() &&
              <ValidationSelector model={model} />
            }
            <Anchor model={model} />
          </More>
          <Done field={field} onDone={this.done} />
          <Remove model={model} />
        </span>
      );
    }
  });
