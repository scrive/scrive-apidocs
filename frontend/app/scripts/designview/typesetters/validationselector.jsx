var React = require("react");

var Button = require("../../common/button");
var CustomPatternNameMixin = require("./custompatternnamemixin");
var CustomValidation = require(
  "../../../js/customvalidations.js"
).CustomValidation;
var CustomValidationEditorModal = require(
  "./customvalidationeditormodal"
).CustomValidationEditorModal;

var ValidationSelector = React.createClass({
  mixins: [CustomPatternNameMixin],
  getInitialState: function () {
    return {
      showModal: false
    };
  },
  onEditCustomPatternButtonClick: function () {
    if (!this.props.model.field().hasCustomValidation()) {
      this.props.model.field().setCustomValidation(new CustomValidation());
    }

    this.setState({showModal: true});
  },
  onRemoveCustomPatternButtonClick: function () {
    this.props.model.field().setCustomValidation(null);
  },
  onCustomValidationEditorModalClose: function () {
    this.setState({showModal: false});
  },
  customValidationPatternName: function () {
    var predefinedPatternKey = CustomValidation.predefinedPatternKey(
      this.props.model.field().customValidation().pattern()
    );

    if (!predefinedPatternKey) {
      return localization.designview.customValidation.custom;
    }

    return this.getCustomPatternName(predefinedPatternKey);
  },
  render: function () {
    var hasCustomValidation = this.props.model.field().hasCustomValidation();
    var buttonText = localization.designview.addAnchor;
    if (hasCustomValidation) {
      buttonText = localization.designview.editField;
    }

    return (
      <div className="subtitle validation-selector">
        <span>{localization.designview.customValidation.validation}</span>
        <div className="fieldTypeSetter-subtitle-select">
          {hasCustomValidation &&
            <span className="fieldTypeSetter-subtitle-anchor pattern-name">
              {this.customValidationPatternName()}
            </span>
          }
          <Button
            size="tiny"
            text={buttonText}
            onClick={this.onEditCustomPatternButtonClick}
          />
          {hasCustomValidation &&
            <a
              className="remove-custom-validation"
              ref="removeCustonPatternButton"
              onClick={this.onRemoveCustomPatternButtonClick}
            >
              <span />
            </a>
          }
        </div>

        <CustomValidationEditorModal
          active={this.state.showModal}
          model={this.props.model.field().customValidation()}
          onClose={this.onCustomValidationEditorModalClose}
        />
      </div>
    );
  }
});

module.exports = ValidationSelector;
