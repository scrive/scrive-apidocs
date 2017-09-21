var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var CustomPatternNameMixin = require("./custompatternnamemixin");
var CustomValidation = require(
  "../../../js/customvalidations.js"
).CustomValidation;
var HtmlTextWithSubstitution = require(
  "../../common/htmltextwithsubstitution"
);
var InfoTextInput = require("../../common/infotextinput");
var Modal = require("../../common/modal");
var Select = require("../../common/select");

var ValidatedInfoTextInput = React.createClass({
  render: function () {
    var className = undefined;
    if (this.props.valid !== null) {
      className = classNames({
        greenborder: (this.props.valid === true),
        redborder: (this.props.valid === false)
      });
    }

    var inputProps = _.extendOwn({}, this.props, {
      className: className
    });
    inputProps.valid = undefined;

    return <InfoTextInput ref="input" {...inputProps} />;
  }
});

var RemainingCharactersCounter = React.createClass({
  propTypes: {
    maxLength: React.PropTypes.number.isRequired,
    value: React.PropTypes.string
  },
  render: function () {
    var remaining = this.props.maxLength;
    if (typeof this.props.value === "string") {
      remaining -= this.props.value.length;
    }

    return (
      <label className="remaining-characters-counter">
        &nbsp;(
          <HtmlTextWithSubstitution
            secureText={localization.designview.customValidation.remainingCharacters}
            subs={{
              ".put-remaining-characters-count-here": "" + remaining
            }}
          />
        )
      </label>
    );
  }
});

var CustomValidationEditorModal = React.createClass({
  mixins: [CustomPatternNameMixin],
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    model: React.PropTypes.instanceOf(CustomValidation),
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      predefinedPatternKey: undefined,
      pattern: "",
      patternValid: null,
      validExample: "",
      validExampleValid: null,
      tooltipMessage: "",
      tooltipMessageValid: null
    };
  },
  getStateFromModel: function (model) {
    return {
      predefinedPatternKey: CustomValidation.predefinedPatternKey(model.pattern()),
      pattern: model.pattern(),
      patternValid: null,
      validExample: model.validExample(),
      validExampleValid: null,
      tooltipMessage: model.tooltipMessage(),
      tooltipMessageValid: null
    };
  },
  componentWillReceiveProps: function (nextProps) {
    var becameVisible = (nextProps.active != this.props.active && nextProps.active);
    if ((nextProps.model != this.props.model) || becameVisible) {
      var newState = this.getInitialState();

      if (nextProps.model) {
        newState = this.getStateFromModel(nextProps.model);
      }

      this.setState(newState);
    }
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevProps.model && this.state.pattern != prevState.pattern) {
      var newState = {
        predefinedPatternKey: CustomValidation.predefinedPatternKey(this.state.pattern)
      };

      if (this.state.patternValid) {
        newState = _.extendOwn({}, newState, {
          validExampleValid: this.validateValidExample(this.state.validExample)
        });
      } else {
        newState = _.extendOwn({}, newState, {
          validExampleValid: null
        });
      }

      this.setState(newState);
    }
  },
  componentDidMount: function () {
    if (this.props.model) {
      this.setState(this.getStateFromModel(this.props.model));
    }
  },
  patternSelectOptions: function () {
    var self = this;

    var options = _.map(
      _.keys(CustomValidation.PATTERNS),
      function (key) {
        return {
          name: self.getCustomPatternName(key),
          selected: (self.state.predefinedPatternKey == key),
          onSelect: function () {
            self.onPredefinedPatternKeyOptionSelect(key);
          }
        };
      }
    );

    options.unshift({
      name: localization.designview.customValidation.custom,
      selected: (!self.state.predefinedPatternKey),
      onSelect: function () {
        self.onPredefinedPatternKeyOptionSelect();
      }
    });

    return options;
  },
  validatePattern: function (pattern) {
    if (!pattern) {
      return false;
    }

    var result = true;
    try {
      new RegExp(pattern);
    } catch (err) {
      result = false;
    }

    return result;
  },
  validateExample: function (example) {
    var result = null;
    var regex = null;

    if (!this.state.pattern) {
      return null;
    }

    try {
      regex = new RegExp(this.state.pattern);
    } catch (err) {
      // pass
    }

    if (regex) {
      if (!example) {
        result = false;
      } else {
        result = regex.test(example);
      }
    }

    return result;
  },
  validateValidExample: function (validExample) {
    return this.validateExample(validExample);
  },
  validateTooltipMessage: function (message) {
    return message != "" && message.length <= 140;
  },
  onAcceptButtonClick: function () {
    var validationState = {
      patternValid: this.validatePattern(this.state.pattern),
      validExampleValid: this.validateValidExample(this.state.validExample),
      tooltipMessageValid: this.validateTooltipMessage(this.state.tooltipMessage)
    };

    var allValid = _.all(_.values(validationState));
    if (!allValid) {
      this.setState(validationState);
    } else {
      this.props.model.setPattern(this.state.pattern);
      this.props.model.setValidExample(this.state.validExample);
      this.props.model.setTooltipMessage(this.state.tooltipMessage);
      this.props.onClose();
    }
  },
  onPatternInputChange: function (newValue) {
    this.setState({
      pattern: newValue,
      patternValid: this.validatePattern(newValue)
    });
  },
  onPredefinedPatternKeyOptionSelect: function (newKey) {
    var newState = {predefinedPatternKey: newKey};

    if (newKey) {
      var newPattern = CustomValidation.PATTERNS[newKey];
      newState = _.extendOwn({}, newState, {
        pattern: newPattern,
        patternValid: this.validatePattern(newPattern)
      });
    } else {
      newState = _.extendOwn({}, newState, {
        pattern: "",
        patternValid: null
      });
    }

    this.setState(newState);
  },
  onTooltipMessageInputChange: function (newValue) {
    this.setState({
      tooltipMessage: newValue,
      tooltipMessageValid: this.validateTooltipMessage(newValue)
    });
  },
  onValidExampleInputChange: function (newValue) {
    this.setState({
      validExample: newValue,
      validExampleValid: this.validateValidExample(newValue)
    });
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active}>
        <Modal.Header
          title={localization.designview.customValidation.fieldValidation}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          {this.props.model &&
            <div className="custom-validation-editor">
              <Select
                ref="patternSelect"
                options={this.patternSelectOptions()}
                width={568}
              />

              <label>{localization.designview.customValidation.pattern}</label>
              <ValidatedInfoTextInput
                ref="patternInput"
                infotext={localization.designview.customValidation.patternPlaceholder}
                value={this.state.pattern}
                valid={this.state.patternValid}
                onChange={this.onPatternInputChange}
              />

              <label>{localization.designview.customValidation.validExample}</label>
              <ValidatedInfoTextInput
                ref="validExampleInput"
                infotext={localization.designview.customValidation.validExamplePlaceholder}
                valid={this.state.validExampleValid}
                value={this.state.validExample}
                onChange={this.onValidExampleInputChange}
              />

              <label>{localization.designview.customValidation.tooltipMessage}</label>
              <RemainingCharactersCounter
                ref="remainingCharactersCounter"
                maxLength={140}
                value={this.state.tooltipMessage}
              />
              <ValidatedInfoTextInput
                ref="tooltipMessageInput"
                infotext={localization.designview.customValidation.tooltipMessagePlaceholder}
                maxLength={140}
                valid={this.state.tooltipMessageValid}
                value={this.state.tooltipMessage}
                onChange={this.onTooltipMessageInputChange}
              />
            </div>
          }
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            ref="acceptModalButton"
            text={localization.save}
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = {
  CustomValidationEditorModal: CustomValidationEditorModal,
  RemainingCharactersCounter: RemainingCharactersCounter,
  ValidatedInfoTextInput: ValidatedInfoTextInput
};
