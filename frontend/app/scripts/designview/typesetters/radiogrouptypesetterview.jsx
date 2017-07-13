var classNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var Button = require("../../common/button");
var Done = require("./done");
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var InfoTextInput = require("../../common/infotextinput");
var More = require("./more");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var RadioButtonSizeSelector = require("./radiobuttonsizeselector");
var SignatorySelector = require("./signatoryselector");
var TypeSetterMixin = require("./typesettermixin");

var RadioButtonEditorView = React.createClass({
  propTypes: {
    index: React.PropTypes.number.isRequired,
    placement: React.PropTypes.instanceOf(FieldPlacement).isRequired
  },
  getInitialState: function () {
    return {
      isValueValid: this.isValueValid(this.getPlacementValue())
    };
  },
  getPlacementValue: function () {
    return this.props.placement.field().getRadioButtonValue(this.props.index);
  },
  isValueValid: function (value) {
    var self = this;

    var isUnique = _.every(
      _.map(
        this.props.placement.field().radioButtonValues(),
        function (otherValue, index) {
          if (otherValue == value) {
            return index != self.props.index;
          }

          return false;
        }
      ),
      function (item) {
        return (item == false);
      }
    );

    return (value != "") && isUnique;
  },
  onValueInputBlur: function () {
    this.props.placement.setHighlighted(false);
  },
  onValueInputChange: function (newName) {
    this.setState({isValueValid: this.isValueValid(newName)});

    this.props.placement.field().setRadioButtonValue(
      this.props.index, newName
    );
  },
  onValueInputFocus: function () {
    this.props.placement.setHighlighted(true);
  },
  onValueInputMouseEnter: function () {
    this.props.placement.setHighlighted(true);
  },
  onValueInputMouseLeave: function () {
    if (!this.refs.inputValue.hasFocus()) {
      this.props.placement.setHighlighted(false);
    }
  },
  onRemoveButtonClick: function (event) {
    event.stopPropagation();
    event.preventDefault();

    this.props.placement.field().removePlacement(this.props.placement);
  },
  render: function () {
    var inputClassName = classNames({
      "redborder": !this.state.isValueValid
    });

    return (
      <div className="subtitle">
        <div className="fieldTypeSetter-subtitle-select radiobutton-editor">
          <InfoTextInput
            ref="inputValue"
            className={inputClassName}
            inputStyle={{width: "178px"}}
            value={this.getPlacementValue()}
            onBlur={this.onValueInputBlur}
            onChange={this.onValueInputChange}
            onFocus={this.onValueInputFocus}
            onMouseEnter={this.onValueInputMouseEnter}
            onMouseLeave={this.onValueInputMouseLeave}
          />
          {/* if */ this.props.placement.field().placements().length > 2 &&
            <a
              className="remove-radiobutton"
              onClick={this.onRemoveButtonClick}
            />
          }
        </div>
      </div>
    );
  }
});

var RadioGroupTypesetterView = React.createClass({
  mixins: [TypeSetterMixin],
  horizontalOffset: FieldPlacementGlobal.textTypeSetterArrowOffset,
  verticalOffset: 0,
  onRemoveRadioGroupButtonClick: function () {
    this.props.model.field().remove();
  },
  getContainerClassName: function () {
    return "radiogroup-typesetter";
  },
  onDone: function () {
    var allUnique = this.props.model.field().areRadioButtonValuesUnique();

    if (!allUnique) {
      new FlashMessage({
        type: "error",
        content: localization.designview.radioButtonNamesNotUnique
      });
    } else {
      this.done();
    }
  },
  renderBody: function () {
    return (
      <span>
        <SignatorySelector field={this.props.model.field()} />
        <RadioButtonSizeSelector model={this.props.model} />
        <More text={localization.designview.radioButtonNames}>
          {_.map(this.props.model.field().placements(), function (item, index) {
            return (
              <RadioButtonEditorView
                key={item.cid}
                index={index}
                placement={item}
              />
            );
          })}
        </More>
        <Done field={this.props.model.field()} onDone={this.onDone} />
        <Button
          ref="removeButton"
          size="tiny"
          text={localization.designview.textFields.remove}
          className="fieldTypeSetter-button"
          onClick={this.onRemoveRadioGroupButtonClick}
        />
      </span>
    );
  }
});


module.exports = {
  RadioButtonEditorView: RadioButtonEditorView,
  RadioGroupTypesetterView: RadioGroupTypesetterView
};
