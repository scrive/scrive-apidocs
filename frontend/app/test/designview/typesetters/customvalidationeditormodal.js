var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var util = require("../../util");

var CustomValidation = require(
  "../../../js/customvalidations.js"
).CustomValidation;
var CustomValidationEditorModal = require(
  "../../../scripts/designview/typesetters/customvalidationeditormodal.jsx"
);
var Modal = require("../../../scripts/common/modal");

describe("designview/typesetters/customvalidationeditormodal", function () {
  var container = null;
  var model = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  describe("ValidatedInfoTextInput", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          infotext: "Spam",
          valid: true,
          value: "spam",
          onChange: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(
          CustomValidationEditorModal.ValidatedInfoTextInput, actualProps
        ),
        container
      );

      return component;
    };

    it("should set valid CSS class when it is valid", function () {
      var component = renderComponent({valid: true});
      assert.equal(component.refs.input.props.className, "greenborder");
    });

    it("should set invalid CSS class when it isn't valid", function () {
      var component = renderComponent({valid: false });
      assert.equal(component.refs.input.props.className, "redborder");
    });

    it("should pass props to the wrapped component", function () {
      var component = renderComponent();

      var input = component.refs.input;
      assert.isUndefined(input.props.valid);
      assert.isDefined(input.props.className);
      assert.equal(input.props.infotext, component.props.infotext);
      assert.equal(input.props.value, component.props.value);
      assert.equal(input.props.onChange, component.props.onChange);
    });
  });

  describe("RemainingCharactersCounter", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          maxLength: 140,
          value: "spam"
        },
        props || {}
      );

      var component = React.render(
        React.createElement(
          CustomValidationEditorModal.RemainingCharactersCounter, actualProps
        ),
        container
      );

      return component;
    };

    it("should not calculate result when value isn't a string", function () {
      var component = renderComponent({value: undefined});
      assert.match($(component.getDOMNode()).text().trim(), /^\(140/);
    });

    it("should not calculate result when value isn't a string", function () {
      var component = renderComponent();
      assert.match($(component.getDOMNode()).text().trim(), /^\(136/);
    });
  });

  describe("CustomValidationEditorModal", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          active: false,
          model: model,
          onClose: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(
          CustomValidationEditorModal.CustomValidationEditorModal, actualProps
        ),
        container
      );

      return component;
    };

    beforeEach(function () {
      model = new CustomValidation({
        pattern: "^[0-9]+?$",
        tooltipMessage: "Spam",
        validExample: "123"
      });
    });

    it("should initialize state with default values if model isn't provided", function () {
      var component = renderComponent({model: undefined});
      assert.isUndefined(component.state.predefinedPatternKey);
      assert.equal(component.state.pattern, "");
      assert.isNull(component.state.patternValid);
      assert.equal(component.state.validExample, "");
      assert.isNull(component.state.validExampleValid);
      assert.equal(component.state.tooltipMessage, "");
      assert.isNull(component.state.tooltipMessageValid);
    });

    it("should get state update from model", function () {
      sinon.stub(CustomValidation, "predefinedPatternKey").returns("email");

      var component = renderComponent();

      var result = component.getStateFromModel(model);
      assert.equal(result.predefinedPatternKey, "email");
      assert.equal(result.pattern, model.pattern());
      assert.isNull(result.patternValid);
      assert.equal(result.validExample, model.validExample());
      assert.isNull(result.validExampleValid);
      assert.equal(result.tooltipMessage, model.tooltipMessage());
      assert.isNull(result.tooltipMessageValid);

      CustomValidation.predefinedPatternKey.restore();
    });

    it("should refresh state when it becomes visible", function () {
      var component = renderComponent({model: undefined});
      sinon.spy(component, "getInitialState");
      sinon.spy(component, "getStateFromModel");
      sinon.spy(component, "setState");

      component.componentWillReceiveProps({model: undefined, active: true});
      assert.isTrue(component.getInitialState.called);
      assert.isFalse(component.getStateFromModel.called);
      assert.isTrue(component.setState.called);
    });

    it("should refresh state when it receives a new model", function () {
      var component = renderComponent({model: undefined});
      sinon.spy(component, "getInitialState");
      sinon.spy(component, "getStateFromModel");
      sinon.spy(component, "setState");

      component.componentWillReceiveProps({model: model, active: false});
      assert.isTrue(component.getInitialState.called);
      assert.isTrue(component.getStateFromModel.calledWith(model));
      assert.isTrue(component.setState.called);
    });

    it("should determine the predefined pattern key when pattern changes", function () {
      sinon.stub(CustomValidation, "predefinedPatternKey").returns("email");

      var component = renderComponent();

      component.setState({pattern: "spam"});
      assert.isTrue(CustomValidation.predefinedPatternKey.calledWith("spam"));
      assert.equal(component.state.predefinedPatternKey, "email");

      CustomValidation.predefinedPatternKey.restore();
    });

    it("should refresh valid example validation state when patterns becomes valid", function () {
      var component = renderComponent();
      sinon.stub(component, "validateValidExample").returns(false);

      component.setState({pattern: "spam", patternValid: true});
      assert.isTrue(
        component.validateValidExample.calledWith(model.validExample())
      );
      assert.isFalse(component.state.validExampleValid);
    });

    it("should refresh valid example validation state when patterns becomes invalid", function () {
      var component = renderComponent();
      sinon.stub(component, "validateValidExample").returns(false);

      component.setState({pattern: "", patternValid: false});
      assert.isFalse(component.validateValidExample.called);
      assert.isNull(component.state.validExampleValid);
    });

    it("should initialize state from model when it mounts", function () {
      var component = renderComponent();
      assert.equal(component.state.pattern, model.pattern());
      assert.isNull(component.state.patternValid);
      assert.equal(component.state.validExample, model.validExample());
      assert.isNull(component.state.validExampleValid);
      assert.equal(component.state.tooltipMessage, model.tooltipMessage());
      assert.isNull(component.state.tooltipMessageValid);
    });

    it("should generate options for select", function () {
      var component = renderComponent();

      var options = component.patternSelectOptions();
      assert.lengthOf(
        options, underscore.keys(CustomValidation.PATTERNS).length + 1
      );
      assert.isTrue(underscore.all(options, function (option) {
        return underscore.isFunction(option.onSelect);
      }));
    });

    it("should select the Custom option if custom pattern is set", function () {
      var component = renderComponent();
      component.setState({predefinedPatternKey: undefined});

      var options = component.patternSelectOptions();
      var selectedOptions = underscore.reduce(
        options,
        function (memo, option, index) {
          if (option.selected) {
            memo.selected.push(index);
          } else {
            memo.deselected.push(index);
          }

          return memo;
        },
        {selected: [], deselected: []}
      );

      assert.lengthOf(selectedOptions.selected, 1);
      assert.equal(selectedOptions.selected[0], 0);
    });

    it("should select one of the predefined options if predefined pattern is set", function () {
      var component = renderComponent();
      component.setState({predefinedPatternKey: "email"});

      var options = component.patternSelectOptions();
      var selectedOptions = underscore.reduce(
        options,
        function (memo, option, index) {
          if (option.selected) {
            memo.selected.push(index);
          } else {
            memo.deselected.push(index);
          }

          return memo;
        },
        {selected: [], deselected: []}
      );

      assert.lengthOf(selectedOptions.selected, 1);
      assert.notEqual(selectedOptions.selected[0], 0);
    });

    it("should invalidate a pattern if it's empty", function () {
      var component = renderComponent();

      var result = component.validatePattern("");
      assert.isFalse(result);
    });

    it("should invalidate a pattern if isn't a valid RegEx", function () {
      var component = renderComponent();

      var result = component.validatePattern("^[0-9");
      assert.isFalse(result);
    });

    it("should validate a pattern", function () {
      var component = renderComponent();

      var result = component.validatePattern("^[0-9]+?$");
      assert.isTrue(result);
    });

    it("should not validate an example if pattern is empty", function () {
      var component = renderComponent();
      component.setState({pattern: ""});

      var result = component.validateExample("spam");
      assert.isNull(result);
    });

    it("should not validate an example if pattern isn't a valid RegEx", function () {
      var component = renderComponent();
      component.setState({pattern: "^[0-9"});

      var result = component.validateExample("spam");
      assert.isNull(result);
    });

    it("should invalidate an example if it's empty", function () {
      var component = renderComponent();

      var result = component.validateExample("");
      assert.isFalse(result);
    });

    it("should invalidate an example if it doesn't match the pattern", function () {
      var component = renderComponent();

      var result = component.validateExample("spam");
      assert.isFalse(result);
    });

    it("should validate an example", function () {
      var component = renderComponent();

      var result = component.validateExample("123");
      assert.isTrue(result);
    });

    it("should validate the valid example", function () {
      var component = renderComponent();
      sinon.stub(component, "validateExample").returns(true);

      var result = component.validateValidExample("spam");
      assert.isTrue(component.validateExample.calledWith("spam"));
      assert.isTrue(result);
    });

    it("should invalidate the tooltip message if it's empty", function () {
      var component = renderComponent();

      var result = component.validateTooltipMessage("");
      assert.isFalse(result);
    });

    it("should invalidate the tooltip message if it's too long", function () {
      var component = renderComponent();

      var message = underscore.map(underscore.range(141), function () {
        return "a";
      });

      var result = component.validateTooltipMessage(message.join(""));
      assert.isFalse(result);
    });

    it("should validate the tooltip message", function () {
      var component = renderComponent();

      var result = component.validateTooltipMessage("spam");
      assert.isTrue(result);
    });

    it("should not update the model if the pattern is invalid when accept button is clicked", function () {
      sinon.stub(model, "setPattern");

      var component = renderComponent();
      sinon.stub(component, "validatePattern").returns(false);

      component.onAcceptButtonClick();
      assert.isTrue(
        component.validatePattern.calledWith(component.state.pattern)
      );
      assert.isFalse(component.state.patternValid);
      assert.isFalse(model.setPattern.called);
      assert.isFalse(component.props.onClose.called);
    });

    it("should not update the model if the valid example is invalid when accept button is clicked", function () {
      sinon.stub(model, "setValidExample");

      var component = renderComponent();
      sinon.stub(component, "validateValidExample").returns(false);

      component.onAcceptButtonClick();
      assert.isTrue(
        component.validateValidExample.calledWith(component.state.validExample)
      );
      assert.isFalse(component.state.validExampleValid);
      assert.isFalse(model.setValidExample.called);
      assert.isFalse(component.props.onClose.called);
    });

    it("should not update the model if the tooltip message is invalid when accept button is clicked", function () {
      sinon.stub(model, "setTooltipMessage");

      var component = renderComponent();
      sinon.stub(component, "validateTooltipMessage").returns(false);

      component.onAcceptButtonClick();
      assert.isTrue(
        component.validateTooltipMessage.calledWith(component.state.tooltipMessage)
      );
      assert.isFalse(component.state.tooltipMessageValid);
      assert.isFalse(model.setTooltipMessage.called);
      assert.isFalse(component.props.onClose.called);
    });

    it("should update the model and close the modal when accept button is clicked", function () {
      sinon.stub(model, "setPattern");
      sinon.stub(model, "setValidExample");
      sinon.stub(model, "setTooltipMessage");

      var component = renderComponent();
      sinon.stub(component, "validatePattern").returns(true);
      sinon.stub(component, "validateValidExample").returns(true);
      sinon.stub(component, "validateTooltipMessage").returns(true);

      component.onAcceptButtonClick();
      assert.isTrue(model.setPattern.calledWith(component.state.pattern));
      assert.isTrue(
        model.setValidExample.calledWith(component.state.validExample)
      );
      assert.isTrue(
        model.setTooltipMessage.calledWith(component.state.tooltipMessage)
      );
      assert.isTrue(component.props.onClose.called);
    });

    it("should update state when the pattern input changes", function () {
      var component = renderComponent();
      sinon.stub(component, "setState");
      sinon.stub(component, "validatePattern").returns(false);

      component.onPatternInputChange("spam");
      assert.isTrue(component.validatePattern.calledWith("spam"));
      assert.isTrue(component.setState.calledWith({
        pattern: "spam",
        patternValid: false
      }));
    });

    it("should update state when custom pattern option is selected", function () {
      var component = renderComponent();
      sinon.stub(component, "setState");

      component.onPredefinedPatternKeyOptionSelect(undefined);
      assert.isTrue(component.setState.calledWith({
        predefinedPatternKey: undefined,
        pattern: "",
        patternValid: null
      }));
    });

    it("should update state when a predefined pattern option is selected", function () {
      var component = renderComponent();
      sinon.stub(component, "setState");
      sinon.stub(component, "validatePattern").returns(true);

      var expectedPattern = CustomValidation.PATTERNS["email"];

      component.onPredefinedPatternKeyOptionSelect("email");
      assert.isTrue(component.validatePattern.calledWith(expectedPattern));
      assert.isTrue(component.setState.calledWith({
        predefinedPatternKey: "email",
        pattern: expectedPattern,
        patternValid: true
      }));
    });

    it("should update state when the tooltip message input changes", function () {
      var component = renderComponent();
      sinon.stub(component, "setState");
      sinon.stub(component, "validateTooltipMessage").returns(false);

      component.onTooltipMessageInputChange("spam");
      assert.isTrue(component.validateTooltipMessage.calledWith("spam"));
      assert.isTrue(component.setState.calledWith({
        tooltipMessage: "spam",
        tooltipMessageValid: false
      }));
    });

    it("should update state when the valid example input changes", function () {
      var component = renderComponent();
      sinon.stub(component, "setState");
      sinon.stub(component, "validateValidExample").returns(false);

      component.onValidExampleInputChange("spam");
      assert.isTrue(component.validateValidExample.calledWith("spam"));
      assert.isTrue(component.setState.calledWith({
        validExample: "spam",
        validExampleValid: false
      }));
    });

    it("should not render the editor if model isn't provided", function () {
      var component = renderComponent({model: undefined});

      var $editor = $('.custom-validation-editor');
      assert.lengthOf($editor, 0);
    });

    it("should render the editor", function () {
      var component = renderComponent();

      var $editor = $('.custom-validation-editor');
      assert.lengthOf($editor, 1);
    });

    it("should render the pattern select", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.patternSelect);
    });

    it("should render the pattern input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.patternInput);
      assert.equal(
        component.refs.patternInput.props.value, component.state.pattern
      );
      assert.equal(
        component.refs.patternInput.props.valid, component.state.patternValid
      );
      assert.equal(
        component.refs.patternInput.props.onChange,
        component.onPatternInputChange
      );
    });

    it("should render the valid example input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.validExampleInput);
      assert.equal(
        component.refs.validExampleInput.props.value,
        component.state.validExample
      );
      assert.equal(
        component.refs.validExampleInput.props.valid,
        component.state.validExampleValid
      );
      assert.equal(
        component.refs.validExampleInput.props.onChange,
        component.onValidExampleInputChange
      );
    });

    it("should render the remaining characters counter for tooltip message", function () {
      var component = renderComponent();
      component.setState({tooltipMessage: "spam"});

      assert.isDefined(component.refs.remainingCharactersCounter);
      assert.equal(
        component.refs.remainingCharactersCounter.props.maxLength, 140
      );
      assert.equal(
        component.refs.remainingCharactersCounter.props.value, component.state.tooltipMessage
      );
    });

    it("should render the tooltip message input", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.tooltipMessageInput);
      assert.equal(
        component.refs.tooltipMessageInput.props.value,
        component.state.tooltipMessage
      );
      assert.equal(
        component.refs.tooltipMessageInput.props.valid,
        component.state.tooltipMessageValid
      );
      assert.equal(
        component.refs.tooltipMessageInput.props.onChange,
        component.onTooltipMessageInputChange
      );
    });
  });
});
