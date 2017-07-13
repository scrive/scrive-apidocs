var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var backend = require("../../backend");
var util = require("../../util");

var Button = require("../../../scripts/common/button");
var Done = require("../../../scripts/designview/typesetters/done.jsx");
var Field = require("../../../js/fields.js").Field;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var FilePage = require("../../../js/files.js").FilePage;
var More = require("../../../scripts/designview/typesetters/more.jsx");
var RadioButtonSizeSelector = require(
  "../../../scripts/designview/typesetters/radiobuttonsizeselector.jsx"
);
var RadioGroupTypesetterView = require(
  "../../../scripts/designview/typesetters/radiogrouptypesetterview.jsx"
);
var SignatorySelector = require(
  "../../../scripts/designview/typesetters/signatoryselector.jsx"
);

describe("designview/typesetters/radiogrouptypesetterview", function () {
  var container = null;
  var document_ = null;
  var field = null;
  var placement = null;

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      field = new Field({
        signatory: document_.signatoriesWhoSign()[0],
        name: "Radio Group",
        type: "radiogroup",
        values: [
          "Radio Button 1", "Radio Button 2", "Radio Button 3"
        ],
        selected_value: null,
        placements: [
          {
            page: 0,
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.1,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.2,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.3,
            yrel: 0.1
          }
        ]
      });

      placement = field.placements()[0];

      done();
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  describe("RadioGroupTypesetterView", function () {
    var typesetterElement = null;

    var renderComponent = function (props) {
      container = document.createElement("div");
      typesetterElement = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          model: placement,
          element: typesetterElement,
          hideFunc: sinon.stub()
        },
        props || {}
      );

      var component = React.render(
        React.createElement(
          RadioGroupTypesetterView.RadioGroupTypesetterView, actualProps
        ),
        container
      );

      return component;
    };

    it("should not close if radio button names aren't unique", function () {
      sinon.stub(placement.field(), "radioButtonValues").returns([
        "spam", "eggs", "spam"
      ]);

      var component = renderComponent();
      sinon.stub(component, "done");

      component.onDone();
      assert.isFalse(component.done.called);
    });

    it("should not close if radio button names are unique", function () {
      var component = renderComponent();
      sinon.stub(component, "done");

      component.onDone();
      assert.isTrue(component.done.called);
    });

    it("should should remove the field when remove button is clicked", function () {
      sinon.stub(placement.field(), "remove");

      var component = renderComponent();

      TestUtils.Simulate.click(component.refs.removeButton.getDOMNode());
      assert.isTrue(placement.field().remove.called);
    });

    it("should render the signatory selector", function () {
      var component = renderComponent();

      var signatorySelector = TestUtils.findRenderedComponentWithType(
        component, SignatorySelector
      );

      assert.equal(
        signatorySelector.props.field, component.props.model.field()
      );
    });

    it("should render the radio button size selector", function () {
      var component = renderComponent();

      var sizeSelector = TestUtils.findRenderedComponentWithType(
        component, RadioButtonSizeSelector
      );

      assert.equal(sizeSelector.props.model, component.props.model);
    });

    it("should render the more section", function () {
      var component = renderComponent();

      var more = TestUtils.findRenderedComponentWithType(
        component, More
      );

      assert.isTrue(underscore.isString(more.props.text));
    });

    it("should render an editor for every radio button", function () {
      var component = renderComponent();

      var more = TestUtils.findRenderedComponentWithType(
        component, More
      );

      assert.equal(more.props.children.length, 3);

      var editors = more.props.children;

      assert.isTrue(underscore.all(field.placements(), function (placement, index) {
        var editor = editors[index];

        return (
          editor.props.index == index &&
          editor.props.placement == placement
        );
      }));
    });

    it("should render the done button", function () {
      var component = renderComponent();

      var doneButton = TestUtils.findRenderedComponentWithType(
        component, Done
      );

      assert.equal(doneButton.props.field, component.props.model.field());
      assert.equal(doneButton.props.onDone, component.onDone);
    });

    it("should render the remove button", function () {
      var component = renderComponent();

      assert.equal(
        component.refs.removeButton.props.onClick,
        component.onRemoveRadioGroupButtonClick
      );
    });
  });

  describe("RadioButtonEditorView", function () {
    var renderComponent = function (props) {
      container = document.createElement("div");

      var actualProps = underscore.extendOwn(
        {
          index: 0,
          placement: placement
        },
        props || {}
      );

      var component = React.render(
        React.createElement(
          RadioGroupTypesetterView.RadioButtonEditorView, actualProps
        ),
        container
      );

      return component;
    };

    it("should initialize state", function () {
      var component = renderComponent();
      assert.isTrue(component.state.isValueValid);
    });

    it("should get the placement's value", function () {
      var component = renderComponent();
      assert.equal(component.getPlacementValue(), "Radio Button 1");
    });

    it("should invalidate an empty value", function () {
      var component = renderComponent();
      assert.isFalse(component.isValueValid(""));
    });

    it("should invalidate a non-unique value", function () {
      var component = renderComponent();
      assert.isFalse(component.isValueValid("Radio Button 2"));
    });

    it("should validate a value", function () {
      var component = renderComponent();
      assert.isTrue(component.isValueValid("spam"));
    });

    it("should remove highlight from the placement when value field loses focus", function () {
      sinon.stub(placement, "setHighlighted");

      var component = renderComponent();

      component.onValueInputBlur();
      assert.isTrue(placement.setHighlighted.calledWith(false));
    });

    it("should handle value field change", function () {
      sinon.stub(placement.field(), "setRadioButtonValue");

      var component = renderComponent();
      sinon.stub(component, "isValueValid").returns(false);

      component.onValueInputChange("spam");
      assert.isTrue(component.isValueValid.calledWith("spam"));
      assert.isFalse(component.state.isValueValid);
      assert.isTrue(placement.field().setRadioButtonValue.calledWith(
        0, "spam"
      ));
    });

    it("should highlight the placement when value field gains focus", function () {
      sinon.stub(placement, "setHighlighted");

      var component = renderComponent();

      component.onValueInputFocus();
      assert.isTrue(placement.setHighlighted.calledWith(true));
    });

    it("should highlight the placement when mouse enters the value field", function () {
      sinon.stub(placement, "setHighlighted");

      var component = renderComponent();

      component.onValueInputMouseEnter();
      assert.isTrue(placement.setHighlighted.calledWith(true));
    });

    it("should remove highlight from the placement when mouse leaves the value field", function () {
      sinon.stub(placement, "setHighlighted");

      var component = renderComponent();
      sinon.stub(component.refs.inputValue, "hasFocus").returns(false);

      component.onValueInputMouseLeave();
      assert.isTrue(placement.setHighlighted.calledWith(false));
    });

    it("should not remove highlight from the placement when mouse leaves the focused value field", function () {
      sinon.stub(placement, "setHighlighted");

      var component = renderComponent();
      sinon.stub(component.refs.inputValue, "hasFocus").returns(true);

      component.onValueInputMouseLeave();
      assert.isFalse(placement.setHighlighted.called);
    });

    it("should remove the placement then a remove button is clicked", function () {
      sinon.stub(placement.field(), "removePlacement");

      var component = renderComponent();

      var removeButton = TestUtils.findRenderedDOMComponentWithClass(
        component, "remove-radiobutton"
      );

      TestUtils.Simulate.click(removeButton);
      assert.isTrue(placement.field().removePlacement.calledWith(
        component.props.placement
      ));
    });

    it("should render the value field", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.inputValue);

      var field = component.refs.inputValue;
      assert.equal(field.props.className, "");
      assert.equal(field.props.value, component.getPlacementValue());
      assert.equal(field.props.onBlur, component.onValueInputBlur);
      assert.equal(field.props.onChange, component.onValueInputChange);
      assert.equal(field.props.onFocus, component.onValueInputFocus);
      assert.equal(field.props.onMouseEnter, component.onValueInputMouseEnter);
      assert.equal(field.props.onMouseLeave, component.onValueInputMouseLeave);
    });

    it("should render the invalid value field", function () {
      var component = renderComponent();
      component.setState({isValueValid: false});

      var field = component.refs.inputValue;
      assert.equal(field.props.className, "redborder");
    });

    it("should render the remove button", function () {
      var component = renderComponent();

      var removeButton = TestUtils.findRenderedDOMComponentWithClass(
        component, "remove-radiobutton"
      );

      assert.equal(removeButton.props.onClick, component.onRemoveButtonClick);
    });

    it("should not render the remove button if the field has 2 or less placements", function () {
      field.removePlacement(field.placements()[2]);

      var component = renderComponent();

      var removeButtons = TestUtils.scryRenderedDOMComponentsWithClass(
        component, "remove-radiobutton"
      );

      assert.equal(removeButtons.length, 0);
    });
  });
});
