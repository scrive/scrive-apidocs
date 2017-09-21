var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var backend = require("../../backend");
var util = require("../../util");

var Button = require("../../../scripts/common/button.jsx");
var CustomValidation = require(
  "../../../js/customvalidations.js"
).CustomValidation;
var CustomValidationEditorModal = require(
  "../../../scripts/designview/typesetters/customvalidationeditormodal.jsx"
);
var Field = require("../../../js/fields.js").Field;
var ValidationSelector = require(
  "../../../scripts/designview/typesetters/validationselector.jsx"
);

describe("designview/typesetters/validationselector", function () {
  var container = null;
  var document_ = null;
  var field = null;
  var placement = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        model: placement
      },
      props || {}
    );

    var component = React.render(
      React.createElement(
        ValidationSelector, actualProps
      ),
      container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      field = new Field({
        signatory: document_.signatoriesWhoSign()[0],
        name: "Custom Validation",
        type: "text",
        value: "",
        placements: [
          {
            page: 0,
            hrel: 0.1,
            wrel: 0.1,
            xrel: 0.1,
            yrel: 0.1
          }
        ]
      });

      field.setCustomValidation(new CustomValidation({
        pattern: "^[0-9]+?$",
        tooltipMessage: "Spam",
        validExample: "123"
      }));
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

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.showModal);
  });

  it("should create new custom validation when the modal is opened and there's no custom validation", function () {
    sinon.stub(field, "hasCustomValidation").returns(false);
    sinon.stub(field, "setCustomValidation");

    var component = renderComponent();

    component.onEditCustomPatternButtonClick();
    assert.isTrue(field.setCustomValidation.calledWith(
      sinon.match.instanceOf(CustomValidation)
    ));
  });

  it("should not create new custom validation when the modal is opened and there custom validation", function () {
    sinon.stub(field, "setCustomValidation");

    var component = renderComponent();

    component.onEditCustomPatternButtonClick();
    assert.isFalse(field.setCustomValidation.called);
  });

  it("should show modal when the edit custom pattern button is clicked", function () {
    var component = renderComponent();

    component.onEditCustomPatternButtonClick();
    assert.isTrue(component.state.showModal);
  });

  it("should remove the custom validation when the remove button is clicked", function () {
    sinon.stub(field, "setCustomValidation");

    var component = renderComponent();

    component.onRemoveCustomPatternButtonClick();
    assert.isTrue(field.setCustomValidation.calledWith(null));
  });

  it("should hide modal when it's closed", function () {
    var component = renderComponent();
    component.setState({showModal: true});

    component.onCustomValidationEditorModalClose()
    assert.isFalse(component.state.showModal);
  });

  it("should identify a custom pattern by name", function () {
    sinon.stub(CustomValidation, "predefinedPatternKey").returns(undefined);

    var component = renderComponent();
    assert.equal(
      component.customValidationPatternName(),
      localization.designview.customValidation.custom
    );
    assert.isTrue(CustomValidation.predefinedPatternKey.calledWith(
      field.customValidation().pattern()
    ));

    CustomValidation.predefinedPatternKey.restore();
  });

  it("should identify a predefined pattern by name", function () {
    sinon.stub(CustomValidation, "predefinedPatternKey").returns("email");

    var component = renderComponent();
    assert.equal(
      component.customValidationPatternName(),
      localization.designview.customValidation.patternName.email
    );

    CustomValidation.predefinedPatternKey.restore();
  });

  it("should not render the pattern name if there's no custom validation", function () {
    sinon.stub(field, "hasCustomValidation").returns(false);

    var component = renderComponent();

    var elements = TestUtils.scryRenderedDOMComponentsWithClass(
      component, "pattern-name"
    );
    assert.lengthOf(elements, 0);
  });

  it("should render the pattern name if there's custom validation", function () {
    var component = renderComponent();

    var element = TestUtils.findRenderedDOMComponentWithClass(
      component, "pattern-name"
    );
    assert.notEqual($(element.getDOMNode()).text(), "");
  });

  it("should not render the add button if there's no custom validation", function () {
    sinon.stub(field, "hasCustomValidation").returns(false);

    var component = renderComponent();

    var button = TestUtils.findRenderedComponentWithType(component, Button);
    assert.equal(button.props.text, localization.designview.addAnchor);
    assert.equal(
      button.props.onClick, component.onEditCustomPatternButtonClick
    );
  });

  it("should render the edit button if there's custom validation", function () {
    var component = renderComponent();

    var button = TestUtils.findRenderedComponentWithType(component, Button);
    assert.equal(button.props.text, localization.designview.editField);
    assert.equal(
      button.props.onClick, component.onEditCustomPatternButtonClick
    );
  });

  it("should not render the remove button if there's no custom validation", function () {
    sinon.stub(field, "hasCustomValidation").returns(false);

    var component = renderComponent();
    assert.isUndefined(component.refs.removeCustonPatternButton);
  });

  it("should render the remove button if there's custom validation", function () {
    var component = renderComponent();

    var button = TestUtils.findRenderedComponentWithType(component, Button);
    assert.isDefined(component.refs.removeCustonPatternButton);
    assert.equal(
      component.refs.removeCustonPatternButton.props.onClick,
      component.onRemoveCustomPatternButtonClick
    );
  });

  it("should render the custom pattern editor modal", function () {
    var component = renderComponent();

    var modal = TestUtils.findRenderedComponentWithType(
      component, CustomValidationEditorModal.CustomValidationEditorModal
    );

    assert.equal(modal.props.active, component.state.showModal);
    assert.equal(modal.props.model, field.customValidation());
    assert.equal(
      modal.props.onClose, component.onCustomValidationEditorModalClose
    );
  });
});
