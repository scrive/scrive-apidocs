var React = require("react");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var Field = require("../../../js/fields.js").Field;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var RadioButton = require("../../../scripts/icons/radiobutton");
var RadioButtonPlacementView = require(
  "../../../scripts/authorview/fileview/radiobuttonplacementview"
);

describe("/authorview/fileview/radiobuttonplacementview", function () {
  var container = null;
  var document_ = null;
  var placement = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        model: placement,
        pageHeight: 100,
        pageWidth: 100
      },
      props || {}
    );

    var component = React.render(
      React.createElement(RadioButtonPlacementView, actualProps),
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

      var field = new Field({
        signatory: document_.signatoriesWhoSign()[0],
        name: "Radio Group",
        radio_button_values: [
          "Radio Button 1", "Radio Button 2", "Radio Button 3"
        ],
        placements: [
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.1,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.2,
            yrel: 0.2
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.3,
            yrel: 0.3
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

  it("should compute the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.height, 10);
    assert.equal(computedStyle.left, 10);
    assert.equal(computedStyle.top, 10);
    assert.equal(computedStyle.width, 10);
  });

  it("should assign the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    var componentNode = React.findDOMNode(component);

    assert.equal(componentNode.style["height"], computedStyle.height + "px");
    assert.equal(componentNode.style["left"], computedStyle.left + "px");
    assert.equal(componentNode.style["top"], computedStyle.top + "px");
    assert.equal(componentNode.style["width"], computedStyle.width + "px");
  });

  it("should render the radio button", function () {
    sinon.stub(placement.field(), "isRadioButtonSelected").returns(false);

    var component = renderComponent();

    var radioButton = TestUtils.findRenderedComponentWithType(
      component, RadioButton
    );

    assert.isFalse(radioButton.props.active);
    assert.equal(radioButton.props.pageWidth, component.props.pageWidth);
    assert.isFalse(radioButton.props.selected);
    assert.equal(radioButton.props.wrel, placement.wrel());
  });

  it("should render the selected radio button", function () {
    sinon.stub(placement.field(), "isRadioButtonSelected").returns(true);

    var component = renderComponent();

    var radioButton = TestUtils.findRenderedComponentWithType(
      component, RadioButton
    );

    assert.isTrue(placement.field().isRadioButtonSelected.calledWith(
      placement
    ));
    assert.isTrue(radioButton.props.selected);
  });
});
