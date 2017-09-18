var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;
var util = require("../../util");

var PlacementTooltipView = require(
  "../../../scripts/signview/fileview/placementtooltipview"
);
var TooltipTop = require("../../../scripts/icons/tooltip-top.svg");

describe("signview/fileview/placementtooltipview", function () {
  var container = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        message: "Spam",
        scale: 1,
        visible: false
      },
      props || {}
    );

    var component = React.render(
      React.createElement(
        PlacementTooltipView, actualProps
      ),
      container
    );

    return component;
  };

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.left, 0);
    assert.equal(component.state.top, 0);
    assert.equal(component.state.tipLeft, 0);
  });

  it("should set layout", function () {
    var component = renderComponent();

    component.setLayout(1, 2, undefined);
    assert.equal(component.state.left, 1);
    assert.equal(component.state.top, 2);
    assert.equal(component.state.tipLeft, 0);
  });

  it("should cancel the mouse down event", function () {
    var event = new util.FakeMouseEvent("mousedown");

    var component = renderComponent();

    component.onMouseDown(event)
    assert.isTrue(event.stopPropagation.called);
    assert.isTrue(event.preventDefault.called);
  });

  it("should render as invisible if it isn't visible", function () {
    var component = renderComponent();
    assert.isFalse(component.getDOMNode().classList.contains("visible"));
  });

  it("should render as visible if it is visible", function () {
    var component = renderComponent({visible: true});
    assert.isTrue(component.getDOMNode().classList.contains("visible"));
  });

  it("should render the container", function () {
    var component = renderComponent();

    var container = TestUtils.findRenderedDOMComponentWithClass(
      component, "placement-tooltip"
    );
    assert.equal(container.props.onMouseDown, component.onMouseDown);
    assert.isDefined(container.props.style);
  });

  it("should render the message", function () {
    var component = renderComponent();

    var message = TestUtils.findRenderedDOMComponentWithTag(
      component, "p"
    );
    assert.equal(message.getDOMNode().innerText, component.props.message);
    assert.isDefined(message.props.style);
  });

  it("should render the top icon", function () {
    var component = renderComponent();

    var top = TestUtils.findRenderedComponentWithType(
      component, TooltipTop
    );
    assert.isDefined(top.props.style);
  });
});
