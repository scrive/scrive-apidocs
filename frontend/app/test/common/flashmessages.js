var React = require("react");
var underscore = require("underscore");

var util = require("../util");

var TestUtils = React.addons.TestUtils;

var FlashMessageView = require("../../scripts/common/flashmessages");

describe("common/flashmessages", function () {
  var container = null;
  var fakeSubmit = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var defaultProps = {
      type: "success",
      content: "SPAM"
    };

    var actualProps = underscore.extendOwn({}, defaultProps, props || {});
    var component = React.render(
      React.createElement(FlashMessageView, actualProps), container
    );

    return component;
  };

  beforeEach(function () {
    sinon.stub(window, "setTimeout");
    sinon.stub(window, "clearTimeout");
  });

  afterEach(function () {
    window.setTimeout.restore();
    window.clearTimeout.restore();
  });

  describe("componentDidMount", function () {
    it("should fill body with content when it mounts", function () {
      var component = renderComponent();
      assert.equal(
        component.refs.body.getDOMNode().innerText, component.props.content
      );
    });

    it("should set show and hide timeouts", function () {
      var component = renderComponent();
      assert.isTrue(window.setTimeout.calledWith(component.show, 100));
      assert.isTrue(window.setTimeout.calledWith(component.hide, 10000));
    });
  });

  it("should hide", function () {
    var component = renderComponent();
    component._hideTimeout = "spam";
    component.setState({visible: true});

    component.hide();
    assert.isTrue(window.clearTimeout.calledWith("spam"));
    assert.isFalse(component.state.visible);
  });

  it("should show", function () {
    var component = renderComponent();
    component._showTimeout = "spam";
    component.setState({visible: true});

    component.show();
    assert.isTrue(window.clearTimeout.calledWith("spam"));
    assert.isTrue(component.state.visible);
  });

  describe("render", function () {
    it("should render as success message if type is success", function () {
      var component = renderComponent({type: "success"});
      assert.isTrue(component.getDOMNode().classList.contains("success"));
    });

    it("should render as error message if type is error", function () {
      var component = renderComponent({type: "error"});
      assert.isTrue(component.getDOMNode().classList.contains("error"));
    });

    it("should render the body container", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.body);
    });

    it("should configure and render the body close button", function () {
      var component = renderComponent();

      var button = TestUtils.findRenderedDOMComponentWithClass(
        component, "flash-close"
      );
      assert.equal(button.props.onClick, component.hide);
    });
  });
});
