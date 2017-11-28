var React = require("react");
var underscore = require("underscore");

var util = require("../../util");

var TestUtils = React.addons.TestUtils;

var ResultModal = require(
  "../../../scripts/pages/verification/resultmodal.jsx"
);

describe("pages/verification/resultmodal", function () {
  var container = null;

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        active: false,
        result: null,
        time: null,
        onClose: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(ResultModal, actualProps), container
    );

    return component;
  };

  it("should configure and render the container", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.container);
    assert.isFalse(component.refs.container.props.active);
    assert.equal(
      component.refs.container.props.onClose, component.props.onClose
    );
  });

  it("should render as active if it is active", function () {
    var component = renderComponent({active: true});
    assert.isTrue(component.refs.container.props.active);
  });

  it("should configure and render the header", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.header);
    assert.isTrue(component.refs.header.props.showClose);
    assert.equal(component.refs.header.props.onClose, component.props.onClose);
  });

  it("should render the header title for successful verification", function () {
    var component = renderComponent({result: "success"});
    assert.equal(
      component.refs.header.props.title, localization.verification.success
    );
  });

  it("should render the header title for verification error", function () {
    var component = renderComponent({result: "error"});
    assert.equal(
      component.refs.header.props.title, localization.verification.error
    );
  });

  it("should render the header title for verification failure", function () {
    var component = renderComponent({result: "failed"});
    assert.equal(
      component.refs.header.props.title, localization.verification.failed
    );
  });

  it("should render the icon for successful verification", function () {
    var component = renderComponent({result: "success"});

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "icon"
    );
    assert.isTrue(
      icon.getDOMNode().classList.contains("verificationSuccessIcon")
    );
  });

  it("should render the icon for verification error", function () {
    var component = renderComponent({result: "error"});

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "icon"
    );
    assert.isTrue(
      icon.getDOMNode().classList.contains("verificationErrorIcon")
    );
  });

  it("should render the icon for verification failure", function () {
    var component = renderComponent({result: "failed"});

    var icon = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "icon"
    );
    assert.isTrue(
      icon.getDOMNode().classList.contains("verificationFailedIcon")
    );
  });

  it("should render the message for successful verification", function () {
    var time = (new Date()).toISOString();
    var component = renderComponent({result: "success", time: time});

    var message = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "message"
    );
    assert.equal(
      message.getDOMNode().innerText,
      localization.verification.time + " " + time
    );
  });

  it("should render the message for verification error", function () {
    var component = renderComponent({result: "error"});

    var message = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "message"
    );
    assert.equal(
      message.getDOMNode().innerText.trim(),
      localization.verification.errorMessage
    );
  });

  it("should render the message for verification failure", function () {
    var component = renderComponent({result: "failed"});

    var message = TestUtils.findRenderedDOMComponentWithClass(
      component.refs.content, "message"
    );
    assert.equal(
      message.getDOMNode().innerText.trim(),
      localization.verification.failedMessage
    );
  });

  it("should configure and render the cancel button", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.cancelButton);
    assert.equal(
      component.refs.cancelButton.props.onClick, component.props.onClose
    );
  });

  it("should configure and render the accept button", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.acceptButton);
    assert.equal(
      component.refs.acceptButton.props.onClick, component.props.onClose
    );
  });
});
