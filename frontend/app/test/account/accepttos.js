var jQuery = require("jquery");
var React = require("react");
var underscore = require("underscore");

var util = require("../util");

var TestUtils = React.addons.TestUtils;

var AcceptTOSView = require("../../scripts/account/accepttos");
var FlashMessages = require("../../js/flashmessages.js");
var Submits = require("../../js/submits.js");

describe("account/accepttos", function () {
  var container = null;
  var fakeSubmit = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn({}, props || {});
    var component = React.render(
      React.createElement(AcceptTOSView, actualProps), container
    );

    return component;
  };

  beforeEach(function () {
    fakeSubmit = new Submits.Submit();
    sinon.stub(fakeSubmit, "send");

    sinon.stub(FlashMessages, "FlashMessageAfterReload");
    sinon.stub(Submits, "Submit").returns(fakeSubmit);
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    FlashMessages.FlashMessageAfterReload.restore();
    Submits.Submit.restore();

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.accepted);
    assert.isFalse(component.state.hasError);
  });

  it("should configure the terms link when it mounts", function () {
    var component = renderComponent();

    var $link = jQuery("a", component.refs.label.getDOMNode());
    assert.isTrue($link.hasClass("clickable"));
    assert.equal($link.attr("target"), "_blank");
    assert.equal($link.attr("href"), "/terms-of-service/");
  });

  describe("onAcceptButtonClick", function () {
    it("should set error state to true if the checkbox isn't checked", function () {
      var component = renderComponent();

      component.onAcceptButtonClick();
      assert.isTrue(component.state.hasError);
    });

    it("should not send the request if the checkbox isn't checked", function () {
      var component = renderComponent();

      component.onAcceptButtonClick();
      assert.isFalse(Submits.Submit.called);
    });

    it("should set error state to false if the checkbox is checked", function () {
      var component = renderComponent();
      component.setState({accepted: true, hasError: true});

      component.onAcceptButtonClick();
      assert.isFalse(component.state.hasError);
    });

    it("should configure and send accept TOS request", function () {
      var component = renderComponent();
      component.setState({accepted: true});

      component.onAcceptButtonClick();
      assert.isTrue(Submits.Submit.calledWithNew());
      assert.isTrue(Submits.Submit.calledWith({
        url: "/accepttos",
        method: "POST",
        ajax: true,
        tos: "on",
        ajaxsuccess: component.onSubmitSuccess
      }));
      assert.isTrue(fakeSubmit.send.called);
    });
  });

  it("should toggle accepted state when the checkbox is clicked", function () {
    var component = renderComponent();

    component.onCheckboxClick();
    assert.isTrue(component.state.accepted);

    component.onCheckboxClick();
    assert.isFalse(component.state.accepted);
  });

  describe("render", function () {
    it("should render the checkbox wrapper as valid if error state is false", function () {
      var component = renderComponent();

      assert.isFalse(
        component.refs.wrapper.getDOMNode().classList.contains("has-error")
      );
    });

    it("should render the checkbox wrapper as invalid if error state is true", function () {
      var component = renderComponent();
      component.setState({hasError: true});

      assert.isTrue(
        component.refs.wrapper.getDOMNode().classList.contains("has-error")
      );
    });

    it("should configure and render the checkbox", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.checkbox);
      assert.isTrue(
        component.refs.checkbox.getDOMNode().classList.contains("checkbox")
      );
    });

    it("should render the checkbox as unchecked if accepted state is false", function () {
      var component = renderComponent();
      assert.isFalse(
        component.refs.checkbox.getDOMNode().classList.contains("checked")
      );
    });

    it("should render the checkbox as checked if accepted state is true", function () {
      var component = renderComponent();
      component.setState({accepted: true});

      assert.isTrue(
        component.refs.checkbox.getDOMNode().classList.contains("checked")
      );
    });

    it("should configure and render the checkbox label", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.label);
      assert.notEqual(component.refs.label.props.secureText, "");
      assert.equal(
        component.refs.label.props.onClicks["label"], component.onCheckboxClick
      );
    });

    it("should configure and render the accept button", function () {
      var component = renderComponent();
      assert.isDefined(component.refs.acceptButton);
      assert.equal(component.refs.acceptButton.props.size, "small");
      assert.equal(
        component.refs.acceptButton.props.text, localization.accept
      );
      assert.equal(component.refs.acceptButton.props.type, "action");
      assert.equal(
        component.refs.acceptButton.props.onClick,
        component.onAcceptButtonClick
      );
    });
  });
});
