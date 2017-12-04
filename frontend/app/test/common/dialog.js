var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var util = require("../util");

var TestUtils = React.addons.TestUtils;

var Dialog = require("../../scripts/common/dialog");

describe("common/dialog", function () {
  var container = null;

  var renderComponent = function (props, children, componentClass) {
    container = document.createElement("div");

    var actualProps = _.extendOwn({}, props || {});

    if (!children) {
      children = React.createElement(
        "div", {className: "content"}, "HERE CONTENT BE"
      );
    }

    var component = React.render(
      React.createElement(componentClass, actualProps, children), container
    );

    return component;
  };

  beforeEach(function () {
    sinon.stub(window, "addEventListener");
    sinon.stub(window, "removeEventListener");

    sinon.stub(window, "setTimeout").returns("spam");
    sinon.stub(window, "clearTimeout");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    window.addEventListener.restore();
    window.removeEventListener.restore();

    window.setTimeout.restore();
    window.clearTimeout.restore();

    util.cleanTimeoutsAndBody();
  });

  describe("Dialog", function () {
    it("should register window resize handler when it mounts", function () {
      var component = renderComponent(
        {active: false, onHide: sinon.stub()},
        null,
        Dialog.Dialog
      );

      assert.isTrue(window.addEventListener.calledWith(
        "resize", component.onWindowResize, false
      ));
    });

    describe("componentWillUnmount", function () {
      it("should deregister window resize handler", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );

        React.unmountComponentAtNode(container);
        container = null;

        assert.isTrue(window.removeEventListener.calledWith(
          "resize", component.onWindowResize, false
        ));
      });

      it("should clear window resize timeout", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        sinon.stub(component, "clearWindowResizeTimeout");

        React.unmountComponentAtNode(container);
        container = null;

        assert.isTrue(component.clearWindowResizeTimeout.called);
      });
    });

    describe("componentWillReceiveProps", function () {
      it("should not fix position when it becomes hidden", function () {
        var component = renderComponent(
          {active: true, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        sinon.stub(component, "fixPosition");

        component.componentWillReceiveProps({active: false});
        assert.isFalse(component.fixPosition.called);
      });

      it("should fix position when it becomes visible", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        sinon.stub(component, "fixPosition");

        component.componentWillReceiveProps({active: true});
        assert.isTrue(component.fixPosition.called);
      });
    });

    describe("clearWindowResizeTimeout", function () {
      it("should not clear the timeout if it isn't set", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );

        component.clearWindowResizeTimeout();
        assert.isFalse(window.clearTimeout.called);
      });

      it("should clear the timeout if it's set", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        component._onWindowResizeTimeout = "spam"

        component.clearWindowResizeTimeout();
        assert.isTrue(window.clearTimeout.calledWith("spam"));
      });
    });

    it("should fix content position", function () {
      var component = renderComponent(
        {active: false, onHide: sinon.stub()},
        null,
        Dialog.Dialog
      );

      component.fixPosition();

      var $node = $(".modal-container", component.refs.modal.getDOMNode());
      assert.isDefined($node.css("left"));
      assert.isDefined($node.css("marginLeft"));
      assert.isDefined($node.css("marginTop"));
      assert.isDefined($node.css("top"));
    });

    describe("onWindowResize", function () {
      it("should not set handler timeout if it's already set", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        component._onWindowResizeTimeout = "spam"

        component.onWindowResize();
        assert.isFalse(window.setTimeout.called);
      });

      it("should set handler timeout if isn't set", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );

        component.onWindowResize();
        assert.isTrue(window.setTimeout.calledWith(
          component.onWindowResizeTimeout, 66
        ));
        assert.equal(component._onWindowResizeTimeout, "spam");
      });
    });

    describe("onWindowResizeTimeout", function () {
      it("should clear window resize timeout", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        sinon.stub(component, "clearWindowResizeTimeout");

        component.onWindowResizeTimeout();
        assert.isTrue(component.clearWindowResizeTimeout.called);
      });

      it("should fix position", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );
        sinon.stub(component, "fixPosition");

        component.onWindowResizeTimeout();
        assert.isTrue(component.fixPosition.called);
      });
    });

    describe("render", function () {
      it("should configure and render the modal container", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          null,
          Dialog.Dialog
        );

        assert.equal(
          component.refs.modal.props.active, component.props.active
        );
        assert.equal(
          component.refs.modal.props.className, "screenblockingdialog"
        );
        assert.equal(component.refs.modal.props.width, 650);
        assert.equal(
          component.refs.modal.props.onHide, component.props.onHide
        );
      });

      it("should render children in the modal", function () {
        var component = renderComponent(
          {active: false, onHide: sinon.stub()},
          React.createElement("div", {className: "spam"}, "SPAM"),
          Dialog.Dialog
        );

        var content = TestUtils.scryRenderedDOMComponentsWithClass(
          component.refs.modalContent, "spam"
        );

        assert.lengthOf(content, 1);
        assert.equal(content[0].getDOMNode().innerText, "SPAM");
      });
    });
  });

  describe("Header", function () {
    it("should render children", function () {
      var component = renderComponent(
        {},
        React.createElement("div", {className: "spam"}, "SPAM"),
        Dialog.Header
      );

      var content = TestUtils.scryRenderedDOMComponentsWithClass(
        component, "spam"
      );

      assert.lengthOf(content, 1);
      assert.equal(content[0].getDOMNode().innerText, "SPAM");
    });
  });

  describe("SubHeader", function () {
    it("should render children", function () {
      var component = renderComponent(
        {},
        React.createElement("div", {className: "spam"}, "SPAM"),
        Dialog.SubHeader
      );

      var content = TestUtils.scryRenderedDOMComponentsWithClass(
        component, "spam"
      );

      assert.lengthOf(content, 1);
      assert.equal(content[0].getDOMNode().innerText, "SPAM");
    });
  });

  describe("Content", function () {
    it("should render children", function () {
      var component = renderComponent(
        {},
        React.createElement("div", {className: "spam"}, "SPAM"),
        Dialog.Content
      );

      var content = TestUtils.scryRenderedDOMComponentsWithClass(
        component, "spam"
      );

      assert.lengthOf(content, 1);
      assert.equal(content[0].getDOMNode().innerText, "SPAM");
    });
  });
});
