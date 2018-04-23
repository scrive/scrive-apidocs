var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var TestUtils = React.addons.TestUtils;
var backend = require("../../backend");
var util = require("../../util");

var UserDeletionModal = require(
  "../../../scripts/account/settings/userdeletionmodal"
);

describe("account/settings/userdeletionmodal", function () {
  var container = null;
  var model = {
    email: function () {
      return "correct@email.tld";
    }
  };

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
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

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        active: false,
        model: model,
        onClose: sinon.stub(),
        onConfirmation: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(UserDeletionModal, actualProps), container
    );

    return component;
  };

  it("should disable the accept button when the text is wrong", function () {
    var component = renderComponent({ active: true });

    var button = $(".user-deletion-button");
    assert(button.hasClass("inactive"));
    component.onConfirmation();

    assert.isFalse(component.props.onConfirmation.called);
  });

  it("should call onConfirmation when the text is correct", function () {
    var component = renderComponent({ active: true });
    component.onTextChange({target: {value: model.email()}});

    var button = $(".user-deletion-button");
    assert.isFalse(button.hasClass("inactive"));
    component.onConfirmation();

    assert(component.props.onConfirmation.called);
  });

  it("should clear the entered text when closed and reopened", function () {
    var component = renderComponent({active: true});

    component.onTextChange({target: {value: "some text"}});
    assert.equal(component.state.textEntered, "some text");

    component.componentWillReceiveProps({active: false});
    component.props.active = false;
    component.componentWillReceiveProps({active: true});
    assert.equal(component.state.textEntered, "");
  });
});
