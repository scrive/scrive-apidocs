var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var ButtonBarView = require(
  "../../../../scripts/admin/useradmin/userdetails/buttonbar"
);

describe("admin/useradmin/userdetails/buttonbar", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement('div');
    componentClass = componentClass || ButtonBarView;

    var actualProps = underscore.extendOwn(
      {
        companyid: "1",
        onDelete: sinon.stub(),
        onResendInvitation: sinon.stub(),
        onMove: sinon.stub(),
        onSave: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(componentClass, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.showDeleteModal);
    assert.isFalse(component.state.showMoveModal);
  });

  it("should hide move modal if company ID changes", function () {
    var wrapperComponent = React.createClass({
      getInitialState: function () {
        return {companyid: "1"};
      },
      render: function () {
        var buttonBar = React.createElement(ButtonBarView, {
          ref: "buttonBar",
          companyid: this.state.companyid,
          onDelete: sinon.stub(),
          onResendInvitation: sinon.stub(),
          onMove: sinon.stub(),
          onSave: sinon.stub()
        });

        return buttonBar;
      }
    });

    var component = renderComponent({}, wrapperComponent);
    component.refs.buttonBar.setState({showMoveModal: true});

    component.setState({companyid: "2"});
    assert.isFalse(component.refs.buttonBar.state.showMoveModal);
  });

  it("should show delete modal", function () {
    var component = renderComponent();

    var deleteButton = $(".user-details-button-delete", component.getDOMNode());
    TestUtils.Simulate.click(deleteButton[0]);

    var deleteModal = $(".modal.user-details-modal-delete");
    assert.isTrue(deleteModal.hasClass("active"));
  });

  it("should hide delete modal when it's closed", function () {
    var component = renderComponent();
    component.setState({showDeleteModal: true});

    var deleteModal = $(".modal.user-details-modal-delete");

    var deleteModalCloseButton = $("a.modal-close", deleteModal);
    TestUtils.Simulate.click(deleteModalCloseButton[0]);

    assert.isFalse(deleteModal.hasClass("false"));
    assert.isFalse(component.props.onDelete.called);
  });

  it("should hide delete modal when it's cancelled", function () {
    var component = renderComponent();
    component.setState({showDeleteModal: true});

    var deleteModal = $(".modal.user-details-modal-delete");

    var deleteModalCloseButton = $("label.close", deleteModal);
    TestUtils.Simulate.click(deleteModalCloseButton[0]);

    assert.isFalse(deleteModal.hasClass("false"));
    assert.isFalse(component.props.onDelete.called);
  });

  it("should call the onDelete callback when delete modal is accepted", function () {
    var component = renderComponent();
    component.setState({showDeleteModal: true});

    var deleteModal = $(".modal.user-details-modal-delete");

    var deleteModalCloseButton = $("a.cancel", deleteModal);
    TestUtils.Simulate.click(deleteModalCloseButton[0]);

    assert.isTrue(component.props.onDelete.called);
  });

  it("should call the onResendInvitation callback", function () {
    var component = renderComponent();

    var resendInvitationButton = $(
      ".user-details-button-resend-invitation", component.getDOMNode()
    );
    TestUtils.Simulate.click(resendInvitationButton[0]);

    assert.isTrue(component.props.onResendInvitation.called);
  });

  it("should show move modal", function () {
    var component = renderComponent();

    var moveButton = $(".user-details-button-move", component.getDOMNode());
    TestUtils.Simulate.click(moveButton[0]);

    var moveModal = $(".modal.user-details-modal-move");
    assert.isTrue(moveModal.hasClass("active"));
  });

  it("should hide move modal when it's closed", function () {
    var component = renderComponent();
    component.setState({showMoveModal: true});

    var moveModal = $(".modal.user-details-modal-move");

    var moveModalCloseButton = $("a.modal-close", moveModal);
    TestUtils.Simulate.click(moveModalCloseButton[0]);

    assert.isFalse(moveModal.hasClass("false"));
    assert.isFalse(component.props.onMove.called);
  });

  it("should hide move modal when it's cancelled", function () {
    var component = renderComponent();
    component.setState({showMoveModal: true});

    var moveModal = $(".modal.user-details-modal-move");

    var moveModalCloseButton = $("label.close", moveModal);
    TestUtils.Simulate.click(moveModalCloseButton[0]);

    assert.isFalse(moveModal.hasClass("false"));
    assert.isFalse(component.props.onMove.called);
  });

  it("should call the onMove callback when move modal is accepted", function () {
    var component = renderComponent();
    component.setState({showMoveModal: true});

    var moveModal = $(".modal.user-details-modal-move");

    var moveModalCloseButton = $("a.action", moveModal);
    TestUtils.Simulate.click(moveModalCloseButton[0]);

    assert.isTrue(component.props.onMove.calledWith("1"));
  });

  it("should call the onSave callback", function () {
    var component = renderComponent();

    var saveButton = $(".user-details-button-save", component.getDOMNode());
    TestUtils.Simulate.click(saveButton[0]);

    assert.isTrue(component.props.onSave.called);
  });
});
