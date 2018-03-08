var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../../../../backend");
var util = require("../../../../util");

var TestUtils = React.addons.TestUtils;

var MergeModal = require(
  "../../../../../scripts/admin/companyadmin/companydetails/modals/merge"
);

describe("admin/companyadmin/companydetails/modals/merge", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || MergeModal;

    var actualProps = _.extendOwn(
      {
        companyid: "1",
        active: false,
        onAccept: sinon.stub(),
        onCancel: sinon.stub()
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

  it("should render inactive modal", function () {
    var component = renderComponent();

    var modal = $(".modal.company-details-modal-merge");
    assert.lengthOf(modal, 1);
    assert.isFalse(modal.hasClass("active"));
  });

  it("should activate modal", function () {
    var component = renderComponent({active: true});

    var modal = $(".modal.company-details-modal-merge");
    assert.isTrue(modal.hasClass("active"));
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.equal(component.state.companyId, component.props.companyId);
    assert.equal(component.state.companyNameOrError, "");
  });

  it("should not load company name if it's inactive", function () {
    var component = renderComponent();
    component.loadCompanyName = sinon.stub();

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("2");
    TestUtils.Simulate.change(input[0]);

    assert.isFalse(component.loadCompanyName.called);
  });

  it("should load company name if the ID changes", function () {
    var component = renderComponent({active: true});
    component.loadCompanyName = sinon.stub();

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("2");
    TestUtils.Simulate.change(input[0]);

    assert.isTrue(component.loadCompanyName.called);
  });

  it("should reset state when it becomes active", function () {
    var wrapperComponent = React.createClass({
      getInitialState: function () {
        return {modalVisible: false};
      },
      render: function () {
        var modal = React.createElement(MergeModal, {
          ref: "modal",
          companyId: "1",
          active: this.state.modalVisible,
          onAccept: sinon.stub(),
          onCancel: sinon.stub()
        });
        modal.loadCompanyName = sinon.stub();

        return modal;
      }
    });

    var component = renderComponent({}, wrapperComponent);
    component.refs.modal.setState({
      companyId: "2",
      companyNameOrError: "spam"
    });

    component.setState({modalVisible: true});
    assert.equal(component.refs.modal.state.companyId, "1");
    assert.equal(component.refs.modal.state.companyNameOrError, "");
  });

  it("should change state when company ID input changes", function () {
    var component = renderComponent({active: true});
    component.loadCompanyName = sinon.stub();

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("2");
    TestUtils.Simulate.change(input[0]);

    assert.equal(component.state.companyId, "2");
  });

  it("should display error if company ID is invalid", function () {
    var component = renderComponent({active: true});

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("invalid");
    TestUtils.Simulate.change(input[0]);

    var messageBox = $(
      ".modal.company-details-modal-merge .company-name"
    );

    assert.equal(messageBox.text(), "Company ID can only contain numbers");
  });

  it("should load and display company name", function () {
    var component = renderComponent({active: true});

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("2");
    TestUtils.Simulate.change(input[0]);

    var messageBox = $(
      ".modal.company-details-modal-merge .company-name"
    );

    assert.equal(messageBox.text(), "Company with name: Test Company");
  });

  it("should load error if company doesn't exist", function () {
    var component = renderComponent({active: true});

    var input = $(".modal.company-details-modal-merge input[type=text]");
    input.val("0");
    TestUtils.Simulate.change(input[0]);

    var messageBox = $(
      ".modal.company-details-modal-merge .company-name"
    );

    assert.equal(messageBox.text(), "No company matches the given ID");
  });

  it("should call the onCancel callback when modal is closed", function () {
    var component = renderComponent({active: true});

    var closeButton = $(".modal.company-details-modal-merge a .modal-close");

    TestUtils.Simulate.click(closeButton[0]);
    assert.isTrue(component.props.onCancel.called);
  });

  it("should call the onCancel callback when modal is cancelled", function () {
    var component = renderComponent({active: true});

    var cancelButton = $(".modal.company-details-modal-merge label.close");

    TestUtils.Simulate.click(cancelButton[0]);
    assert.isTrue(component.props.onCancel.called);
  });

  it("should call the onAccept callback when modal is accepted", function () {
    var component = renderComponent({active: true});

    var acceptButton = $(".modal.company-details-modal-merge a.action");

    TestUtils.Simulate.click(acceptButton[0]);
    assert.isTrue(
      component.props.onAccept.calledWith(component.state.companyId)
    );
  });
});
