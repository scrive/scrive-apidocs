var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../../../backend");
var util = require("../../../util");

var TestUtils = React.addons.TestUtils;

var ButtonBarView = require(
  "../../../../scripts/admin/companyadmin/companydetails/buttonbar"
);

describe("admin/companyadmin/companydetails/buttonbar", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || ButtonBarView;

    var actualProps = _.extendOwn(
      {
        companyId: "1",
        onMerge: sinon.stub(),
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
    assert.isFalse(component.state.showMergeModal);
  });

  it("should show merge modal", function () {
    var component = renderComponent();

    var mergeButton = $(".company-details-button-merge", component.getDOMNode());
    TestUtils.Simulate.click(mergeButton[0]);

    var mergeModal = $(".modal.company-details-modal-merge");
    assert.isTrue(mergeModal.hasClass("active"));
  });

  it("should hide merge modal when it's closed", function () {
    var component = renderComponent();
    component.setState({showMergeModal: true});

    var mergeModal = $(".modal.company-details-modal-merge");

    var mergeModalCloseButton = $("a .modal-close", mergeModal);
    TestUtils.Simulate.click(mergeModalCloseButton[0]);

    assert.isFalse(mergeModal.hasClass("false"));
    assert.isFalse(component.props.onMerge.called);
  });

  it("should hide merge modal when it's cancelled", function () {
    var component = renderComponent();
    component.setState({showMergeModal: true});

    var mergeModal = $(".modal.company-details-modal-merge");

    var mergeModalCloseButton = $("label.close", mergeModal);
    TestUtils.Simulate.click(mergeModalCloseButton[0]);

    assert.isFalse(mergeModal.hasClass("false"));
    assert.isFalse(component.props.onMerge.called);
  });

  it("should call the onMerge callback when merge modal is accepted", function () {
    var component = renderComponent();
    component.setState({showMergeModal: true});

    var mergeModal = $(".modal.company-details-modal-merge");

    var mergeModalCloseButton = $("a.action", mergeModal);
    TestUtils.Simulate.click(mergeModalCloseButton[0]);

    assert.isTrue(component.props.onMerge.calledWith("1"));
  });

  it("should call the onSave callback", function () {
    var component = renderComponent();

    var saveButton = $(".company-details-button-save", component.getDOMNode());
    TestUtils.Simulate.click(saveButton[0]);

    assert.isTrue(component.props.onSave.called);
  });
});
