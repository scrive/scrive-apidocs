var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var util = require("../../../../util");

var TestUtils = React.addons.TestUtils;

var DeleteModal = require(
  "../../../../../scripts/admin/useradmin/userdetails/modals/delete"
);

describe("admin/useradmin/userdetails/modals/delete", function () {
  var container = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn(
      {active: false, onAccept: sinon.stub(), onCancel: sinon.stub()},
      props || {}
    );

    var component = React.render(
      React.createElement(DeleteModal, actualProps), container
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

  it("should render inactive modal", function () {
    var component = renderComponent();

    var modal = $(".modal.user-details-modal-delete");
    assert.lengthOf(modal, 1);
    assert.isFalse(modal.hasClass("active"));
  });

  it("should activate modal", function () {
    var component = renderComponent({active: true});

    var modal = $(".modal.user-details-modal-delete");
    assert.isTrue(modal.hasClass("active"));
  });

  it("should call the onCancel callback when modal is closed", function () {
    var component = renderComponent({active: true});

    var closeButton = $(".modal.user-details-modal-delete a.modal-close");

    TestUtils.Simulate.click(closeButton[0]);
    assert.isTrue(component.props.onCancel.called);
  });

  it("should call the onCancel callback when modal is cancelled", function () {
    var component = renderComponent({active: true});

    var cancelButton = $(".modal.user-details-modal-delete label.close");

    TestUtils.Simulate.click(cancelButton[0]);
    assert.isTrue(component.props.onCancel.called);
  });

  it("should call the onAccept callback when modal is accepted", function () {
    var component = renderComponent({active: true});

    var acceptButton = $(".modal.user-details-modal-delete a.cancel");

    TestUtils.Simulate.click(acceptButton[0]);
    assert.isTrue(component.props.onAccept.called);
  });
});
