var React = require("react");
var _ = require("underscore");
var $ = require("jquery");

var backend = require("../backend");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var PaymentsPanel = require("../../scripts/admin/paymentspanel");

describe("admin/paymentspanel", function () {
  var container = null;
  var server = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = _.extendOwn({
        companyid: "1",
        forAdmin: true,
        loadLater: false
      },
      props || {}
    );

    var component = React.render(
      React.createElement(PaymentsPanel, actualProps), container
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

  it("should render the payments panel", function () {
    var component = renderComponent();
    var checkboxes = $(".checkbox", component.getDOMNode());
    // We should have 21 x 2 checkboxes. One for each option.
    // 2 sets - one for regular user, one for admin
    assert.equal(checkboxes.size(), 42);
    // We should have one select (for price plan)
    var selects = $(".select", component.getDOMNode());
    assert.equal(selects.size(), 1);
    // We there should be no disabled checkboxes
    var disabledCheckboxes = $(".checkbox.disabled", component.getDOMNode());
    assert.equal(disabledCheckboxes.size(), 0);

  });


  it("should render the payments panel for non-admin with disabled checkboxes", function () {
    var component = renderComponent({forAdmin: false});
    var checkboxes = $(".checkbox.disabled", component.getDOMNode());

    component.forceUpdate();
    // We should have 21 x 2 checkboxes. One for each option.
    // 2 sets - one for regular user, one for admin
    assert.equal(checkboxes.size(), 42);

  });

  it("should call subscriptions updateSubscriptionAsAdmin on button clicked", function () {
    var component = renderComponent({forAdmin: false});
    var subscription = component.state.subscription;
    sinon.stub(subscription, 'updateSubscriptionAsAdmin');

    var button = $(".button", component.getDOMNode());
    TestUtils.Simulate.click(button[0]);
    assert.isTrue(subscription.updateSubscriptionAsAdmin.called);

  });

  it("should render checked and unchecked checkboxes based on state", function () {
    var component = renderComponent();
    component.setState({
      adminUserFeatures: {
        canUseTemplates: true,
        canUseBranding: true,
        canUseAuthorAttachments: true,
        canUseSignatoryAttachments: false,
        canUseMassSendout: true,
        canUseSMSInvitations: true,
        canUseSMSConfirmations: true,
        canUseDKAuthenticationToView: false,
        canUseDKAuthenticationToSign: true,
        canUseNOAuthenticationToView: true,
        canUseNOAuthenticationToSign: true,
        canUseSEAuthenticationToView: true,
        canUseSEAuthenticationToSign: true,
        canUseSMSPinAuthenticationToView: true,
        canUseSMSPinAuthenticationToSign: true,
        canUseStandardAuthenticationToView: true,
        canUseStandardAuthenticationToSign: true,
        canUseEmailInvitations: true,
        canUseEmailConfirmations: true,
        canUseAPIInvitations: true,
        canUsePadInvitations: true
      },
      regularUserFeatures: {
        canUseTemplates: true,
        canUseBranding: true,
        canUseAuthorAttachments: true,
        canUseSignatoryAttachments: false,
        canUseMassSendout: true,
        canUseSMSInvitations: true,
        canUseSMSConfirmations: true,
        canUseDKAuthenticationToView: false,
        canUseDKAuthenticationToSign: true,
        canUseNOAuthenticationToView: true,
        canUseNOAuthenticationToSign: true,
        canUseSEAuthenticationToView: true,
        canUseSEAuthenticationToSign: true,
        canUseSMSPinAuthenticationToView: true,
        canUseSMSPinAuthenticationToSign: true,
        canUseStandardAuthenticationToView: true,
        canUseStandardAuthenticationToSign: true,
        canUseEmailInvitations: true,
        canUseEmailConfirmations: true,
        canUseAPIInvitations: true,
        canUsePadInvitations: true
      }
    });
    component.forceUpdate();
    var checkedCheckboxes = $(".checkbox.checked", component.getDOMNode());
    assert.equal(checkedCheckboxes.size(), 38); // 4 options were set to false

    TestUtils.Simulate.click(checkedCheckboxes[0]); // Lets pick one to uncheck
    component.forceUpdate();
    var changedCheckedCheckboxes = $(".checkbox.checked", component.getDOMNode());
    assert.equal(changedCheckedCheckboxes.size(), 37);


  });


});
