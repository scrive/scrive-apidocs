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
      featuresIsInherited: false,
      features: {
        adminUsers: {
          canUseTemplates: true,
          canUseShareableLinks: true,
          canUseBranding: true,
          canUseAuthorAttachments: true,
          canUseSignatoryAttachments: false,
          canUseMassSendout: true,
          canUseSMSInvitations: true,
          canUseSMSConfirmations: true,
          canUseDKCPRAuthenticationToView: true,
          canUseDKPIDAuthenticationToView: true,
          canUseDKCVRAuthenticationToView: true,
          canUseDKCPRAuthenticationToSign: true,
          canUseDKPIDAuthenticationToSign: true,
          canUseDKCVRAuthenticationToSign: true,
          canUseFIAuthenticationToView: true,
          canUseNOAuthenticationToView: true,
          canUseNOAuthenticationToSign: true,
          canUseSEAuthenticationToView: true,
          canUseSEAuthenticationToSign: true,
          canUseSMSPinAuthenticationToView: true,
          canUseSMSPinAuthenticationToSign: true,
          canUseStandardAuthenticationToView: true,
          canUseStandardAuthenticationToSign: true,
          canUseVerimiAuthenticationToView: true,
          canUseIDINAuthenticationToView: true,
          canUseIDINAuthenticationToSign: true,
          canUseEmailInvitations: true,
          canUseEmailConfirmations: true,
          canUseAPIInvitations: true,
          canUsePadInvitations: true,
          canUseForwarding: true,
          canUsePortal: true,
          canUseCustomSMSTexts: true
        },
        regularUsers: {
          canUseTemplates: true,
          canUseShareableLinks: true,
          canUseBranding: true,
          canUseAuthorAttachments: true,
          canUseSignatoryAttachments: false,
          canUseMassSendout: true,
          canUseSMSInvitations: true,
          canUseSMSConfirmations: true,
          canUseDKCPRAuthenticationToView: true,
          canUseDKPIDAuthenticationToView: true,
          canUseDKCVRAuthenticationToView: true,
          canUseDKCPRAuthenticationToSign: true,
          canUseDKPIDAuthenticationToSign: true,
          canUseDKCVRAuthenticationToSign: true,
          canUseFIAuthenticationToView: true,
          canUseNOAuthenticationToView: true,
          canUseNOAuthenticationToSign: true,
          canUseSEAuthenticationToView: true,
          canUseSEAuthenticationToSign: true,
          canUseSMSPinAuthenticationToView: true,
          canUseSMSPinAuthenticationToSign: true,
          canUseStandardAuthenticationToView: true,
          canUseStandardAuthenticationToSign: true,
          canUseVerimiAuthenticationToView: true,
          canUseIDINAuthenticationToView: true,
          canUseIDINAuthenticationToSign: true,
          canUseEmailInvitations: true,
          canUseEmailConfirmations: true,
          canUseAPIInvitations: true,
          canUsePadInvitations: true,
          canUseForwarding: true,
          canUsePortal: true,
          canUseCustomSMSTexts: true
        }
      }
    });
    component.forceUpdate();
    var checkedCheckboxes = $(".checkbox.checked", component.getDOMNode());
    assert.equal(checkedCheckboxes.size(), 64); // 4 options were set to false

    TestUtils.Simulate.click(checkedCheckboxes[0]); // Lets pick one to uncheck
    component.forceUpdate();
    var changedCheckedCheckboxes = $(".checkbox.checked", component.getDOMNode());
    assert.equal(changedCheckedCheckboxes.size(), 63);
  });
});
