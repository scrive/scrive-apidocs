var React = require("react");
var Utils = require("../archive/utils");
var moment = require("moment");
var _ = require("underscore");
var Subscription = require("../account/subscription");
var BackboneMixin = require("../common/backbone_mixin");
var Select = require("../common/select");
var Button = require("../common/button");
var Checkbox = require("../common/checkbox");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    propTypes: {
      loadLater: React.PropTypes.bool,
      companyid: React.PropTypes.string
    },
    getBackboneModels: function () {
      return [this.state.subscription];
    },
    componentDidMount: function () {
      if (this.props.loadLater === false) {
        this.reload();
      }
    },
    reload: function() {
      this.state.subscription.reload();
    },
    getInitialState: function () {
      return {
        subscription: new Subscription({companyid : this.props.companyid, forAdmin: true}),
        initiated: false,
      };
    },
    initFeatureFlagsFromFeatures : function(features) {
      return {
        canUseTemplates: features.canUseTemplates(),
        canUseShareableLinks: features.canUseShareableLinks(),
        canUseBranding: features.canUseBranding(),
        canUseAuthorAttachments: features.canUseAuthorAttachments(),
        canUseSignatoryAttachments: features.canUseSignatoryAttachments(),
        canUseMassSendout: features.canUseMassSendout(),
        canUseSMSInvitations: features.canUseSMSInvitations(),
        canUseSMSConfirmations: features.canUseSMSConfirmations(),
        canUseDKAuthenticationToView: features.canUseDKAuthenticationToView(),
        canUseDKAuthenticationToSign: features.canUseDKAuthenticationToSign(),
        canUseFIAuthenticationToView: features.canUseFIAuthenticationToView(),
        canUseNOAuthenticationToView: features.canUseNOAuthenticationToView(),
        canUseNOAuthenticationToSign: features.canUseNOAuthenticationToSign(),
        canUseSEAuthenticationToView: features.canUseSEAuthenticationToView(),
        canUseSEAuthenticationToSign: features.canUseSEAuthenticationToSign(),
        canUseSMSPinAuthenticationToView: features.canUseSMSPinAuthenticationToView(),
        canUseSMSPinAuthenticationToSign: features.canUseSMSPinAuthenticationToSign(),
        canUseStandardAuthenticationToView: features.canUseStandardAuthenticationToView(),
        canUseStandardAuthenticationToSign: features.canUseStandardAuthenticationToSign(),
        canUseVerimiAuthenticationToView: features.canUseVerimiAuthenticationToView(),
        canUseIDINAuthenticationToView: features.canUseIDINAuthenticationToView(),
        canUseEmailInvitations: features.canUseEmailInvitations(),
        canUseEmailConfirmations: features.canUseEmailConfirmations(),
        canUseAPIInvitations: features.canUseAPIInvitations(),
        canUsePadInvitations: features.canUsePadInvitations(),
        canUseForwarding: features.canUseForwarding(),
        canUseDocumentPartyNotifications: features.canUseDocumentPartyNotifications(),
      };
    },
    initStateFromSubscription : function() {
      var inheritedFeatures = undefined;
      if (this.state.subscription.inheritedFeatures()) {
        inheritedFeatures = {
          adminUsers : this.initFeatureFlagsFromFeatures(
            this.state.subscription.inheritedFeatures().adminUsers()),
          regularUsers : this.initFeatureFlagsFromFeatures(
            this.state.subscription.inheritedFeatures().regularUsers())
        };
      }
      var featuresIsInherited = this.state.subscription.featuresIsInherited();
      var features = {
        adminUsers : this.initFeatureFlagsFromFeatures(
          this.state.subscription.features().adminUsers()),
        regularUsers : this.initFeatureFlagsFromFeatures(
          this.state.subscription.features().regularUsers())
      };
      this.setState({
        initiated: true,
        features: features,
        inheritedFeatures: inheritedFeatures,
        featuresIsInherited: featuresIsInherited,
        selectedInvoicingType: this.state.subscription.invoicingtype(),
        selectedPlan: this.state.subscription.paymentplan()
      });
    },
    saveBilling: function() {
      var self = this;
      this.state.subscription.updateSubscriptionAsAdmin({
          selectedInvoicingType : self.state.selectedInvoicingType,
          selectedPlan : self.state.selectedPlan,
          features : self.state.features,
          featuresIsInherited : self.state.featuresIsInherited
        }, function () {
          new FlashMessage({ type: "success", content: "Saved" });
          self.reload();
        });
    },
    changePlan: function(v) {
      var adminUserFeatures = this.state.features.adminUsers;
      var regularUserFeatures = this.state.features.regularUsers;
      if (v == "free") {
          adminUserFeatures.canUseDKAuthenticationToView = false;
          adminUserFeatures.canUseDKAuthenticationToSign = false;
          adminUserFeatures.canUseFIAuthenticationToView = false;
          adminUserFeatures.canUseNOAuthenticationToView = false;
          adminUserFeatures.canUseNOAuthenticationToSign = false;
          adminUserFeatures.canUseSEAuthenticationToView = false;
          adminUserFeatures.canUseSEAuthenticationToSign = false;
          regularUserFeatures.canUseDKAuthenticationToView = false;
          regularUserFeatures.canUseDKAuthenticationToSign = false;
          regularUserFeatures.canUseFIAuthenticationToView = false;
          regularUserFeatures.canUseNOAuthenticationToView = false;
          regularUserFeatures.canUseNOAuthenticationToSign = false;
          regularUserFeatures.canUseSEAuthenticationToView = false;
          regularUserFeatures.canUseSEAuthenticationToSign = false;
          regularUserFeatures.canUseVerimiAuthenticationToView = false;
          regularUserFeatures.canUseIDINAuthenticationToView = false;
      } else {
          adminUserFeatures.canUseDKAuthenticationToView = true;
          adminUserFeatures.canUseDKAuthenticationToSign = true;
          adminUserFeatures.canUseFIAuthenticationToView = true;
          adminUserFeatures.canUseNOAuthenticationToView = true;
          adminUserFeatures.canUseNOAuthenticationToSign = true;
          adminUserFeatures.canUseSEAuthenticationToView = true;
          adminUserFeatures.canUseSEAuthenticationToSign = true;
          regularUserFeatures.canUseDKAuthenticationToView = true;
          regularUserFeatures.canUseDKAuthenticationToSign = true;
          regularUserFeatures.canUseFIAuthenticationToView = true;
          regularUserFeatures.canUseNOAuthenticationToView = true;
          regularUserFeatures.canUseNOAuthenticationToSign = true;
          regularUserFeatures.canUseSEAuthenticationToView = true;
          regularUserFeatures.canUseSEAuthenticationToSign = true;
          regularUserFeatures.canUseVerimiAuthenticationToView = true;
          regularUserFeatures.canUseIDINAuthenticationToView = true;
      }
      this.setState({
          selectedPlan: v,
          adminUserFeatures: adminUserFeatures,
          regularUserFeatures: regularUserFeatures
      });
    },
    changeInvoicingType: function(v) {
      this.setState({
          selectedInvoicingType: v,
      });
    },
    componentWillUpdate: function() {
      if (!this.state.initiated) {
        this.initStateFromSubscription();
      }
    },
    onFeaturesIsInheritedChange: function (event) {
      if (!this.state.inheritedFeatures && event.target.checked) {
        // we are trying to inherit, but there is no parent (no inherited features)
        new FlashMessage({
          content: "Top level user group cannot inherit",
          type: "error"
        });
        event.target.checked = false;
      } else {
        this.setState({featuresIsInherited : event.target.checked });
      }
    },
    renderTROptionSeparator: function (desc) {
      return (
        <tr><td><strong>{desc}</strong></td></tr>
      );
    },
    renderTRForOptionWithCheckbox: function (desc, stateProp) {
      var self = this;
      var localFeatures = undefined;
      var checkboxIsDisabled = false;
      var regularOnChange = undefined;
      var adminOnChange = undefined
      if (this.state.featuresIsInherited) {
        checkboxIsDisabled = true;
        localFeatures = this.state.inheritedFeatures;
      } else {
        localFeatures = this.state.features;
        regularOnChange = function (v) {
          localFeatures.regularUsers[stateProp] = v;
          self.setState({features: localFeatures});
        };
        adminOnChange = function (v) {
          localFeatures.adminUsers[stateProp] = v;
          self.setState({features: localFeatures});
        };
      }
      return (
        <tr>
          <td>{desc}</td>
          <td>
            <Checkbox
              disabled={checkboxIsDisabled}
              checked={localFeatures.regularUsers[stateProp]}
              onChange={regularOnChange}
            />
          </td>
          <td>
            <Checkbox
              disabled={checkboxIsDisabled}
              checked={localFeatures.adminUsers[stateProp]}
              onChange={adminOnChange}
            />
          </td>
        </tr>
      );
    },
    render: function() {
      var self = this;
      var subscription = this.state.subscription;
      var selectedInvoicingType = this.state.selectedInvoicingType;
      var selectedPlan = this.state.selectedPlan;
      var inheritedPlan = subscription.inheritedplan()?(" (" + subscription.inheritedplan()) + ")":"";
      var planOptions = function (it) {
        var obj =
          { "none":
            [
              {value : "inherit", name : "Inherit" + inheritedPlan},
            ],
          "billitem":
            [
              {value : "inherit", name : "Inherit" + inheritedPlan},
              {value : "free", name : "Free"},
              {value : "one",  name : "One"},
              {value : "team",  name : "Team"},
              {value : "enterprise",  name : "Enterprise"},
              {value : "trial",  name : "Trial"}
            ],
          "invoice":
            [
              {value : "free", name : "Free"},
              {value : "one",  name : "One"},
              {value : "team",  name : "Team"},
              {value : "enterprise",  name : "Enterprise"},
              {value : "trial",  name : "Trial"}
            ] };
        return obj[it?it:subscription.invoicingtype()];
      };

      return (
        <div className="tab-container account">
          { /* if */ subscription.ready() && this.state.initiated &&
            <table>
              <tr>
                <th style={{"width":"250px"}}>
                </th>
                <th style={{"width":"200px"}}>
                </th>
              </tr>
              <tr>
                <td>
                  Number of users
                </td>
                <td>
                  {subscription.numberOfUsers()}
                </td>
              </tr>

              <tr>
                <td>
                  Invoicing type
                </td>
                <td>
                  <Select
                    style={{display: "inline-block"}}
                    options={[
                      {value : "none", name : "None"},
                      {value : "billitem", name : "BillItem"},
                      {value : "invoice",  name : "Invoice"}
                    ]}
                    isOptionSelected={function(o) {
                      if (selectedInvoicingType) {
                        return selectedInvoicingType === o.value
                      } else {
                        return o.value === subscription.invoicingtype();
                      }
                    }}
                    onSelect={this.changeInvoicingType}
                  />
                </td>
              </tr>


              <tr>
                <td>
                  Price plan
                </td>
                <td>
                  <Select
                    style={{display: "inline-block"}}
                    options={planOptions(selectedInvoicingType)}
                    isOptionSelected={function(o) {
                      if (selectedPlan) {
                        return selectedPlan === o.value
                      } else {
                        return o.value === subscription.paymentplan();
                      }
                    }}
                    onSelect={this.changePlan}
                  />
                </td>
              </tr>
              <tr><td colSpan={3}><hr/></td></tr>
              <tr>
                <td><label>Inherit feature flags</label></td>
                <td>
                  <input
                    type="checkbox"
                    checked={this.state.featuresIsInherited}
                    onChange={this.onFeaturesIsInheritedChange}
                  />
                </td>
                <td>If enabled, all feature flags will be inherited from the parent user group.</td>
              </tr>
              <tr>
                <td></td>
                <td><strong>Regular users</strong></td>
                <td><strong>Admin users</strong></td>
              </tr>
              {this.renderTROptionSeparator("General features")}
              {this.renderTRForOptionWithCheckbox("Can use templates","canUseTemplates")}
              {this.renderTRForOptionWithCheckbox("Can use shareable links","canUseShareableLinks")}
              {this.renderTRForOptionWithCheckbox("Can use branding","canUseBranding")}
              {this.renderTRForOptionWithCheckbox("Can use mass sendout","canUseMassSendout")}
              {this.renderTRForOptionWithCheckbox("Can use email invitations","canUseEmailInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use email confirmations","canUseEmailConfirmations")}
              {this.renderTRForOptionWithCheckbox("Can use link (i.e. API) invitations","canUseAPIInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use in-person invitations","canUsePadInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use forwarding","canUseForwarding")}
              {this.renderTRForOptionWithCheckbox("Can use document party notifications","canUseDocumentPartyNotifications")}
              {this.renderTROptionSeparator("Attachments")}
              {this.renderTRForOptionWithCheckbox("Can use author attachments","canUseAuthorAttachments")}
              {this.renderTRForOptionWithCheckbox("Can use signatory attachments","canUseSignatoryAttachments")}
              {this.renderTROptionSeparator("SMS")}
              {this.renderTRForOptionWithCheckbox("Can use sms invitations","canUseSMSInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use sms confirmations","canUseSMSConfirmations")}
              {this.renderTRForOptionWithCheckbox("Can use SMS Pin authorization to view","canUseSMSPinAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use SMS Pin authorization to sign","canUseSMSPinAuthenticationToSign")}
              {this.renderTROptionSeparator("eID to view and sign")}
              {this.renderTRForOptionWithCheckbox("Can use DK authorization to view","canUseDKAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use DK authorization to sign","canUseDKAuthenticationToSign")}
              {this.renderTRForOptionWithCheckbox("Can use FI authorization to view","canUseFIAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to view","canUseNOAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to sign","canUseNOAuthenticationToSign")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to view","canUseSEAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to sign","canUseSEAuthenticationToSign")}
              {this.renderTROptionSeparator("Verimi auth to view")}
              {this.renderTRForOptionWithCheckbox("Can use Verimi authorization to view","canUseVerimiAuthenticationToView")}
              {this.renderTROptionSeparator("iDIN auth to view")}
              {this.renderTRForOptionWithCheckbox("Can use iDIN authorization to view","canUseIDINAuthenticationToView")}
              {this.renderTROptionSeparator("Standard auth to view and sign")}
              {this.renderTROptionSeparator("(disable to force use of eID/SMS)")}
              {this.renderTRForOptionWithCheckbox("Can use Standard authorization to view","canUseStandardAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use Standard authorization to sign","canUseStandardAuthenticationToSign")}
              <tr>
                <td>
                  <Button
                    type="action"
                    text="Save"
                    onClick={self.saveBilling}
                  />
                </td>
                <td>
                </td>
              </tr>
            </table>
          }
        </div>
      );
    }
});
