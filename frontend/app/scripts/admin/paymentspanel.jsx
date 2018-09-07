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
      forAdmin: React.PropTypes.bool,
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
        canUseBranding: features.canUseBranding(),
        canUseAuthorAttachments: features.canUseAuthorAttachments(),
        canUseSignatoryAttachments: features.canUseSignatoryAttachments(),
        canUseMassSendout: features.canUseMassSendout(),
        canUseSMSInvitations: features.canUseSMSInvitations(),
        canUseSMSConfirmations: features.canUseSMSConfirmations(),
        canUseDKAuthenticationToView: features.canUseDKAuthenticationToView(),
        canUseDKAuthenticationToSign: features.canUseDKAuthenticationToSign(),
        canUseNOAuthenticationToView: features.canUseNOAuthenticationToView(),
        canUseNOAuthenticationToSign: features.canUseNOAuthenticationToSign(),
        canUseSEAuthenticationToView: features.canUseSEAuthenticationToView(),
        canUseSEAuthenticationToSign: features.canUseSEAuthenticationToSign(),
        canUseSMSPinAuthenticationToView: features.canUseSMSPinAuthenticationToView(),
        canUseSMSPinAuthenticationToSign: features.canUseSMSPinAuthenticationToSign(),
        canUseStandardAuthenticationToView: features.canUseStandardAuthenticationToView(),
        canUseStandardAuthenticationToSign: features.canUseStandardAuthenticationToSign(),
        canUseEmailInvitations: features.canUseEmailInvitations(),
        canUseAPIInvitations: features.canUseAPIInvitations(),
        canUsePadInvitations: features.canUsePadInvitations()
      };
    },
    initStateFromSubscription : function() {
      var adminFs   = this.state.subscription.featuresForAdminUsers();
      var regularFs = this.state.subscription.featuresForRegularUsers();
      var adminUserFeatures   = this.initFeatureFlagsFromFeatures(adminFs);
      var regularUserFeatures = this.initFeatureFlagsFromFeatures(regularFs);
      this.setState({
        initiated: true,
        adminUserFeatures: adminUserFeatures,
        regularUserFeatures: regularUserFeatures
      });
    },
    savePlan: function() {
      var self = this;
      this.state.subscription.updateSubscriptionAsAdmin({
          selectedPlan : self.state.selectedPlan,
          adminUserFeatures: self.state.adminUserFeatures,
          regularUserFeatures: self.state.regularUserFeatures
        }, function () {
          new FlashMessage({ type: "success", content: "Saved" });
          self.reload();
        });
    },
    changePlan: function(v) {
      var adminUserFeatures = this.state.adminUserFeatures;
      var regularUserFeatures = this.state.regularUserFeatures;
      if (v == "free") {
          adminUserFeatures.canUseDKAuthenticationToView = false;
          adminUserFeatures.canUseDKAuthenticationToSign = false;
          adminUserFeatures.canUseNOAuthenticationToView = false;
          adminUserFeatures.canUseNOAuthenticationToSign = false;
          adminUserFeatures.canUseSEAuthenticationToView = false;
          adminUserFeatures.canUseSEAuthenticationToSign = false;
          regularUserFeatures.canUseDKAuthenticationToView = false;
          regularUserFeatures.canUseDKAuthenticationToSign = false;
          regularUserFeatures.canUseNOAuthenticationToView = false;
          regularUserFeatures.canUseNOAuthenticationToSign = false;
          regularUserFeatures.canUseSEAuthenticationToView = false;
          regularUserFeatures.canUseSEAuthenticationToSign = false;
      } else {
          adminUserFeatures.canUseDKAuthenticationToView = true;
          adminUserFeatures.canUseDKAuthenticationToSign = true;
          adminUserFeatures.canUseNOAuthenticationToView = true;
          adminUserFeatures.canUseNOAuthenticationToSign = true;
          adminUserFeatures.canUseSEAuthenticationToView = true;
          adminUserFeatures.canUseSEAuthenticationToSign = true;
          regularUserFeatures.canUseDKAuthenticationToView = true;
          regularUserFeatures.canUseDKAuthenticationToSign = true;
          regularUserFeatures.canUseNOAuthenticationToView = true;
          regularUserFeatures.canUseNOAuthenticationToSign = true;
          regularUserFeatures.canUseSEAuthenticationToView = true;
          regularUserFeatures.canUseSEAuthenticationToSign = true;
      }
      this.setState({
          selectedPlan: v,
          adminUserFeatures: adminUserFeatures,
          regularUserFeatures: regularUserFeatures
      });
    },
    componentWillUpdate: function() {
      if (!this.state.initiated) {
        this.initStateFromSubscription();
      }
    },
    renderTROptionSeparator: function (desc) {
      return (
        <tr><td><strong>{desc}</strong></td></tr>
      );
    },
    renderTRForOptionWithCheckbox: function (desc, stateProp) {
      var self = this;
      var adminUserFeatures = this.state.adminUserFeatures;
      var regularUserFeatures = this.state.regularUserFeatures;
      return (
        <tr>
          <td>{desc}</td>
          <td>
            <Checkbox
              disabled={!this.props.forAdmin}
              checked={regularUserFeatures[stateProp]}
              onChange={function (v) {
                  regularUserFeatures[stateProp] = v;
                  self.setState({regularUserFeatures: regularUserFeatures});
              }}
            />
          </td>
          <td>
            <Checkbox
              disabled={!this.props.forAdmin}
              checked={adminUserFeatures[stateProp]}
              onChange={function (v) {
                  adminUserFeatures[stateProp] = v;
                  self.setState({adminUserFeatures: adminUserFeatures});
              }}
            />
          </td>
        </tr>
      );
    },
    render: function() {
      var self = this;
      var subscription = this.state.subscription;
      var selectedPlan = this.state.selectedPlan;
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
                  Price plan
                </td>
                <td>
                  <Select
                    style={{display: "inline-block"}}
                    options={[
                      {value : "free", name : "Free"},
                      {value : "one",  name : "One"},
                      {value : "team",  name : "Team"},
                      {value : "enterprise",  name : "Enterprise"},
                      {value : "trial",  name : "Trial"},
                    ]}
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
              <tr>
                <td></td>
                <td><strong>Regular users</strong></td>
                <td><strong>Admin users</strong></td>
              </tr>
              {this.renderTROptionSeparator("General features")}
              {this.renderTRForOptionWithCheckbox("Can use templates","canUseTemplates")}
              {this.renderTRForOptionWithCheckbox("Can use branding","canUseBranding")}
              {this.renderTRForOptionWithCheckbox("Can use mass sendout","canUseMassSendout")}
              {this.renderTRForOptionWithCheckbox("Can use email invitations","canUseEmailInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use link (i.e. API) invitations","canUseAPIInvitations")}
              {this.renderTRForOptionWithCheckbox("Can use in-person invitations","canUsePadInvitations")}
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
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to view","canUseNOAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to sign","canUseNOAuthenticationToSign")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to view","canUseSEAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to sign","canUseSEAuthenticationToSign")}
              {this.renderTROptionSeparator("Standard auth to view and sign")}
              {this.renderTROptionSeparator("(disable to force use of eID/SMS)")}
              {this.renderTRForOptionWithCheckbox("Can use Standard authorization to view","canUseStandardAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use Standard authorization to sign","canUseStandardAuthenticationToSign")}
              <tr>
                <td>
                  <Button
                    type="action"
                    text="Save"
                    onClick={self.savePlan}
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
