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
    initStateFromSubscription : function() {
      var subscription = this.state.subscription;
      this.setState({
        initiated: true,
        canUseTemplates: subscription.canUseTemplates(),
        canUseBranding: subscription.canUseBranding(),
        canUseAuthorAttachments: subscription.canUseAuthorAttachments(),
        canUseSignatoryAttachments: subscription.canUseSignatoryAttachments(),
        canUseMassSendout: subscription.canUseMassSendout(),
        canUseSMSInvitations: subscription.canUseSMSInvitations(),
        canUseSMSConfirmations: subscription.canUseSMSConfirmations(),
        canUseDKAuthenticationToView: subscription.canUseDKAuthenticationToView(),
        canUseNOAuthenticationToView: subscription.canUseNOAuthenticationToView(),
        canUseNOAuthenticationToSign: subscription.canUseNOAuthenticationToSign(),
        canUseSEAuthenticationToView: subscription.canUseSEAuthenticationToView(),
        canUseSEAuthenticationToSign: subscription.canUseSEAuthenticationToSign(),
        canUseSMSPinAuthenticationToView : subscription.canUseSMSPinAuthenticationToView(),
        canUseSMSPinAuthenticationToSign: subscription.canUseSMSPinAuthenticationToSign()
      });
    },
    savePlan: function() {
      var self = this;
      this.state.subscription.updateSubscriptionAsAdmin({
          selectedPlan : this.state.selectedPlan,
          canUseTemplates : this.state.canUseTemplates,
          canUseBranding : this.state.canUseBranding,
          canUseAuthorAttachments : this.state.canUseAuthorAttachments,
          canUseSignatoryAttachments : this.state.canUseSignatoryAttachments,
          canUseMassSendout : this.state.canUseMassSendout,
          canUseSMSInvitations : this.state.canUseSMSInvitations,
          canUseSMSConfirmations : this.state.canUseSMSConfirmations,
          canUseDKAuthenticationToView : this.state.canUseDKAuthenticationToView,
          canUseNOAuthenticationToView : this.state.canUseNOAuthenticationToView,
          canUseNOAuthenticationToSign : this.state.canUseNOAuthenticationToSign,
          canUseSEAuthenticationToView : this.state.canUseSEAuthenticationToView,
          canUseSEAuthenticationToSign : this.state.canUseSEAuthenticationToSign,
          canUseSMSPinAuthenticationToView : this.state.canUseSMSPinAuthenticationToView,
          canUseSMSPinAuthenticationToSign : this.state.canUseSMSPinAuthenticationToSign
        }, function() {
          new FlashMessage({ type: "success", content: "Saved" });
          self.reload();
        });
    },
    changePlan: function(v) {
      var self = this;
      self.setState({selectedPlan : v});
      if (v == "free") {
        self.setState({
          canUseDKAuthenticationToView : false,
          canUseNOAuthenticationToView : false,
          canUseNOAuthenticationToSign : false,
          canUseSEAuthenticationToView : false,
          canUseSEAuthenticationToSign : false
        });
      } else {
        self.setState({
          canUseDKAuthenticationToView : true,
          canUseNOAuthenticationToView : true,
          canUseNOAuthenticationToSign : true,
          canUseSEAuthenticationToView : true,
          canUseSEAuthenticationToSign : true
        });
      }
    },
    componentWillUpdate: function() {
      if (!this.state.initiated) {
        this.initStateFromSubscription();
      }
    },
    renderTROptionSeparator(desc) {
      return (
        <tr>
          <td>
            <strong>{desc}</strong>
          </td>
        </tr>
      );
    },
    renderTRForOptionWithCheckbox(desc, stateProp) {
      var self = this;
      return (
        <tr>
          <td>
            {desc}
          </td>
          <td>
            <Checkbox
              disabled={!this.props.forAdmin}
              checked={this.state[stateProp]}
              onChange={function(v) { self.setState({[stateProp]:v})}}
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
          { /* if */ (subscription.ready()) &&
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
              {this.renderTROptionSeparator("General features")}
              {this.renderTRForOptionWithCheckbox("Can use templates","canUseTemplates")}
              {this.renderTRForOptionWithCheckbox("Can use branding","canUseBranding")}
              {this.renderTRForOptionWithCheckbox("Can use mass sendout","canUseMassSendout")}
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
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to view","canUseNOAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use NO authorization to sign","canUseNOAuthenticationToSign")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to view","canUseSEAuthenticationToView")}
              {this.renderTRForOptionWithCheckbox("Can use SE authorization to sign","canUseSEAuthenticationToSign")}
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
