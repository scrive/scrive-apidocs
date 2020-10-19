var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var SubscriptionPanelModel = require("./subscriptionpanelmodel");
var SubscriptionSelectionPanel = require("./subscriptionselectionpanel");
var Button = require("../../common/button");

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    propTypes: {
      companyAdmin: React.PropTypes.bool
    },
    getInitialState: function () {
      return {
        model: new SubscriptionPanelModel()
      };
    },
    getBackboneModels: function () {
      return [this.state.model];
    },
    reload: function () {
      this.state.model.reload();
    },
    planName: function () {
      var subscription = this.state.model.subscription();
      if (subscription.hasFreePlan()) {
        return "";
      } else if (subscription.hasPaidPlan()) {
        return localization.payments.plans.paid.name;
      } else if (subscription.hasTrialPlan()) {
        return localization.payments.plans.trial.name;
      }
    },
    render: function () {
      if (!this.state.model.ready()) {
        return (<div/>);
      }
      var subscription = this.state.model.subscription();
      return (
        <div className="tab-container">
          <div className="tab-content subscription">
            { /* if */ (!subscription.hasFreePlan()) &&
              <div className="subscription-plan">
                <div className="account-header">
                  <h4>{localization.payments.table.currentplan}</h4>
                </div>
                <div className="plan-information" >
                  <p className="plan-name">
                    {this.planName()}
                  </p>
                  <p>
                    {localization.payments.numberOfUsers + " : " + subscription.numberOfUsers()}
                  </p>
                  </div>
                <div className="contact-sales">
                  <HtmlTextWithSubstitution
                    secureText={localization.payments.contactSales}
                    links={{".put-link-to-mail-sales-here": "mailto:info@scrive.com"}}
                  />
                </div>
              </div>
           }
           { /* else */ (subscription.hasFreePlan()) &&
              <SubscriptionSelectionPanel
                fstname={this.state.model.user().fstname()}
                sndname={this.state.model.user().sndname()}
                email={this.state.model.user().email()}
              />
           }
          </div>
        </div>
      );
    }
});
