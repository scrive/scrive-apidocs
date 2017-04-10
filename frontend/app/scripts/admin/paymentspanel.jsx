var React = require("react");
var Utils = require("../archive/utils");
var moment = require("moment");
var _ = require("underscore");
var Subscription = require("../account/subscription");
var BackboneMixin = require("../common/backbone_mixin");
var Select = require("../common/select");
var Button = require("../common/button");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    propTypes: {
      loadLater: React.PropTypes.bool
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
        selectedPlan: undefined
      };
    },
    savePlan: function() {
      var self = this;
      if (this.state.selectedPlan) {
        this.state.subscription.updateSubscriptionAsAdmin(this.state.selectedPlan, function() {
          new FlashMessage({ type: "success", content: "Saved" });
          self.reload();
        });
      } else {
        new FlashMessage({ type: "success", content: "Nothing changed" });
        this.reload();
      }
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
                <th style={{"width":"120px"}}>
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
                    onSelect={function(v) {
                      self.setState({selectedPlan : v});
                    }}
                  />
                </td>
              </tr>
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
