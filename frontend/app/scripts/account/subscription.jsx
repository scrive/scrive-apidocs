var Backbone = require("backbone");
var Submit = require("../../js/submits.js").Submit;

/* Main archive definition. Its a tab based set of different documents lists. */

var FREE_DOCUMENT_LIMIT = 3;
var TEAM_DOCUMENT_LIMIT = 100;

module.exports = Backbone.Model.extend({
  defaults: {
    "payment_plan": "free",
    "number_of_users": 0,
    "started_last_month": 0,
    "ready": false
  },
  initialize: function (args) {
      if (args.forAdmin && args.companyid != undefined)
        this.url = "/adminonly/companyadmin/getsubscription/" + args.companyid;
      else
        this.url = "/api/frontend/getsubscription";
  },
  reload: function () {
    this.set({"ready": false}, {silent: true});
    this.fetch({cache: false, processData: true});
  },
  ready: function () {
     return this.get("ready");
  },
  companyid: function () {
     return this.get("companyid");
  },
  paymentplan: function () {
     return this.get("payment_plan");
  },
  hasFreePlan: function () {
     return this.paymentplan() == "free";
  },
  hasOnePlan: function () {
     return this.paymentplan() == "one";
  },
  hasTeamPlan: function () {
     return this.paymentplan() == "team";
  },
  hasEnterprisePlan: function () {
     return this.paymentplan() == "enterprise";
  },
  hasTrialPlan: function () {
     return this.paymentplan() == "trial";
  },
  numberOfUsers: function () {
     return this.get("number_of_users");
  },
  startedLastMonth: function () {
     return this.get("started_last_month");
  },
  isOverLimit: function () {
    if (this.hasFreePlan() && this.startedLastMonth() >= FREE_DOCUMENT_LIMIT) {
      return true;
    } else if (this.hasTeamPlan() && this.startedLastMonth() >= TEAM_DOCUMENT_LIMIT) {
      return true;
    }
    return false;
  },
  updateSubscriptionAsAdmin: function (newPlan, callback) {
    new Submit({
      method: "POST",
      url: "/adminonly/companyadmin/updatesubscription/" + this.companyid(),
      payment_plan: newPlan,
      ajaxsuccess: callback
    }).sendAjax();
  },
  parse: function (args) {
     return {
      payment_plan: args.payment_plan,
      number_of_users: args.number_of_users,
      started_last_month: args.started_last_month,
      ready: true
    };
  }
});

