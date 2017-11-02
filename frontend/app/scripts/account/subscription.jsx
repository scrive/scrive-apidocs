var Backbone = require("backbone");
var _ = require("underscore");
var Submit = require("../../js/submits.js").Submit;

/* Main archive definition. Its a tab based set of different documents lists. */

var FREE_DOCUMENT_LIMIT = 3;
var TEAM_DOCUMENT_LIMIT = 100;

var Subscription = Backbone.Model.extend({
  defaults: {
    "payment_plan": "free",
    "number_of_users": 0,
    "started_last_month": 0,
    "can_use_templates": true,
    "can_use_branding": true,
    "can_use_author_attachments": true,
    "can_use_signatory_attachments": true,
    "can_use_mass_sendout": true,
    "can_use_sms_invitations": true,
    "can_use_sms_confirmations": true,
    "can_use_dk_authentication_to_view": true,
    "can_use_no_authentication_to_view": true,
    "can_use_se_authentication_to_view": true,
    "can_use_se_authentication_to_sign": true,
    "can_use_sms_pin_authentication_to_sign": true,
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
  canUseTemplates: function () {
     return this.get("can_use_templates");
  },
  canUseBranding: function () {
     return this.get("can_use_branding");
  },
  canUseAuthorAttachments: function () {
     return this.get("can_use_author_attachments");
  },
  canUseSignatoryAttachments: function () {
     return this.get("can_use_signatory_attachments");
  },
  canUseMassSendout: function () {
     return this.get("can_use_mass_sendout");
  },
  canUseSMSInvitations: function () {
     return this.get("can_use_sms_invitations");
  },
  canUseSMSConfirmations: function () {
     return this.get("can_use_sms_confirmations");
  },
  canUseDKAuthenticationToView: function () {
     return this.get("can_use_dk_authentication_to_view");
  },
  canUseNOAuthenticationToView: function () {
     return this.get("can_use_no_authentication_to_view");
  },
  canUseSEAuthenticationToView: function () {
     return this.get("can_use_se_authentication_to_view");
  },
  canUseSEAuthenticationToSign: function () {
     return this.get("can_use_se_authentication_to_sign");
  },
  canUseSMSPinAuthenticationToSign: function () {
     return this.get("can_use_sms_pin_authentication_to_sign");
  },
  canUseNonstandardAuthenticationToView: function () {
    return this.canUseDKAuthenticationToView() ||
      this.canUseNOAuthenticationToView() ||
      this.canUseSEAuthenticationToView();
  },
  canUseNonstandardAuthenticationToSign: function () {
    return this.canUseSEAuthenticationToSign() ||
      this.canUseSMSPinAuthenticationToSign();
  },
  isOverLimit: function () {
    if (this.hasFreePlan() && this.startedLastMonth() >= FREE_DOCUMENT_LIMIT) {
      return true;
    } else if (this.hasTeamPlan() && this.startedLastMonth() >= TEAM_DOCUMENT_LIMIT) {
      return true;
    }
    return false;
  },
  updateSubscriptionAsAdmin: function (nsd, callback) {
    var pick = function (param, currentValue) {
      return _.isUndefined(param) ? currentValue : param;
    };
    new Submit({
      method: "POST",
      url: "/adminonly/companyadmin/updatesubscription/" + this.companyid(),
      payment_plan: pick(nsd.selectedPlan, this.paymentplan()),
      can_use_templates: pick(nsd.canUseTemplates, this.canUseTemplates()),
      can_use_branding: pick(nsd.canUseBranding, this.canUseBranding()),
      can_use_author_attachments: pick(nsd.canUseAuthorAttachments, this.canUseAuthorAttachments()),
      can_use_signatory_attachments: pick(nsd.canUseSignatoryAttachments, this.canUseSignatoryAttachments()),
      can_use_mass_sendout: pick(nsd.canUseMassSendout, this.canUseMassSendout()),
      can_use_sms_invitations: pick(nsd.canUseSMSInvitations, this.canUseSMSInvitations()),
      can_use_sms_confirmations: pick(nsd.canUseSMSConfirmations, this.canUseSMSConfirmations()),
      can_use_dk_authentication_to_view: pick(nsd.canUseDKAuthenticationToView, this.canUseDKAuthenticationToView()),
      can_use_no_authentication_to_view: pick(nsd.canUseNOAuthenticationToView, this.canUseNOAuthenticationToView()),
      can_use_se_authentication_to_view: pick(nsd.canUseSEAuthenticationToView, this.canUseSEAuthenticationToView()),
      can_use_se_authentication_to_sign: pick(nsd.canUseSEAuthenticationToSign, this.canUseSEAuthenticationToSign()),
      can_use_sms_pin_authentication_to_sign: pick(
        nsd.canUseSMSPinAuthenticationToSign, this.canUseSMSPinAuthenticationToSign()
      ),
      ajaxsuccess: callback
    }).sendAjax();
  },
  parse: function (args) {
     return {
      payment_plan: args.payment_plan,
      number_of_users: args.number_of_users,
      started_last_month: args.started_last_month,
      can_use_templates: args.can_use_templates,
      can_use_branding: args.can_use_branding,
      can_use_author_attachments: args.can_use_author_attachments,
      can_use_signatory_attachments: args.can_use_signatory_attachments,
      can_use_mass_sendout: args.can_use_mass_sendout,
      can_use_sms_invitations: args.can_use_sms_invitations,
      can_use_sms_confirmations: args.can_use_sms_confirmations,
      can_use_dk_authentication_to_view: args.can_use_dk_authentication_to_view,
      can_use_no_authentication_to_view: args.can_use_no_authentication_to_view,
      can_use_se_authentication_to_view: args.can_use_se_authentication_to_view,
      can_use_se_authentication_to_sign: args.can_use_se_authentication_to_sign,
      can_use_sms_pin_authentication_to_sign: args.can_use_sms_pin_authentication_to_sign,
      ready: true
    };
  }
});

/* Static methods */
Subscription.initCurrentSubscription = function (subscriptionData) {
  window.currentSubscription = new Subscription(_.extend(subscriptionData, {ready: true}));
};

Subscription.currentSubscription = function () {
  if (_.isUndefined(window.currentSubscription)) {
    window.currentSubscription = new Subscription({});
  }
  return window.currentSubscription;
};

module.exports = Subscription;