var Backbone = require("backbone");
var _ = require("underscore");
var Submit = require("../../js/submits.js").Submit;
var User = require("../../js/account/user.js").User;

/* Main archive definition. Its a tab based set of different documents lists. */

var FREE_DOCUMENT_LIMIT = 3;
var TEAM_DOCUMENT_LIMIT = 100;

var Subscription = Backbone.Model.extend({
  defaults: {
    "payment_plan": "free",
    "number_of_users": 0,
    "started_last_month": 0,
    "features_admin_users": undefined,
    "features_regular_users": undefined,
    "current_user_is_admin": undefined,
    "ready": false
  },
  initialize: function (args) {
    if (args != undefined && args.forAdmin && args.companyid != undefined)
      this.url = "/adminonly/companyadmin/getsubscription/" + args.companyid;
    else
      this.url = "/api/frontend/getsubscription";

    if (args != undefined && args.features) {
      var adminFF = new FeatureFlag(args.features.admin_users);
      var regularFF = new FeatureFlag(args.features.regular_users);
      this.set({"features_admin_users": adminFF, "features_regular_users": regularFF});
    }

    if (args.current_user_is_admin != undefined) {
       this.set({"current_user_is_admin": args.current_user_is_admin});
    }
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
  currentUserIsAdmin: function () {
     return this.get("current_user_is_admin");
  },

  noFeatures: function () {
    return new FeatureFlag({});
  },

  featuresForAdminUsers: function () {
    return this.get("features_admin_users") || this.noFeatures();
  },

  featuresForRegularUsers: function () {
    return this.get("features_regular_users") || this.noFeatures();
  },

  currentUserFeatures: function () {
     var isAdmin = this.currentUserIsAdmin();
     if (isAdmin != undefined && isAdmin) {
       return this.featuresForAdminUsers();
     } else if (isAdmin != undefined && !isAdmin) {
       return this.featuresForRegularUsers();
     } else {
       return this.noFeatures();
     }
  },
  isOverLimit: function (numberOfDocs) {
    if (numberOfDocs === undefined) {
      numberOfDocs = 1;
    }
    if (this.hasFreePlan() && (this.startedLastMonth() + numberOfDocs) > FREE_DOCUMENT_LIMIT) {
      return true;
    } else if (this.hasTeamPlan() && (this.startedLastMonth() + numberOfDocs) > TEAM_DOCUMENT_LIMIT) {
      return true;
    }
    return false;
  },
  updateSubscriptionAsAdmin: function (nsd, callback) {
    var self = this;
    var pick = function (param, currentValue) {
        return _.isUndefined(param) ? currentValue : param;
    };
    var pickToFeature = function (f, forAdmin) {
        var ff = self.featuresForRegularUsers();
        if (forAdmin)
            ff = self.featuresForAdminUsers();
        return ({
            can_use_templates: pick(f.canUseTemplates, ff.canUseTemplates()),
            can_use_branding: pick(f.canUseBranding, ff.canUseBranding()),
            can_use_author_attachments: pick(
                f.canUseAuthorAttachments, ff.canUseAuthorAttachments()
            ),
            can_use_signatory_attachments: pick(
                f.canUseSignatoryAttachments, ff.canUseSignatoryAttachments()
            ),
            can_use_mass_sendout: pick(
                f.canUseMassSendout, ff.canUseMassSendout()
            ),
            can_use_sms_invitations: pick(
                f.canUseSMSInvitations, ff.canUseSMSInvitations()
            ),
            can_use_sms_confirmations: pick(
                f.canUseSMSConfirmations, ff.canUseSMSConfirmations()
            ),
            can_use_dk_authentication_to_view: pick(
                f.canUseDKAuthenticationToView, ff.canUseDKAuthenticationToView()
            ),
            can_use_dk_authentication_to_sign: pick(
                f.canUseDKAuthenticationToSign, ff.canUseDKAuthenticationToSign()
            ),
            can_use_fi_authentication_to_view: pick(
                f.canUseFIAuthenticationToView, ff.canUseFIAuthenticationToView()
            ),
            can_use_no_authentication_to_view: pick(
                f.canUseNOAuthenticationToView, ff.canUseNOAuthenticationToView()
            ),
            can_use_no_authentication_to_sign: pick(
                f.canUseNOAuthenticationToSign, ff.canUseNOAuthenticationToSign()
            ),
            can_use_se_authentication_to_view: pick(
                f.canUseSEAuthenticationToView, ff.canUseSEAuthenticationToView()
            ),
            can_use_se_authentication_to_sign: pick(
                f.canUseSEAuthenticationToSign, ff.canUseSEAuthenticationToSign()
            ),
            can_use_sms_pin_authentication_to_view: pick(
              f.canUseSMSPinAuthenticationToView, ff.canUseSMSPinAuthenticationToView()
            ),
            can_use_sms_pin_authentication_to_sign: pick(
              f.canUseSMSPinAuthenticationToSign, ff.canUseSMSPinAuthenticationToSign()
            ),
            can_use_standard_authentication_to_view: pick(
              f.canUseStandardAuthenticationToView, ff.canUseStandardAuthenticationToView()
            ),
            can_use_standard_authentication_to_sign: pick(
              f.canUseStandardAuthenticationToSign, ff.canUseStandardAuthenticationToSign()
            ),
            can_use_email_invitations: pick(
              f.canUseEmailInvitations, ff.canUseEmailInvitations()
            ),
            can_use_api_invitations: pick(
              f.canUseAPIInvitations, ff.canUseAPIInvitations()
            ),
            can_use_pad_invitations: pick(
              f.canUsePadInvitations, ff.canUsePadInvitations()
            )
        });
    };
    var features = {
        admin_users: pickToFeature(nsd.adminUserFeatures, true),
        regular_users: pickToFeature(nsd.regularUserFeatures, false)
    };
    var newSubscription = {
        payment_plan: pick(nsd.selectedPlan, this.paymentplan()),
        features: features
    };
    new Submit({
      method: "POST",
      url: "/adminonly/companyadmin/updatesubscription/" + this.companyid(),
      subscription: JSON.stringify(newSubscription),
      ajaxsuccess: callback
    }).sendAjax();
  },
  parse: function (args) {
    return {
      payment_plan: args.payment_plan,
      number_of_users: args.number_of_users,
      started_last_month: args.started_last_month,
      features_admin_users: new FeatureFlag(args.features.admin_users),
      features_regular_users: new FeatureFlag(args.features.regular_users),
      ready: true
    };
  }
});

var Features = exports.Features = Backbone.Model.extend({
  initialize: function (args) {
     this.admin_users = new FeatureFlag(args.admin_users);
     this.regular_users = new FeatureFlag(args.regular_users);
  },
  adminUsers: function () {
     return this.get("admin_users");
  },
  regularUsers: function () {
     return this.get("regular_users");
  },
  parse: function (args) {
    return {
      admin_users: new FeatureFlag(args.admin_users),
      regular_users: new FeatureFlag(args.regular_users)
    };
  }
});

var FeatureFlag = exports.FeatureFlag = Backbone.Model.extend({
  defaults: {
    "can_use_templates": true,
    "can_use_branding": true,
    "can_use_author_attachments": true,
    "can_use_signatory_attachments": true,
    "can_use_mass_sendout": true,
    "can_use_sms_invitations": true,
    "can_use_sms_confirmations": true,
    "can_use_dk_authentication_to_view": true,
    "can_use_dk_authentication_to_sign": true,
    "can_use_no_authentication_to_view": true,
    "can_use_no_authentication_to_sign": true,
    "can_use_se_authentication_to_view": true,
    "can_use_se_authentication_to_sign": true,
    "can_use_sms_pin_authentication_to_view": true,
    "can_use_sms_pin_authentication_to_sign": true,
    "can_use_standard_authentication_to_view": true,
    "can_use_standard_authentication_to_sign": true,
    "can_use_email_invitations": true,
    "can_use_api_invitations": true,
    "can_use_pad_invitations": true
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
  canUseDKAuthenticationToSign: function () {
     return this.get("can_use_dk_authentication_to_sign");
  },
  canUseFIAuthenticationToView: function () {
     return this.get("can_use_fi_authentication_to_view");
  },
  canUseNOAuthenticationToView: function () {
     return this.get("can_use_no_authentication_to_view");
  },
  canUseNOAuthenticationToSign: function () {
     return this.get("can_use_no_authentication_to_sign");
  },
  canUseSEAuthenticationToView: function () {
     return this.get("can_use_se_authentication_to_view");
  },
  canUseSEAuthenticationToSign: function () {
     return this.get("can_use_se_authentication_to_sign");
  },
  canUseSMSPinAuthenticationToView: function () {
     return this.get("can_use_sms_pin_authentication_to_view");
  },
  canUseSMSPinAuthenticationToSign: function () {
     return this.get("can_use_sms_pin_authentication_to_sign");
  },
  canUseStandardAuthenticationToView: function () {
     return this.get("can_use_standard_authentication_to_view");
  },
  canUseStandardAuthenticationToSign: function () {
     return this.get("can_use_standard_authentication_to_sign");
  },
  canUseEmailInvitations: function () {
     return this.get("can_use_email_invitations");
  },
  canUseAPIInvitations: function () {
     return this.get("can_use_api_invitations");
  },
  canUsePadInvitations: function () {
     return this.get("can_use_pad_invitations");
  },
  firstAllowedAuthenticationToView: function () {
    if (this.canUseStandardAuthenticationToView())
      return "standard";
    else if (this.canUseSEAuthenticationToView())
      return "se_bankid";
    else if (this.canUseNOAuthenticationToView())
      return "no_bankid";
    else if (this.canUseDKAuthenticationToView())
      return "dk_bankid";
    else if (this.canUseFIAuthenticationToView())
      return "fi_tupas";
    else if (this.canUseSMSPinAuthenticationToView())
      return "sms_pin";
    else
      // Should not happen, just in case
      return "standard";
  },
  firstAllowedAuthenticationToSign: function () {
    if (this.canUseStandardAuthenticationToSign())
      return "standard";
    else if (this.canUseSEAuthenticationToSign())
      return "se_bankid";
    else if (this.canUseNOAuthenticationToSign())
      return "no_bankid";
    else if (this.canUseDKAuthenticationToSign())
      return "dk_bankid";
    else if (this.canUseSMSPinAuthenticationToSign())
      return "sms_pin";
    else
      // Should not happen, just in case
      return "standard";
  },
  firstAllowedInvitationDelivery: function () {
    if (this.canUseEmailInvitations())
      return "email";
    else if (this.canUseSMSInvitations())
      return "mobile";
    else if (this.canUseEmailInvitations() && this.canUseSMSInvitations())
      return "email_mobile";
    else if (this.canUseAPIInvitations())
      return "api";
    else if (this.canUsePadInvitations())
      return "pad";
    else
      // Should not happen, just in case
      return "email";
  },
  canUseNonstandardAuthenticationToView: function () {
    return this.canUseDKAuthenticationToView() ||
      this.canUseNOAuthenticationToView() ||
      this.canUseSEAuthenticationToView() ||
      this.canUseFIAuthenticationToView();
  },
  canUseNonstandardAuthenticationToSign: function () {
    return this.canUseSEAuthenticationToSign() ||
      this.canUseSMSPinAuthenticationToSign() ||
      this.canUseNOAuthenticationToSign() ||
      this.canUseDKAuthenticationToSign();
  },
  parse: function (args) {
     return {
      can_use_templates: args.can_use_templates,
      can_use_branding: args.can_use_branding,
      can_use_author_attachments: args.can_use_author_attachments,
      can_use_signatory_attachments: args.can_use_signatory_attachments,
      can_use_mass_sendout: args.can_use_mass_sendout,
      can_use_sms_invitations: args.can_use_sms_invitations,
      can_use_sms_confirmations: args.can_use_sms_confirmations,
      can_use_dk_authentication_to_view: args.can_use_dk_authentication_to_view,
      can_use_dk_authentication_to_sign: args.can_use_dk_authentication_to_sign,
      can_use_fi_authentication_to_view: args.can_use_fi_authentication_to_view,
      can_use_no_authentication_to_view: args.can_use_no_authentication_to_view,
      can_use_no_authentication_to_sign: args.can_use_no_authentication_to_sign,
      can_use_se_authentication_to_view: args.can_use_se_authentication_to_view,
      can_use_se_authentication_to_sign: args.can_use_se_authentication_to_sign,
      can_use_sms_pin_authentication_to_view: args.can_use_sms_pin_authentication_to_view,
      can_use_sms_pin_authentication_to_sign: args.can_use_sms_pin_authentication_to_sign,
      can_use_standard_authentication_to_view: args.can_use_standard_authentication_to_view,
      can_use_standard_authentication_to_sign: args.can_use_standard_authentication_to_sign,
      can_use_email_invitations: args.can_use_email_invitations,
      can_use_api_invitations: args.can_use_api_invitations,
      can_use_pad_invitations: args.can_use_pad_invitations
    };
  }
});

/* Static methods */
Subscription.initCurrentSubscription = function (subscriptionData, currentUserIsAdmin) {
  window.currentSubscription = new Subscription(_.extend(subscriptionData,
      {ready: true, current_user_is_admin: currentUserIsAdmin}
  ));
};

Subscription.currentSubscription = function () {
  if (_.isUndefined(window.currentSubscription)) {
    var user = new User({});
    user.set({"ready": false}, {silent: true});
    user.fetch({cache: false, processData: true});
    var subscription = new Subscription({current_user_is_admin: user.companyadmin()});
    window.currentSubscription = subscription;
    subscription.reload();
  }
  return window.currentSubscription;
};

module.exports = Subscription;
