var Backbone = require("backbone");
var _ = require("underscore");
var Submit = require("../../js/submits.js").Submit;
var User = require("../../js/account/user.js").User;

/* Main archive definition. Its a tab based set of different documents lists. */

var Subscription = Backbone.Model.extend({
  defaults: {
    "invoicing_type": "none",
    "payment_plan": "free",
    "inherited_plan": "",
    "number_of_users": 0,
    "free_document_tokens": 0,
    "free_document_tokens_valid_till": new Date(),
    "features": undefined,
    "inherited_features": undefined,
    "features_is_inherited": false,
    "current_user_is_admin": undefined,
    "ready": false
  },
  initialize: function (args) {
    if (args != undefined && args.forAdmin && args.companyid != undefined)
      this.url = "/adminonly/companyadmin/getsubscription/" + args.companyid;
    else
      this.url = "/api/frontend/getsubscription";

    if (args && args.features) {
      var adminFF = new FeatureFlag(args.features.admin_users);
      var regularFF = new FeatureFlag(args.features.regular_users);
      this.set({"features": new Features({
        "admin_users": adminFF,
        "regular_users": regularFF
      })});
    } else {
      this.set({"features": this.noFeatures()});
    }

    this.set({"features_is_inherited": !!(args && args.features_is_inherited)});

    if (args && args.inherited_features) {
      var inheritedAdminFF = new FeatureFlag(args.inherited_features.admin_users);
      var inheritedRegularFF = new FeatureFlag(args.inherited_features.regular_users);
      this.set({"inherited_features": new Features({
        "admin_users": inheritedAdminFF,
        "regular_users": inheritedRegularFF
      })});
    }
    // keep inherited features as undefined, if they are not available

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
  effectivepaymentplan: function () {
     return (this.paymentplan() == "inherit" ? this.inheritedplan() : this.paymentplan());
  },
  invoicingtype: function () {
     return this.get("invoicing_type");
  },
  inheritedplan: function () {
     return this.get("inherited_plan");
  },
  hasFreePlan: function () {
     return this.effectivepaymentplan() == "free";
  },
  hasOnePlan: function () {
     return this.effectivepaymentplan() == "one";
  },
  hasTeamPlan: function () {
     return this.effectivepaymentplan() == "team";
  },
  hasEnterprisePlan: function () {
     return this.effectivepaymentplan() == "enterprise";
  },
  hasTrialPlan: function () {
     return this.effectivepaymentplan() == "trial";
  },
  numberOfUsers: function () {
     return this.get("number_of_users");
  },
  freeDocumentTokens: function () {
     return this.get("free_document_tokens");
  },
  freeDocumentTokensValidTill: function () {
     return this.get("free_document_tokens_valid_till");
  },
  currentUserIsAdmin: function () {
     return this.get("current_user_is_admin");
  },

  noFeatureFlags: function () {
    return new FeatureFlag({});
  },

  noFeatures: function () {
    return new Features({
      "regular_users": this.noFeatureFlags(),
      "admin_users": this.noFeatureFlags()
    });
  },

  features: function () {
    return this.get("features") || this.noFeatures();
  },

  inheritedFeatures: function () {
    return this.get("inherited_features");
  },

  featuresIsInherited: function () {
    return this.get("features_is_inherited") || false;
  },

  // actually - current use feature flags
  currentUserFeatures: function () {
     var isAdmin = this.currentUserIsAdmin();
     var currentFeatures = undefined;
     if (!this.featuresIsInherited()) {
       currentFeatures = this.features();
     } else if (this.inheritedFeatures()) {
       currentFeatures = this.inheritedFeatures();
     } else {
       // defensive programming. This really should not happen.
       console.log("Inherited features are missing!");
       currentFeatures = this.noFeatures();
     }
     if (isAdmin != undefined && isAdmin) {
       return currentFeatures.adminUsers();
     } else if (isAdmin != undefined && !isAdmin) {
       return currentFeatures.regularUsers();
     } else {
       return this.noFeatureFlags();
     }
  },
  isOverLimit: function (numberOfDocs) {
    if (numberOfDocs === undefined) {
      numberOfDocs = 1;
    }
    return this.hasFreePlan() && this.freeDocumentTokens() < numberOfDocs;
  },
  updateSubscriptionAsAdmin: function (nsd, callback) {
    var self = this;
    var fromFeatureFlags = function (ff) {
      return ({
        can_use_templates: ff.canUseTemplates,
        can_use_shareable_links: ff.canUseShareableLinks,
        can_use_branding: ff.canUseBranding,
        can_use_author_attachments: ff.canUseAuthorAttachments,
        can_use_signatory_attachments: ff.canUseSignatoryAttachments,
        can_use_mass_sendout: ff.canUseMassSendout,
        can_use_sms_invitations: ff.canUseSMSInvitations,
        can_use_sms_confirmations: ff.canUseSMSConfirmations,
        can_use_dk_cpr_authentication_to_view: ff.canUseDKCPRAuthenticationToView,
        can_use_dk_pid_authentication_to_view: ff.canUseDKPIDAuthenticationToView,
        can_use_dk_cvr_authentication_to_view: ff.canUseDKCVRAuthenticationToView,
        can_use_dk_authentication_to_sign: ff.canUseDKAuthenticationToSign,
        can_use_fi_authentication_to_view: ff.canUseFIAuthenticationToView,
        can_use_fi_authentication_to_sign: ff.canUseFIAuthenticationToSign,
        can_use_no_authentication_to_view: ff.canUseNOAuthenticationToView,
        can_use_no_authentication_to_sign: ff.canUseNOAuthenticationToSign,
        can_use_se_authentication_to_view: ff.canUseSEAuthenticationToView,
        can_use_se_authentication_to_sign: ff.canUseSEAuthenticationToSign,
        can_use_sms_pin_authentication_to_view: ff.canUseSMSPinAuthenticationToView,
        can_use_sms_pin_authentication_to_sign: ff.canUseSMSPinAuthenticationToSign,
        can_use_standard_authentication_to_view: ff.canUseStandardAuthenticationToView,
        can_use_standard_authentication_to_sign: ff.canUseStandardAuthenticationToSign,
        can_use_verimi_authentication_to_view: ff.canUseVerimiAuthenticationToView,
        can_use_idin_authentication_to_view: ff.canUseIDINAuthenticationToView,
        can_use_idin_authentication_to_sign: ff.canUseIDINAuthenticationToSign,
        can_use_email_invitations: ff.canUseEmailInvitations,
        can_use_email_confirmations: ff.canUseEmailConfirmations,
        can_use_api_invitations: ff.canUseAPIInvitations,
        can_use_pad_invitations: ff.canUsePadInvitations,
        can_use_forwarding: ff.canUseForwarding,
        can_use_document_party_notifications: ff.canUseDocumentPartyNotifications,
        can_use_portal: ff.canUsePortal,
        can_use_custom_sms_texts: ff.canUseCustomSMSTexts,
        can_use_onfido_authentication_to_sign: ff.canUseOnfidoAuthenticationToSign
      });
    };
    var newSubscription = {
      invoicing_type: nsd.selectedInvoicingType,
      features_is_inherited: nsd.featuresIsInherited,
      features: {
        admin_users: fromFeatureFlags(nsd.features.adminUsers),
        regular_users: fromFeatureFlags(nsd.features.regularUsers)
      }
    };

    if (nsd.selectedPlan !== "inherit") {
      newSubscription["payment_plan"] = nsd.selectedPlan;
    }

    if (nsd.selectedPlan == "free") {
      newSubscription["free_document_tokens"] = nsd.freeDocumentTokens;
      newSubscription["free_document_tokens_valid_till"] = nsd.freeDocumentTokensValidTill;
    }

    new Submit({
      method: "POST",
      url: "/adminonly/companyadmin/updatesubscription/" + this.companyid(),
      subscription: JSON.stringify(newSubscription),
      ajaxsuccess: callback
    }).sendAjax();
  },
  parse: function (args) {
    var features = undefined;
    var featuresIsInherited = args.features_is_inherited;
    var inheritedFeatures = undefined;
    if (args.features) {
      features = new Features({
        "admin_users": new FeatureFlag(args.features.admin_users),
        "regular_users": new FeatureFlag(args.features.regular_users)
      });
    } else {
      features = new Features({
        "admin_users": new FeatureFlag({}),
        "regular_users": new FeatureFlag({})
      });
    }
    if (args.inherited_features) {
      inheritedFeatures = new Features({
        "admin_users": new FeatureFlag(args.inherited_features.admin_users),
        "regular_users": new FeatureFlag(args.inherited_features.regular_users)
      });
    } // no else. If there are no inherited features, they stay undefined.
    return {
      invoicing_type: args.invoicing_type,
      inherited_plan: args.inherited_plan,
      payment_plan: args.payment_plan || "inherit",
      number_of_users: args.number_of_users,
      valid_free_document_tokens: args.valid_free_document_tokens,
      features: features,
      inherited_features: inheritedFeatures,
      features_is_inherited: featuresIsInherited,
      free_document_tokens: args.free_document_tokens,
      free_document_tokens_valid_till: new Date(args.free_document_tokens_valid_till),
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
    if (args) {
      return {
        admin_users: new FeatureFlag(args.admin_users),
        regular_users: new FeatureFlag(args.regular_users)
      };
    } else {
      return {
        admin_users: new FeatureFlag({}),
        regular_users: new FeatureFlag({})
      };
    }
  }
});

var FeatureFlag = exports.FeatureFlag = Backbone.Model.extend({
  defaults: {
    "can_use_templates": true,
    "can_use_shareable_links": true,
    "can_use_branding": true,
    "can_use_author_attachments": true,
    "can_use_signatory_attachments": true,
    "can_use_mass_sendout": true,
    "can_use_sms_invitations": true,
    "can_use_sms_confirmations": true,
    "can_use_dk_cpr_authentication_to_view": true,
    "can_use_dk_pid_authentication_to_view": true,
    "can_use_dk_cvr_authentication_to_view": true,
    "can_use_dk_authentication_to_sign": true,
    "can_use_fi_authentication_to_view": true,
    "can_use_fi_authentication_to_sign": true,
    "can_use_no_authentication_to_view": true,
    "can_use_no_authentication_to_sign": true,
    "can_use_se_authentication_to_view": true,
    "can_use_se_authentication_to_sign": true,
    "can_use_sms_pin_authentication_to_view": true,
    "can_use_sms_pin_authentication_to_sign": true,
    "can_use_standard_authentication_to_view": true,
    "can_use_standard_authentication_to_sign": true,
    "can_use_verimi_authentication_to_view": true,
    "can_use_idin_authentication_to_view": true,
    "can_use_idin_authentication_to_sign": true,
    "can_use_email_invitations": true,
    "can_use_email_confirmations": true,
    "can_use_api_invitations": true,
    "can_use_pad_invitations": true,
    "can_use_document_party_notifications": false,
    "can_use_portal": false,
    "canUseCustomSMSTexts": false,
    "can_use_onfido_authentication_to_sign": true
  },
  canUseTemplates: function () {
     return this.get("can_use_templates");
  },
  canUseShareableLinks: function () {
     return this.get("can_use_shareable_links");
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
  canUseDKCPRAuthenticationToView: function () {
     return this.get("can_use_dk_cpr_authentication_to_view");
  },
  canUseDKPIDAuthenticationToView: function () {
     return this.get("can_use_dk_pid_authentication_to_view");
  },
  canUseDKCVRAuthenticationToView: function () {
     return this.get("can_use_dk_cvr_authentication_to_view");
  },
  canUseDKAuthenticationToSign: function () {
     return this.get("can_use_dk_authentication_to_sign");
  },
  canUseFIAuthenticationToView: function () {
     return this.get("can_use_fi_authentication_to_view");
  },
  canUseFIAuthenticationToSign: function () {
     return this.get("can_use_fi_authentication_to_sign");
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
  canUseCustomSMSTexts: function () {
     return this.get("can_use_custom_sms_texts");
  },
  canUseStandardAuthenticationToView: function () {
     return this.get("can_use_standard_authentication_to_view");
  },
  canUseStandardAuthenticationToSign: function () {
     return this.get("can_use_standard_authentication_to_sign");
  },
  canUseVerimiAuthenticationToView: function () {
     return this.get("can_use_verimi_authentication_to_view");
  },
  canUseIDINAuthenticationToView: function () {
     return this.get("can_use_idin_authentication_to_view");
  },
  canUseIDINAuthenticationToSign: function () {
     return this.get("can_use_idin_authentication_to_sign");
  },
  canUseEmailInvitations: function () {
     return this.get("can_use_email_invitations");
  },
  canUseEmailConfirmations: function () {
     return this.get("can_use_email_confirmations");
  },
  canUseAPIInvitations: function () {
     return this.get("can_use_api_invitations");
  },
  canUsePadInvitations: function () {
     return this.get("can_use_pad_invitations");
  },
  canUseForwarding: function () {
     return this.get("can_use_forwarding");
  },
  canUseDocumentPartyNotifications: function () {
     return this.get("can_use_document_party_notifications");
  },
  canUsePortal: function () {
     return this.get("can_use_portal");
  },
  canUseOnfidoAuthenticationToSign: function () {
     return this.get("can_use_onfido_authentication_to_sign");
  },
  firstAllowedAuthenticationToView: function () {
    if (this.canUseStandardAuthenticationToView())
      return "standard";
    else if (this.canUseSEAuthenticationToView())
      return "se_bankid";
    else if (this.canUseNOAuthenticationToView())
      return "no_bankid";
    else if (this.canUseDKCPRAuthenticationToView())
      return "dk_nemid_cpr";
    else if (this.canUseDKPIDAuthenticationToView())
      return "dk_nemid_pid";
    else if (this.canUseDKCVRAuthenticationToView())
      return "dk_nemid_cvr";
    else if (this.canUseFIAuthenticationToView())
      return "fi_tupas";
    else if (this.canUseSMSPinAuthenticationToView())
      return "sms_pin";
    else if (this.canUseVerimiAuthenticationToView())
      return "verimi";
    else if (this.canUseIDINAuthenticationToView())
      return "nl_idin";
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
    else if (this.canUseIDINAuthenticationToSign())
      return "nl_idin";
    else if (this.canUseFIAuthenticationToSign())
      return "fi_tupas";
    else if (this.canUseOnfidoAuthenticationToSign())
      return "onfido_document_check";  // we could also use "onfido_document_and_photo_check"
    else
      // Should not happen, just in case
      return "standard";
  },
  firstAllowedInvitationDelivery: function () {
    if (this.canUseEmailInvitations())
      return "email";
    else if (this.canUseSMSInvitations())
      return "mobile";
    else if (this.canUseAPIInvitations())
      return "api";
    else if (this.canUsePadInvitations())
      return "pad";
    else
      // Should not happen, just in case
      return "email";
  },
  firstAllowedConfirmationDelivery: function () {
    if (this.canUseEmailConfirmations())
      return "email";
    else if (this.canUseSMSConfirmations())
      return "mobile";
    else
      return "none";
  },
  canUseNonstandardAuthenticationToView: function () {
    return this.canUseDKCPRAuthenticationToView() ||
      this.canUseDKCPRAuthenticationToView() ||
      this.canUseDKPIDAuthenticationToView() ||
      this.canUseDKCVRAuthenticationToView() ||
      this.canUseNOAuthenticationToView() ||
      this.canUseSEAuthenticationToView() ||
      this.canUseFIAuthenticationToView() ||
      this.canUseVerimiAuthenticationToView() ||
      this.canUseIDINAuthenticationToView();
  },
  canUseNonstandardAuthenticationToSign: function () {
    return this.canUseSEAuthenticationToSign() ||
      this.canUseSMSPinAuthenticationToSign() ||
      this.canUseNOAuthenticationToSign() ||
      this.canUseDKAuthenticationToSign() ||
      this.canUseIDINAuthenticationToSign() ||
      this.canUseFIAuthenticationToSign() ||
      this.canUseOnfidoAuthenticationToSign();
  },
  parse: function (args) {
    return {
      can_use_templates: args.can_use_templates,
      can_use_shareable_links: args.can_use_shareable_links,
      can_use_branding: args.can_use_branding,
      can_use_author_attachments: args.can_use_author_attachments,
      can_use_signatory_attachments: args.can_use_signatory_attachments,
      can_use_mass_sendout: args.can_use_mass_sendout,
      can_use_sms_invitations: args.can_use_sms_invitations,
      can_use_sms_confirmations: args.can_use_sms_confirmations,
      can_use_dk_cpr_authentication_to_view: args.can_use_dk_cpr_authentication_to_view,
      can_use_dk_pid_authentication_to_view: args.can_use_dk_pid_authentication_to_view,
      can_use_dk_authentication_to_sign: args.can_use_dk_authentication_to_sign,
      can_use_fi_authentication_to_view: args.can_use_fi_authentication_to_view,
      can_use_fi_authentication_to_sign: args.can_use_fi_authentication_to_sign,
      can_use_no_authentication_to_view: args.can_use_no_authentication_to_view,
      can_use_no_authentication_to_sign: args.can_use_no_authentication_to_sign,
      can_use_se_authentication_to_view: args.can_use_se_authentication_to_view,
      can_use_se_authentication_to_sign: args.can_use_se_authentication_to_sign,
      can_use_sms_pin_authentication_to_view: args.can_use_sms_pin_authentication_to_view,
      can_use_sms_pin_authentication_to_sign: args.can_use_sms_pin_authentication_to_sign,
      can_use_standard_authentication_to_view: args.can_use_standard_authentication_to_view,
      can_use_standard_authentication_to_sign: args.can_use_standard_authentication_to_sign,
      can_use_verimi_authentication_to_view: args.can_use_verimi_authentication_to_view,
      can_use_idin_authentication_to_view: args.can_use_idin_authentication_to_view,
      can_use_idin_authentication_to_sign: args.can_use_idin_authentication_to_sign,
      can_use_email_invitations: args.can_use_email_invitations,
      can_use_email_confirmations: args.can_use_email_confirmations,
      can_use_api_invitations: args.can_use_api_invitations,
      can_use_pad_invitations: args.can_use_pad_invitations,
      can_use_portal: args.can_use_portal,
      can_use_custom_sms_texts: args.can_use_custom_sms_texts,
      can_use_onfido_authentication_to_sign: args.can_use_onfido_authentication_to_sign
    };
  }
});

/* Static methods */
Subscription.initCurrentSubscription = function (subscriptionData, currentUserIsAdmin) {
  subscriptionData.payment_plan = subscriptionData.payment_plan || "inherit";
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
