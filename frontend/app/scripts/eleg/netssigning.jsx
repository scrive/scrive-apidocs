var Backbone = require("backbone");
var LocationUtils = require("../common/location");
var BankIDUtils = require("./bankidutils");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var Submit = require("../../js/submits.js").Submit;

module.exports = Backbone.Model.extend({
    defaults: {
      signatory: undefined,
      onStatusChange: function () {},
      onSuccess: function () {},
      onFail: function () {},
      onCriticalError: function () {},
      onInitiated: function () {},
      activationTime: 0
    },
    triggerStatusChange: function () {
      this.get("onStatusChange")();
    },
    triggerSuccess: function () {
      this.get("onSuccess")();
    },
    triggerFail: function () {
      this.get("onFail")();
    },
    triggerCriticalError: function (xhr) {
      this.get("onCriticalError")(xhr);
    },
    triggerInitiated: function () {
      this.get("onInitiated")();
    },
    document: function () {
      return this.signatory().document();
    },
    activeForAtLeast5Sec: function () {
      // Activation time is initiated when we get autostart token.
      return new Date().getTime() - this.get("activationTime") > 5000;
    },
    signatory: function () {
      return this.get("signatory");
    },
    setSignUrl: function (signUrl) {
      this.set({"signUrl": signUrl, "activationTime": new Date().getTime()});
    },
    signUrl: function () {
      return this.get("signUrl");
    },
    autoStartToken: function () {
      return this.get("autoStartToken");
    },
    sessionID: function () {
      return this.get("sessionID");
    },
    setStatus: function (s) {
      this.set({"status": s}, {silent: true});
      this.trigger("change");
    },
    status: function () {
      return this.get("status");
    },
    thisDevice: function () {
      return true;
    },
    bankIdUrl: function () {
      return this.signUrl();
    },
    /*
    * State reporting
    */
    isWaitingForToken: function () {
      return this.autoStartToken() == undefined;
    },
    isFaultStatus: function () {
      var status = this.status();
      return status == "nets_error_expired_transaction"
        || status == "nets_error_api"
        || status == "nets_error_canceled_by_merchant"
        || status == "nets_error_expired"
        || status == "nets_error_expired_by_proxy"
        || status == "nets_error_rejected_by_signer";
    },
    statusMessage: function () {
      var status = this.status();
      if (status == "nets_error_expired_transaction") {
        return localization.docsignview.eleg.nets.errorExpiredTransaction;
      } else if (status == "nets_error_api") {
        return localization.docsignview.eleg.nets.errorAPI;
      } else if (status == "nets_error_canceled_by_merchant") {
        return localization.docsignview.eleg.nets.errorCanceledByMerchant;
      } else if (status == "nets_error_expired") {
        return localization.docsignview.eleg.nets.errorExpired;
      } else if (status == "nets_error_expired_by_proxy") {
        return localization.docsignview.eleg.nets.errorExpiredByProxy;
      } else if (status == "nets_error_rejected_by_signer") {
        return localization.docsignview.eleg.nets.errorRejectedBySigner;
      // This is not and error state and should not ever be used, but we add it
      // for sake of completeness
      } else if (status == "nets_in_progress") {
        return localization.docsignview.eleg.nets.transactionInProgress;
      } else {
        return localization.docsignview.eleg.nets.errorGeneric;
      }
    },
    /*
    * Workflow logic functions
    */
    initiateTransaction: function (eidMethod) {
      var self = this;
      new Submit({
        method: "POST",
        url: "/nets/sign/" + self.document().documentid() + "/" + self.signatory().signatoryid(),
        personal_number: self.signatory().personalnumber(),
        eid_method: eidMethod,
        ajaxsuccess: function (resp, s, xhr) {
          if (resp.nets_sign_url) {
            self.setSignUrl(resp.nets_sign_url);
            self.triggerStatusChange();
            self.triggerInitiated();
          } else {
            self.triggerCriticalError(xhr);
          }
        },
        ajaxerror: function (xhr, textStatus, errorThrown) {
          self.triggerCriticalError(xhr);
        }
      }).sendAjax();
    }
  });
