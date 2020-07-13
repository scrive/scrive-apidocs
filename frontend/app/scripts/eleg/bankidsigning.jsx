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
      thisDevice: false,
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
    thisDevice: function () {
      return this.get("thisDevice");
    },
    setAutoStartTokenAndSessionID: function (t, s) {
      this.set({"autoStartToken": t, "sessionID": s, "activationTime": new Date().getTime()});
    },
    autoStartToken: function () {
      return this.get("autoStartToken");
    },
    sessionID: function () {
      return this.get("sessionID");
    },
    setStatus: function (s) {
      // EID Hub returns camelCase status codes instead of snake_case; in order
      // to be able to reuse this module for both EID Hub and CGI we convert all
      // status codes to snake_case.
      if (s) {
        s = s.replace(/[A-Z]/g, c => `_${c.toLowerCase()}`);
      }

      this.set({"status": s}, {silent: true});
      this.trigger("change");
    },
    status: function () {
      return this.get("status");
    },
    /*
    * State reporting
    */
    isWaitingForToken: function () {
      return this.autoStartToken() == undefined;
    },
    isFaultStatus: function () {
      return BankIDUtils.isFaultStatus(this.status());
    },
    statusMessage: function () {
      return BankIDUtils.statusMessage(this.thisDevice(), this.status(), this.signatory());
    },
    bankIdUrl: function () {
      if (this.autoStartToken()) {
        return "bankid:///?autostarttoken=" + this.autoStartToken() + "&redirect=null";
      }
    },
    /*
    * Workflow logic functions
    */
    initiateTransaction: function () {
      var self = this;
      new Submit({
        method: "POST",
        url: (fromTemplate.useEIDHubForSEBankIDSign ? "/eid-service/start/sebankid/sign/" : "/s/eid/cgi/grp/sign/")
          + self.document().documentid()
          + "/"
          + self.signatory().signatoryid(),
        personal_number: BankIDUtils.normalizedPersonalNumber(self.signatory()),
        ajaxsuccess: function (resp, s, xhr) {
          if (resp.auto_start_token && resp.session_id) {
            self.setAutoStartTokenAndSessionID(resp.auto_start_token, resp.session_id);
            self.triggerStatusChange();
            self.triggerInitiated();
          } else if (resp.grp_fault) {
            self.setStatus(resp.grp_fault);
            self.triggerFail();
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
