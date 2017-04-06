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
      thisDevice: true,
      pollingInterval: 3000,
      activationTime: 0,
      cancelled: false // Flag for when transaction is aborted in frontend - to stop polling
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
    document: function () {
      return this.signatory().document();
    },
    cancelled: function () {
      return this.get("cancelled");
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
    cancel: function () {
      this.set("cancelled", true);
    },
    pollingInterval: function () {
      return this.get("pollingInterval");
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
    isStatusOutstanding: function () {
      return BankIDUtils.isStatusOutstanding(this.status());
    },
    isStatusNoClient: function () {
      return BankIDUtils.isStatusNoClient(this.status());
    },
    isStatusComplete: function () {
      return BankIDUtils.isStatusComplete(this.status());
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
    bankIdUrlWithRedirectIfNeeded: function () {
      if (this.autoStartToken()) {
        if (BrowserInfo.isIpad()
          || BrowserInfo.isIphone()
          || BrowserInfo.isWindowsPhone()
          || BrowserInfo.isAndroid()) {
          // We set redirect only for selected devices (and Android, as Android is advertised to close itself
          // but it"s not working 100% of the time). On other platform BankID app can close itself

          var documentUrl = "null";
          if (window.parent != window) {
            documentUrl = document.referrer;
          } else {
            documentUrl = window.location;
          }

          var returnUrl =  LocationUtils.origin() + "/s/eid/cgi/grp/checkcgiauthstatuswithredirect/" +
            this.document().documentid() + "/" + this.signatory().signatoryid() +
            "?session_id=" + this.sessionID() + "&url=" + encodeURIComponent(documentUrl) +
            "&_=" + Math.random();
          return "bankid:///?autostarttoken=" + this.autoStartToken() + "&redirect=" + encodeURIComponent(returnUrl);
        } else {
          return this.bankIdUrl();
        }
      }
    },
    /*
    * Workflow logic functions
    */
    initiateTransaction: function () {
      var self = this;
      new Submit({
        method: "POST",
        url: "/s/eid/cgi/grp/auth/" + self.document().documentid() +
          "/" + self.signatory().signatoryid(),
        personal_number: BankIDUtils.normalizedPersonalNumber(self.signatory()),
        ajaxsuccess: function (resp, s, xhr) {
          if (resp.auto_start_token && resp.session_id) {
            self.setAutoStartTokenAndSessionID(resp.auto_start_token, resp.session_id);
            self.triggerStatusChange();
            self.pollCollect();
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
    },
    pollCollect: function () {
      var self = this;
      var poller = function () {
        new Submit({
          method: "GET",
          url: "/s/eid/cgi/grp/checkcgiauthstatus/" + self.document().documentid() + "/" +
            self.signatory().signatoryid() + "?_=" + Math.random(),
          ajaxsuccess: function (resp, s, xhr) {
            if (self.cancelled()) {
              return; // If action was cancelled we stop processing any results and we will stop polling
            }
            if (resp.progress_status && BankIDUtils.isProgressStatus(resp.progress_status)) {
              self.setStatus(resp.progress_status);
              self.triggerStatusChange();
              if (self.isStatusComplete()) {
                self.triggerSuccess();
              } else {
                setTimeout(poller, self.pollingInterval());
              }
            } else if (resp.grp_fault && BankIDUtils.isFaultStatus(resp.grp_fault)) {
              self.setStatus(resp.grp_fault);
              self.triggerFail();
            } else {
              self.triggerCriticalError(xhr);
            }
          },
          ajaxerror: function (xhr, textStatus, errorThrown) {
            if (self.cancelled()) {
               return; // If action was cancelled we stop processing any results and we will stop polling
            }
            // Don't throw error on ajax error. Some environments trigger error, when switching from bankid native app.
            setTimeout(poller, self.pollingInterval());
          }
        }).sendAjax();
      };
      // Start polling at collect endpoint
      poller();
    }
  });
