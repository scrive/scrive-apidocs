define(['Backbone', 'legacy_code'], function(Backbone) {

return Backbone.Model.extend({
    defaults : {
      signatory : undefined,
      onStatusChange : function() {},
      onSuccess : function() {},
      onFail : function() {},
      onCriticalError : function() {},
      thisDevice : true,
      pollingInterval : 3000
    },
    triggerStatusChange : function() {
      this.get("onStatusChange")();
    },
    triggerSuccess : function()  {
      this.get("onSuccess")();
    },
    triggerFail: function(){
      this.get("onFail")();
    },
    triggerCriticalError : function(xhr) {
      this.get("onCriticalError")(xhr);
    },
    document : function() {
      return this.signatory().document();
    },
    signatory : function() {
      return this.get('signatory');
    },
    thisDevice : function() {
      return this.get('thisDevice');
    },
    pollingInterval : function() {
      return this.get('pollingInterval');
    },
    setAutoStartToken : function(t) {
      this.set({'autoStartToken' : t});
    },
    autoStartToken : function() {
      return this.get('autoStartToken');
    },
    setStatus : function(s) {
      this.set({'status' : s}, {silent : true});
      this.trigger('change');
    },
    status : function() {
      return this.get('status');
    },
    /*
    * State reporting
    */
    isWaitingForToken : function() {
      return this.autoStartToken() == undefined;
    },
    isWaitingForStatus : function() {
      return this.status() == undefined;
    },
    isProgressStatus : function() {
      return this.status() != undefined
          && this.checkIsProgressStatus(this.status());
    },
    checkIsProgressStatus : function(str) {
      return str == 'complete'
          || str == 'user_sign'
          || str == 'outstanding_transaction'
          || str == 'no_client'
          || str == 'started';
    },
    isStatusOutstanding : function() {
      return this.status() != undefined
          && this.status() == 'outstanding_transaction';
    },
    isStatusUserSign : function() {
      return this.status() != undefined
          && this.status() == 'user_sign';
    },
    isStatusNoClient : function() {
      return this.status() != undefined
          && this.status() == 'no_client';
    },
    isStatusStarted : function() {
      return this.status() != undefined
          && this.status() == 'started';
    },
    isStatusComplete : function() {
      return this.status() != undefined
          && this.status() == 'complete';
    },
    isFaultStatus : function() {
      return this.status() != undefined
          && this.checkIsFaultStatus(this.status());
    },
    checkIsFaultStatus : function(str) {
      return str == 'invalid_parameters'
          || str == 'already_in_progress'
          || str == 'access_denied_rp'
          || str == 'retry'
          || str == 'internal_error'
          || str == 'expired_transaction'
          || str == 'user_cancel'
          || str == 'client_err'
          || str == 'certificate_err'
          || str == 'cancelled'
          || str == 'start_failed';
    },
    canChangeSSN : function() {
      return this.signatory().personalnumberField().isClosed();
    },
    statusMessage : function() {
      var self = this;
      if(self.isWaitingForStatus() && self.thisDevice()) {
        return localization.docsignview.eleg.bankid.rfa13;
      } else if (self.isWaitingForStatus() && !self.thisDevice()) {
        return  localization.docsignview.eleg.bankid.rfa1;
      } else if (self.status() == 'no_client') {
        return localization.docsignview.eleg.bankid.rfa1;
      } else if (self.status() == 'already_in_progress') {
        return localization.docsignview.eleg.bankid.alreadyInProgress;
      } else if (self.status() == 'cancelled') {
        return localization.docsignview.eleg.bankid.rfa3;
      } else if (self.status() == 'retry') {
        return localization.docsignview.eleg.bankid.retry;
      } else if (self.status() == 'internal_error') {
        return localization.docsignview.eleg.bankid.rfa5;
      } else if (self.status() == 'user_cancel') {
        return localization.docsignview.eleg.bankid.rfa6;
      } else if (self.status() == 'expired_transaction') {
        return localization.docsignview.eleg.bankid.rfa8;
      } else if (self.status() == 'user_sign') {
        return localization.docsignview.eleg.bankid.rfa9;
      } else if (self.status() == 'client_err') {
        return localization.docsignview.eleg.bankid.rfa12;
        // If RP tried to start the client automatically, the RP should inform the
        // user that the app is starting. Message RFA13 should be used.
        // If RP did not try to start the client automatically [...] RFA1 should be used.
      } else if (self.status() == 'outstanding_transaction' && self.thisDevice()) {
        return localization.docsignview.eleg.bankid.rfa13;
      } else if (self.status() == 'outstanding_transaction' && !self.thisDevice()){
        return localization.docsignview.eleg.bankid.rfa1;
      } else if (self.status() == 'started' && BrowserInfo.isSmallScreen()) {
        return localization.docsignview.eleg.bankid.rfa14Mobile;
      } else if (self.status() == 'started' && !BrowserInfo.isSmallScreen()){
        return localization.docsignview.eleg.bankid.rfa14Computer;
      } else if (self.status() == 'certificate_err') {
        return localization.docsignview.eleg.bankid.rfa16;
      } else if (self.status() == 'start_failed') {
        return localization.docsignview.eleg.bankid.rfa17;
        // RP must not try the same request again. This is an internal error
        // within RP's system and must not be communicated to the user as a BankID-error.
      } else if (self.status() == 'invalid_parameters' && self.canChangeSSN()) {
        return localization.docsignview.eleg.bankid.invalidParametersCanChange;
      } else if (self.status() == 'invalid_parameters' && !self.canChangeSSN()) {
        return localization.docsignview.eleg.bankid.invalidParametersCantChange;
      } else if (self.status() == 'access_denied_rp') {
        return localization.docsignview.eleg.bankid.accessDenied;
      } else {
        return '';
      }
    },
    /*
    * Data Constructor methods
    */
    bankIdUrl : function() {
      if(this.autoStartToken()) {
        return 'bankid:///?autostarttoken=' + this.autoStartToken();
      }
    },
    /*
    * Workflow logic functions
    */
    initiateTransaction : function() {
      var self = this;
      new Submit({
        method : 'POST',
        url  : "/s/eid/cgi/grp/sign/" + self.document().documentid() + "/" + self.signatory().signatoryid(),
        personal_number : self.signatory().personalnumber(),
        ajaxsuccess : function(d, s, xhr) {
          var resp = JSON.parse(d);
          if(resp.auto_start_token) {
            self.setAutoStartToken(resp.auto_start_token);
            self.triggerStatusChange();
            self.pollCollect();
          }
          else if(resp.grp_fault) {
            self.setStatus(resp.grp_fault);
            self.triggerFail();
          }
          else {
            self.triggerCriticalError(xhr);
          }
        },
        ajaxerror : function(xhr, textStatus, errorThrown) {
          self.triggerCriticalError(xhr);
        }
      }).sendAjax();
    },
    pollCollect : function() {
      var self = this;
      var poller = function() {
        new Submit({
          method : 'GET',
          url : "/s/eid/cgi/grp/collect/" + self.document().documentid() + "/" + self.signatory().signatoryid(),
          ajaxsuccess : function(d, s, xhr) {
            var resp = JSON.parse(d);
            if(resp.progress_status && self.checkIsProgressStatus(resp.progress_status)) {
              self.setStatus(resp.progress_status);
              self.triggerStatusChange();
              if(self.isStatusComplete()) {
                self.triggerSuccess();
              }
              else {
                setTimeout(poller, self.pollingInterval());
              }
            } else if(resp.grp_fault && self.checkIsFaultStatus(resp.grp_fault)) {
              self.setStatus(resp.grp_fault);
              self.triggerFail();
            }
            else {
              self.triggerCriticalError(xhr);
            }
          },
          ajaxerror : function(xhr, textStatus, errorThrown) {
            self.triggerCriticalError(xhr);
          }
        }).sendAjax();
      };
      // Start polling at collect endpoint
      poller();
    }
  });

});
