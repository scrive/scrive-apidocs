define(['Backbone', 'legacy_code'], function(Backbone) {

// TODO add some useful mixpanel events
var BankIDModel = Backbone.Model.extend({
  defaults : {
    signatory : undefined,
    callback : function() {},
    errorcallback : function() {},
    thisDevice : true,
    pollingInterval : 3000
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
  /*
   * Data Constructor methods
   */
  bankIdUrl : function() {
    if(this.autoStartToken()) {
      return 'bankid:///?autostarttoken='
           + this.autoStartToken()
           + '&redirect=null';
    }
  },
  /*
   * Workflow logic functions
   */
  initiateTransaction : function() {
    var self = this;
    var url = "/s/eid/cgi/grp/sign/" + self.document().documentid() + "/" + self.signatory().signatoryid();

    new Submit({
      method : 'POST',
      url  : url,
      personal_number : self.signatory().personalnumber(),
      ajaxsuccess : function(d, s, xhr) {
        var resp = $.parseJSON(d);
        if(resp.auto_start_token) {
          console.log('/s/eid/.../sign/: ' + resp.auto_start_token);
          self.setAutoStartToken(resp.auto_start_token);
          self.pollCollect();
        }
        else if(resp.grp_fault) {
          console.log('/s/eid/.../sign/: ' + resp.grp_fault);
          self.setStatus(resp.grp_fault);
        }
        else {
          console.log("Didn't get well formed auto_start_token or grp_fault");
          console.log(resp);
          new ReloadDueToErrorModal(xhr);
        }
      },
      ajaxerror : function(xhr, textStatus, errorThrown) {
        console.log(textStatus);
        console.log(errorThrown);
        new ReloadDueToErrorModal(xhr);
      }
    }).sendAjax();
  },
  pollCollect : function() {
    var self = this;
    var url = "/s/eid/cgi/grp/collect/" + self.document().documentid() + "/" + self.signatory().signatoryid();
    var poller = function() {
      new Submit({
        method : 'GET',
        url : url,
        ajaxsuccess : function(d, s, xhr) {
          var resp = $.parseJSON(d);
          if(resp.progress_status && self.checkIsProgressStatus(resp.progress_status)) {
            console.log('/s/eid/.../collect/: ' + resp.progress_status); // FIXME TEMP
            self.setStatus(resp.progress_status);
            if(self.isStatusComplete()) {
              self.runCallback();
            }
            else {
              setTimeout(poller, self.pollingInterval());
            }
          } else if(resp.grp_fault && self.checkIsFaultStatus(resp.grp_fault)) {
            console.log('/s/eid/.../collect/: ' + resp.grp_fault); // FIXME TEMP
            self.setStatus(resp.grp_fault);
          }
          else {
            console.log("Got neither valid progress_status nor grp_fault");
            console.log(resp);
            new ReloadDueToErrorModal(xhr);
          }
        },
        ajaxerror : function(xhr, textStatus, errorThrown) {
          console.log(textStatus);
          console.log(errorThrown);
          new ReloadDueToErrorModal(xhr);
        }
      }).sendAjax();
    };
    // Start polling at collect endpoint
    poller();
  },
  runCallback : function() {
    if(!this.isWaitingForStatus() && this.isStatusComplete()) {
      this.get('callback')();
    } else {
      this.get('errorcallback')();
    }
  }
});

return BankIDModel;

});
