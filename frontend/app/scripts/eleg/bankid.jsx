/** @jsx React.DOM */

define(['React', 'common/backbone_mixin', 'eleg/bankid_model', 'legacy_code', 'common/button' ], function(React, BackboneMixin, BankIDModel, _legacyCode, Button) {

// TODO add some useful mixpanel events
var BankIDModalContent = React.createClass({
  mixins : [BackboneMixin.BackboneMixin],
  getBackboneModels : function() {
    return [this.props.model];
  },
  propTypes : {
    model : React.PropTypes.object,
  },
  statusMessage : function() {
    var model = this.props.model;
    if(model.isWaitingForStatus()) {
      return model.thisDevice() ? localization.docsignview.eleg.bankid.rfa13
                                : localization.docsignview.eleg.bankid.rfa1;
    }
    else {
      switch(model.status()) {
        case 'no_client':
          return localization.docsignview.eleg.bankid.rfa1;
        case 'already_in_progress':
          return '';
        case 'cancelled':
          return localization.docsignview.eleg.bankid.rfa3;
        case 'retry':
          return '';
        case 'internal_error':
          return localization.docsignview.eleg.bankid.rfa5;
        case 'user_cancel':
          return localization.docsignview.eleg.bankid.rfa6;
        case 'expired_transaction':
          return localization.docsignview.eleg.bankid.rfa8;
        case 'user_sign':
          return localization.docsignview.eleg.bankid.rfa9;
        case 'client_err':
          return localization.docsignview.eleg.bankid.rfa12;
        // If RP tried to start the client automatically, the RP should inform the
        // user that the app is starting. Message RFA13 should be used.
        // If RP did not try to start the client automatically [...] RFA1 should be used.
        case 'outstanding_transaction':
          if(model.thisDevice()) {
            return localization.docsignview.eleg.bankid.rfa13;
          } else {
            return localization.docsignview.eleg.bankid.rfa1;
          }
        case 'started':
          if(BrowserInfo.isSmallScreen()) {
            return localization.docsignview.eleg.bankid.rfa14Mobile;
          } else {
            return localization.docsignview.eleg.bankid.rfa14Computer;
          }
        case 'certificate_err':
          return localization.docsignview.eleg.bankid.rfa16;
        case 'start_failed':
          return localization.docsignview.eleg.bankid.rfa17;
        // RP must not try the same request again. This is an internal error
        // within RP's system and must not be communicated to the user as a BankID-error.
        case 'invalid_parameters':
          return '';
        case 'access_denied_rp':
          ReloadDueToErrorModal({status: model.status()});
          return '';
      }
    }
    return '';
  },
  faultModal : function() {
    LoadingDialog.close();
    var self = this;
    var modal = new Confirmation({
      title : localization.docsignview.eleg.bankid.faultModalTitle,
      cssClass : 'grey sign-eleg-bankid-fault',
      content : self.statusMessage(),
      margin : (($(window).height()- 200) /2)  + "px auto 0",
      closeVisible : false,
      cancelVisible : false,
      onAccept : function() {
        modal.close();
      }
    });
    return true;
  },
  render : function() {
    var model = this.props.model;
    var buttonOnClick = function() {
      window.location.href = model.bankIdUrl();
    };
    return (
      <div style={{textAlign: "center"}}>
        {/*if*/       model.isFaultStatus() &&
          this.faultModal()
        }
        {/*else if*/ !model.isFaultStatus() &&  model.isWaitingForToken()}
        {/*else*/    !model.isFaultStatus() && !model.isWaitingForToken() &&
          <div>
            {/*if*/ model.thisDevice() &&
              <div>
                <div><small> You BankdID application should automatically open. If it does not click button bellow</small></div>
                <Button color="green" size="tiny" text={localization.docsignview.eleg.bankid.tryToOpenAgain} onClick={function() {window.location = model.bankIdUrl();}}/>
                <iframe src={model.bankIdUrl()} width='0' height='0'></iframe>
              </div>
            }
            {/*if*/ !model.thisDevice() &&
              <div>
                <div><small>Check you Mobile BankID application to continue signing</small></div>
              </div>
            }
            {/*if*/  !model.isStatusComplete() &&
              <p>{this.statusMessage()}</p>
            }
          </div>
        }
      </div>
    );
  }
});

var updateLoadingDialogWithBankIDStatus = function(model) {
  var div = $("<div style='text-align:center'/>");
  if (!model.isFaultStatus() && !model.isWaitingForToken()) {
    div.append($("<p/>").html(model.statusMessage()));
    if (model.thisDevice() && model.isStatusOutstanding()) {
      div.append($("<a class='button button-green button-tiny'/>").text("Open BankID application").click(function() {window.location = model.bankIdUrl();}));
    }
  }
  LoadingDialog.open(div);
};

var addBankIDIframeIfItsNeeded = function(model) {
  if (!model.isFaultStatus() && !model.isWaitingForToken() && model.thisDevice() && ($("#bankid-"+ model.autoStartToken()).size() == 0)) {
        $("body").append($("<iframe width='0' height='0' class='bankid-iframe'/>").attr('id','bankid-'+ model.autoStartToken()).attr('src',model.bankIdUrl()));
  }
}

var clearBankIDIframes = function() {
    $("iframe.bankid-iframe").remove();
}
return function(args) {
  var model = new BankIDModel({
    signatory : args.signatory,
    onStatusChange : function() {
      updateLoadingDialogWithBankIDStatus(model);
      addBankIDIframeIfItsNeeded(model);
    },
    onSuccess  : function() {
      LoadingDialog.close();
      clearBankIDIframes();
      args.onSuccess();
    },
    onFail: function() {
      LoadingDialog.close();
      clearBankIDIframes();
      var modal = new Confirmation({
        title : localization.docsignview.eleg.bankid.faultModalTitle,
        cssClass : 'grey sign-eleg-bankid-fault',
        content : model.statusMessage(),
        margin : (($(window).height()- 200) /2)  + "px auto 0",
        closeVisible : false,
        cancelVisible : false,
        onAccept : function() {
          modal.close();
        }
      });
      return true;
    },
    onCriticalError : function() {
      clearBankIDIframes();
      new ReloadDueToErrorModal(xhr);
    },
    thisDevice : args.thisDevice
  });
  model.initiateTransaction();
  updateLoadingDialogWithBankIDStatus(model);

};

});
