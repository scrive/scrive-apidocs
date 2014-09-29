/** @jsx React.DOM */

define(['eleg/bankidsigning', 'legacy_code'], function(BankIDSigning, _legacyCode) {

var updateLoadingDialogWithBankIDStatus = function(bankID) {
  var div = $("<div/>");
  if (!bankID.isFaultStatus() && !bankID.isWaitingForToken()) {
    div.append($("<p/>").html(bankID.statusMessage()));
    if (bankID.thisDevice() && (bankID.isStatusOutstanding() || bankID.isStatusNoClient()) && bankID.activeForAtLeast5Sec()) {
      div.append($("<a class='button button-green button-tiny'/>").text(localization.docsignview.eleg.bankid.rfa18).click(function() {
        window.location = bankID.bankIdUrl();
      }));
    }
  }
  LoadingDialog.open({header: div});
};

var addBankIDIframeIfItsNeeded = function(bankID) {
  if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice() && ($("#bankid-"+ bankID.autoStartToken()).size() == 0)) {
    $("body").append($("<iframe width='0' height='0' class='bankid-iframe'/>").attr('id','bankid-'+ bankID.autoStartToken()).attr('src',bankID.bankIdUrl()));
  }
};

var clearBankIDIframes = function() {
    $("iframe.bankid-iframe").remove();
};

return function(args) {
  var bankID = new BankIDSigning({
    signatory : args.signatory,
    onStatusChange : function() {
      updateLoadingDialogWithBankIDStatus(bankID);
      addBankIDIframeIfItsNeeded(bankID);
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
        content : bankID.statusMessage(),
        margin : (($(window).height()- 200) /2)  + "px auto 0",
        closeVisible : false,
        cancelVisible : false,
        onAccept : function() {
          modal.close();
          args.onError();
        }
      });
      return true;
    },
    onCriticalError : function(xhr) {
      clearBankIDIframes();
      new ReloadDueToErrorModal(xhr);
    },
    thisDevice : args.thisDevice
  });
  bankID.initiateTransaction();
  updateLoadingDialogWithBankIDStatus(bankID);

};

});
