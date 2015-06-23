/** @jsx React.DOM */

define(['eleg/bankidsigning', 'legacy_code'], function(BankIDSigning, _legacyCode) {

var updateLoadingDialogWithBankIDStatus = function(bankID) {
  var div = $("<div/>");
  if (!bankID.isFaultStatus() && !bankID.isWaitingForToken()) {
    div.append($("<p/>").html(bankID.statusMessage()));
    if (bankID.thisDevice() && (bankID.isStatusOutstanding() || bankID.isStatusNoClient())) {
      if (BrowserInfo.isAndroid()) {
        // on android devices, dont wait 5s, fall back to the other method immediately
        // since iframes dont work at all, as a bonus, there's no need to click
        // another button

        // but first changing window.location from https:// to bankid:// scheme
        // when window.onbeforeunload is set causes alert on android devices
        // so disable onbeforeunload for some time
        ReloadManager.stopBlocking();
        setTimeout(function() {
          ReloadManager.startBlocking();
        }, 5000);

        window.location = bankID.bankIdUrl();
      } else if (bankID.activeForAtLeast5Sec()) {
        div.append($("<a class='button button-green button-tiny'/>").text(localization.docsignview.eleg.bankid.rfa18).click(function() {
          window.location = bankID.bankIdUrl();
        }));
      }
    }
  }
  LoadingDialog.open({header: div});
};

var addBankIDIframeIfItsNeeded = function(bankID) {
  if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice() && ($("#bankid-"+ bankID.autoStartToken()).size() == 0) && !BrowserInfo.isAndroid()) {
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
