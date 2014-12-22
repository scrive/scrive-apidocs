/** @jsx React.DOM */

define(['eleg/bankidsigningmodal', 'Backbone', 'legacy_code'], function(BankIDSigningModal, Backbone) {


return function(args) {
    var self = this;
    var copy = $(localization.docsignview.eleg.bankid.signConfirmationText);
    copy.find('.put-signatory-name-here').text(args.signatory.name());

    if (!BrowserInfo.isSmallScreen()) {
      var buttons = $("<div/>");
      self.modal = new Confirmation({
        title : localization.docsignview.eleg.bankid.signConfirmationTitle,
        cssClass: 'grey sign-confirmation-modal',
        content : $("<div/>").append(copy).append($("<p/>").text(localization.docsignview.eleg.bankid.rfa20)),
        onReject : args.onReject,
        acceptButton : buttons,
        signview : args.signview,
        margin: args.margin,
        fast: args.fast,
        width: 520
      });

      var signAnotherDeviceButton = new Button({
        color: "black",
        cssClass : "other-sign-button",
        text:localization.docsignview.eleg.bankid.modalAnotherDevice,
        onClick:function() {
	  mixpanel.track('Clicked BankID on another device');
          self.modal.close(true);
          new BankIDSigningModal({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
              onError    : args.onError,
              thisDevice : false
            });
        }
      });

      var signThisDeviceButton = new Button({
        color: "green",
        text:localization.docsignview.eleg.bankid.modalThisDevice,
        onClick:function() {
	  mixpanel.track('Clicked BankID on this device');
          self.modal.close(true);
          new BankIDSigningModal({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
              onError    : args.onError,
              thisDevice : true
            });
        }
      });
      buttons.append(signAnotherDeviceButton.el()).append(signThisDeviceButton.el());
    } else {
      self.modal = new Confirmation({
        width: 825,
        title : "",
        cssClass: 'grey sign-confirmation-modal small-device',
        content : copy,
        acceptButton : undefined,
        signview : args.signview,
        margin: args.margin,
        fast: args.fast
      });
      var signModal = $('.sign-confirmation-modal .modal-container');
      var modalHeader = signModal.find('.modal-header');
      var modalBody = signModal.find('.modal-body');
      var modalFooter = signModal.find('.modal-footer');
      // Add a custom close button
      var close = $('<a class="small-device-go-back-button">' + localization.process.cancel + '</a>');
      close.click(function() { self.modal.close(); args.onReject(); });

      // Remove the modal footer but keep the button (regular or mobile bankid)
      var signButton = new Button({
        color: "green",
        size: "big",
        style:"margin-top:-10px;margin-bottom:10px",
        cssClass: "signbutton",
        text: localization.docsignview.eleg.bankid.mobilebankid,
        onClick:function() {
          self.modal.close(true);
          new BankIDSigningModal({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
              onError    : args.onError,
              thisDevice : true
            });
        }
      });

      // Styling
      modalBody.append(signButton.el());
      modalBody.append(close);
      modalHeader.remove();
      modalFooter.remove();
      modalBody.find('.body')
        .css('font-size', '52px')
        .css('line-height', '72px')
        .css('margin-top', '40px');
      // Check so we didn't put the modal outside of the window, if we can help it.
      // This is a special case that will go away when we refactor the modals / small screen modals.
      if (window.innerHeight > signModal.height()) {
        var modalBottom = signModal.height() + parseInt(signModal.css("margin-top"));
        if (modalBottom > window.innerHeight) {
          signModal.css("margin-top", window.innerHeight - signModal.height() - 100);
        }
      }
    }
  };

});
