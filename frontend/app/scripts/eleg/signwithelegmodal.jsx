/** @jsx React.DOM */

define(['eleg/bankidsigningmodal', 'Backbone', 'legacy_code'], function(BankIDSigningModal, Backbone) {


return function(args) {
    var self = this;
    var copy = $(localization.docsignview.eleg.bankid.signConfirmationText);
    copy.find('.put-signatory-name-here').text(args.signatory.name());

    var signThisDeviceButton = new Button({
      cssClass : "other-sign-button signbutton",
      text:localization.docsignview.eleg.bankid.modalAnotherDevice,
      onClick:function() {
        self.modal.close(true);
        new BankIDSigningModal({
            signatory : args.signatory,
            onSuccess  : args.onSuccess,
            onError    : args.onError,
            thisDevice : false
          });
      }
    });

    var signOtherDeviceButton = new Button({
      text:localization.docsignview.eleg.bankid.modalThisDevice,
      cssClass: "signbutton",
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

    if (!BrowserInfo.isSmallScreen()) {
      var buttons = $("<div/>");
      self.modal = new Confirmation({
        title : localization.docsignview.eleg.bankid.signConfirmationTitle,
        cssClass: 'grey sign-confirmation-modal',
        content : $("<div/>").append($("<p/>").text(localization.docsignview.eleg.bankid.rfa20)),
        onReject : args.onReject,
        acceptButton : buttons,
        signview : args.signview,
        margin: args.margin,
        fast: args.fast,
        width: 520
      });

      buttons.append(signOtherDeviceButton.el()).append(signThisDeviceButton.el());
    } else {
      self.modal = new Confirmation({
        width: 825,
        title : "",
        cssClass: 'grey sign-confirmation-modal small-device',
        content : $("<p/>").text(localization.docsignview.eleg.bankid.rfa20),
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

      // Styling
      var thisDeviceButton = signThisDeviceButton.el();
      thisDeviceButton.css("margin-top", "20px");

      modalBody.append(signOtherDeviceButton.el());
      modalBody.append(thisDeviceButton);
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
