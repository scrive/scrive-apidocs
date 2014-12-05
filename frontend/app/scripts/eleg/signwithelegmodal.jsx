/** @jsx React.DOM */

define(['eleg/bankid', 'Backbone', 'legacy_code'], function(BankID, Backbone) {


return function(args) {
    var self = this;
    if (!BrowserInfo.isSmallScreen()) {
      var buttons = $("<div/>");
      self.modal = new Confirmation({
        title : localization.docsignview.eleg.chooseElegModalTitle,
        cssClass: 'grey sign-confirmation-modal',
        content : localization.docsignview.eleg.bankid.rfa20,
        acceptButton : buttons,
        signview : args.signview,
        margin: args.margin,
        fast: args.fast,
        width: 520,
      });

      var signThisDeviceButton = new Button({
        color: "grey",
        style: "margin-right: 15px;",
        text:localization.docsignview.eleg.bankid.modalAnotherDevice,
        onClick:function() {
          self.modal.close();
          new BankID({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
              thisDevice : false
            });
        }
      });

      var signOtherDeviceButton = new Button({
        color: "grey",
        text:localization.docsignview.eleg.bankid.modalThisDevice,
        onClick:function() {
          self.modal.close();
          new BankID({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
              thisDevice : true
            });
        }
      });
      buttons.append(signThisDeviceButton.el()).append(signOtherDeviceButton.el());
    } else {
      self.modal = new Confirmation({
        width: 825,
        title : localization.docsignview.eleg.chooseElegModalTitle,
        cssClass: 'grey sign-confirmation-modal small-device',
        content : localization.docsignview.eleg.bankid.rfa20,
        acceptButton : buttons,
        signview : args.signview,
        margin: args.margin,
        fast: args.fast,
      });
      var signModal = $('.sign-confirmation-modal .modal-container');
      var modalHeader = signModal.find('.modal-header');
      var modalBody = signModal.find('.modal-body');
      var modalFooter = signModal.find('.modal-footer');
      // Add a custom close button
      var close = $('<a class="small-device-go-back-button">' + localization.process.cancel + '</a>');
      close.click(function() { self.modal.close(); });

      // Remove the modal footer but keep the button (regular or mobile bankid)
      var signButton = new Button({
        color: "grey",
        size: "big",
        style:"margin-top:-10px;margin-bottom:10px",
        cssClass: "signbutton",
        text:localization.docsignview.eleg.bankid.modalThisDevice,
        onClick:function() {
          self.modal.close();
          new BankID({
              signatory : args.signatory,
              onSuccess  : args.onSuccess,
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
