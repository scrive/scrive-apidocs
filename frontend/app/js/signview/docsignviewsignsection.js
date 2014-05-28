/* Signatory view of document
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection(model : document).el);
 */

define(['signview/send_sms_pin_modal', '../../libs/tinycolor-min','Backbone', 'legacy_code'], function(SendSMSPinModal,tinycolor) {


window.DocumentSignConfirmation = function(args) {
          if (args.model.document().currentSignatory().smsPinAuthentication()) {
             var modal = new SendSMSPinModal({
                model: args.model,
                margin : args.margin,
                fast : args.fast,
                onSend : function() {
                 args.fast = true;
                 args.margin = modal.modalAbsoluteTop() + "px auto 0";
                 new DocumentSignConfirmationForSigning(args).popup();
                }
            });
          } else
             new DocumentSignConfirmationForSigning(args).popup();
};


window.DocumentSignConfirmationForSigning = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'popup', 'onSignedDocument', 'createContentElems');
    this.signaturesPlaced = args.signaturesPlaced;
    this.signview = args.signview;
    this.fast = args.fast;
    this.margin = args.margin;
  },

  document : function() {
    return this.model.document();
  },

  elegMishmatchErrorMessage : function(onName,onNumber) {
    var document = this.document();
    var signatory = document.currentSignatory();
    var numberCanBeChanged = (signatory.personalnumberField() != undefined && !signatory.personalnumberField().isClosed());
    var nameCanBeChanged = (signatory.fstnameField() != undefined && !signatory.fstnameField().isClosed()) || (signatory.sndnameField() != undefined && !signatory.sndnameField().isClosed());
    if (onName && onNumber && (!numberCanBeChanged || !nameCanBeChanged))
      return localization.sign.eleg.mismatch.mismatchOnNameAndNumberCantChange;
    if (onName && onNumber && numberCanBeChanged && nameCanBeChanged)
      return localization.sign.eleg.mismatch.mismatchOnNameAndNumberCanChange;
    if (onName && !nameCanBeChanged)
      return localization.sign.eleg.mismatch.mismatchOnNameAndCantChange;
    if (onNumber && !numberCanBeChanged)
      return localization.sign.eleg.mismatch.mismatchOnNumberAndCantChange;
    if (onName && nameCanBeChanged)
      return localization.sign.eleg.mismatch.mismatchOnNameAndCanChange;
    if (onNumber && numberCanBeChanged)
      return localization.sign.eleg.mismatch.mismatchOnNumberAndCanChange;
  },

  /**
   *  Block browser from reloading page
   */
  startBlockingReload : function() {
    window.onbeforeunload = function() {return localization.signingInProgressDontCloseWindow;};
  },

  /**
   *  Stop blocking browser from reloading page
   */
  stopBlockingReload : function() {
    window.onbeforeunload = function() {};
  },

  openSigningFailedAndReloadModal : function() {
      var text = $("<div>").append($("<span/>").text(localization.signingErrorMessage1))
                           .append("<BR/>")
                           .append($("<span/>").text(localization.signingErrorMessage2));
      var button = new Button({color: 'green',
                               style : "margin:20px",
                               text: localization.signingErrorReload,
                               onClick : function() {
                                    new Submit().send(); // Same as window.location.reload(), but will reset scrolling
                               }
                             }).el();
      ScreenBlockingDialog.open({header:text,  content : button});
  },

  createElegButtonElems: function() {
    var document = this.document();
    var signatory = document.currentSignatory();
    var self = this;

    var errorCallback = function(xhr) {
        self.stopBlockingReload();

        if (self.confirmation != undefined)         self.confirmation.clear();
        if (self.signinprogressmodal != undefined) self.signinprogressmodal.close();
        var data = {};
        try {
          data = JSON.parse(xhr.responseText);
        } catch (e) {}

        if (xhr.status == 403) {
            // session timed out
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
        } else if (xhr.status == 400 && data.elegProblem && data.mismatch){
            new FlashMessage({content: self.elegMishmatchErrorMessage(data.onName,data.onNumber),
                              color: 'red'});
        } else if (xhr.status == 400 && data.elegProblem){
            new FlashMessage({content: localization.sign.eleg.failed,
                              color: 'red'});
        } else {
            self.openSigningFailedAndReloadModal();
        }
    };

    var makeCallback = function(bankName, bankSign, bankSignExtraOpt) {
      mixpanel.track('Click ' + bankName);
      bankSign(document, signatory, function(elegParams) {
        document.checksign(function() {
            self.startBlockingReload();
            self.confirmation.clear();
            new FlashMessagesCleaner(); // We clean all flash message, so they don't land on screenshot
            self.signinprogressmodal = new SigningInProgressModal({
                                            document : document,
                                            margin: self.confirmation.margin(),
                                            textcolor : self.model.usebranding() ? self.model.signviewbranding().signviewtextcolour() : undefined,
                                            textfont : self.model.usebranding() ? self.model.signviewbranding().signviewtextfont() : undefined
                                       });
            self.screenshotDone = false;
            document.takeSigningScreenshot(function() {
                  self.screenshotDone = true;
            });

            document.sign(errorCallback, self.onSignedDocument,elegParams).sendAjax();

        }, errorCallback,elegParams).send();

        }, bankSignExtraOpt);
      return false;

    };

    var bankid = new Button({
      text: localization.sign.eleg.bankid,
      cssClass: 'bankid',
      color: 'blue',
      oneClick: true,
      onClick: function() { return makeCallback('BankID', Eleg.bankidSign); }
    });

    var telia = new Button({
      text: localization.sign.eleg.telia,
      cssClass: 'bankid',
      color: 'blue',
      oneClick: true,
      onClick: function() { return makeCallback('Telia', Eleg.teliaSign); }
    });

    var mbi = new Button({
      text: localization.sign.eleg.mobilebankid,
      cssClass: 'bankid mbi',
      color: 'blue',
      oneClick: true,
      onClick: function() { return makeCallback('Mobile BankID', Eleg.mobileBankIDSign, signatory.personalnumberField().value()); }
    });

    return $("<span class='elegButtonFooter' />").append(bankid.el()).append(mbi.el()).append(telia.el());
  },


  /**
   *  Show different pages when a document is signed,
   *  based on a few conditions,
   */
  postSigningAction: function(newDocument, oldDocument) {
    // Signing through api
    if (oldDocument.currentSignatory().signsuccessredirect() != undefined && oldDocument.currentSignatory().signsuccessredirect() != "") {
      window.location = oldDocument.currentSignatory().signsuccessredirect();
    }
    // Display regular document page
    else {
      new Submit().send();
    }
  },

  /**
   *  Start checking with an interval, if we can show postsignview
   */
  onSignedDocument: function(newDocument, oldDocument) {
    var self = this;
    this.stopBlockingReload();

    var postSign = function() {
      if (self.signinprogressmodal != undefined && !self.signinprogressmodal.done()) {
        self.signinprogressmodal.setCanBeFinished();
        setTimeout(postSign, 100);
      } else {
	self.postSigningAction(newDocument, oldDocument);
      }
    };
    postSign();
  },
  createSignButtonElems: function() {
    var document = this.document();
    var signviewbranding = this.model.signviewbranding();
    var signatory = document.currentSignatory();
    var self = this;
    var button =  new Button({
      size: BrowserInfo.isSmallScreen() ? "big" : "small",
      color: "green",
      customcolor: this.model.usebranding() ? signviewbranding.signviewprimarycolour() : undefined,
      textcolor: this.model.usebranding() ? signviewbranding.signviewprimarytextcolour() : undefined,
      cssClass: 'greybg signbutton',
      text: this.signaturesPlaced ? localization.process.signbuttontextfromsignaturedrawing : localization.process.signbuttontext,
      oneClick : true,
      onClick: function() {
        if (signatory.smsPinAuthentication() && (self.pin == undefined || self.pin == "")) {
          new FlashMessage({content: localization.docsignview.pinSigning.noPinProvided,  color: 'red'});
          button.setNotClicked();
          return;
        }

        self.startBlockingReload();
        var errorCallback = function(xhr) {
            self.stopBlockingReload();
            var data = {};
            try {
              data = JSON.parse(xhr.responseText);
            } catch (e) {}

            if (xhr.status == 400 && data.pinProblem) {
                button.setNotClicked();
                new FlashMessage({content: localization.docsignview.pinSigning.invalidPin,  color: 'red'});
            } else {
              if (self.confirmation != undefined)         self.confirmation.clear();
              if (self.signinprogressmodal != undefined) self.signinprogressmodal.close();
              if (xhr.status == 403)
                ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
              else
                self.openSigningFailedAndReloadModal();
            }
        };

        var pinParam = signatory.smsPinAuthentication() ? {pin : self.pin} : {};

        document.checksign(function() {

          var modalTop = self.confirmation.absoluteTop();

          self.confirmation.clear();
          new FlashMessagesCleaner(); // We clean all flash message, so they don't land on screenshot
          self.signinprogressmodal = new SigningInProgressModal({
                                          document : document,
                                          margin: modalTop + "px auto 0",
                                          textcolor : self.model.usebranding() ? self.model.signviewbranding().signviewtextcolour() : undefined,
                                          textfont : self.model.usebranding() ? self.model.signviewbranding().signviewtextfont() : undefined
                                     });
          self.screenshotDone = false;
          document.takeSigningScreenshot(function() {
                self.screenshotDone = true;
          });
          var f = function() {
            if (!self.screenshotDone) {
              setTimeout(f, 100);
              return false;
            }
            trackTimeout('Accept', {'Accept' : 'sign document'});

            var pinParam = signatory.smsPinAuthentication() ? {pin : self.pin} : {};
            document.sign(errorCallback, self.onSignedDocument, pinParam).send();
          };
          f();
      }, errorCallback,pinParam).send();
      return false;
      }
    });

    button.el().css('margin-top', '-10px')
               .css('margin-bottom', BrowserInfo.isSmallScreen() ? '10px' : '0px');
    return button.el();
  },

  createPreambleElems: function() {
    var document = this.document();
    var signatory = document.currentSignatory();

    if (signatory.author()) {
     var content = $("<div />");
     if (document.authorIsOnlySignatory())
            content = $(localization.process.signatorysignmodalcontentauthoronly);
     else if (signatory.elegAuthentication())
          content.append(localization.process.signatorysignmodalcontentsignvieweleg);
     else
          content.append(localization.process.signatorysignmodalcontent);

     if (signatory.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.signByAuthor.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.signByAuthor.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.signByAuthor.eleg.body1).append(a).append(localization.signByAuthor.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    } else {
      var content = $("<div />");
      if (signatory.elegAuthentication())
          content.append(localization.process.signatorysignmodalcontentsignvieweleg);
      else {
        content.append(this.signaturesPlaced ? localization.process.signatorysignmodalcontentfromsignaturedrawing : localization.process.signatorysignmodalcontent);
      }

      if (signatory.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    }
  },

  pinCodeInput : function() {
    var self = this;
    var signviewbranding = this.model.signviewbranding();
    var color = this.model.usebranding() && signviewbranding.signviewprimarycolour() ? signviewbranding.signviewprimarycolour() : '#53b688';
    var standardBorderColor = tinycolor(color);
    var highlightedBorderColor = tinycolor(color);
    highlightedBorderColor.setAlpha(1);
    standardBorderColor.setAlpha(0.6);
    var focused = false;
    var p = $("<p>");
    p.append("<span/>").text(localization.docsignview.pinSigning.enterSMSPin);
    var iti = new InfoTextInput({
          infotext: localization.docsignview.pinSigning.checkYourPhone,
          value: "",
          onChange: function(v) {
            self.pin = v;
          },
          inputtype: 'text',
          cssClass : "pin-input " + (BrowserInfo.isSmallScreen() ? "small-screen" : ""),
          name: 'pin',
          onFocus: function() {
            focused = true;
            iti.el().css("border-color", highlightedBorderColor);
          },
          onBlur: function() {
            focused = false;
            iti.el().css("border-color", standardBorderColor);
          }
        });

    var el = iti.el();
    el.css("border-color", standardBorderColor);

    el.hover(function() {
      if (focused) return;
      iti.el().css("border-color", highlightedBorderColor);
    }, function() {
      if (focused) return;
      iti.el().css("border-color", standardBorderColor);
    });

    p.append(iti.el());
    return p;
  },
  createContentElems: function() {
    var content = $("<div />");
    var document = this.document();
    var signatory = document.currentSignatory();

    if (signatory.smsPinAuthentication()) {
      content.append(this.pinCodeInput());
    }

    content.append(this.createPreambleElems());
    if (BrowserInfo.isSmallScreen()) {
        var p = content.find('p'); p.css('font-size', '52px');
        p.css('line-height', '72px');
        p.css('margin-top', '40px');
    }
    return content;
  },

  popup: function() {
    var document = this.document();
    var signviewbranding = this.model.signviewbranding();
    var arrow = this.model.arrow();
    var signatory = document.currentSignatory();
    var self = this;
    var title;
    self.signview = this.signview;

    if (signatory.elegAuthentication()) {
      title = localization.process.signatorysignmodaltitleeleg;
    } else {
      title = localization.signByAuthor.modalTitle;
      if (this.signaturesPlaced)
        title = localization.process.signatorysignmodaltitlefromsignatorydrawing;
    }

    if (arrow) {
      arrow.disable();
    }

    var isSmallScreen = BrowserInfo.isSmallScreen();

    self.confirmation = new Confirmation({
      cssClass: 'grey sign-confirmation-modal' + (isSmallScreen ? ' small-device' : ''),
      title: title,
      signview: this.margin ? false : self.signview, // margin overrides signview
      acceptButton: signatory.elegAuthentication() ? this.createElegButtonElems() : this.createSignButtonElems(),
      rejectText: localization.cancel,
      // use default width for eleg, as there is less text
      width: signatory.elegAuthentication() ? undefined : (isSmallScreen ? 825 : 520),
      margin : this.margin || (isSmallScreen ? '150px auto 0px' : undefined),
      textcolor : this.model.usebranding() ? signviewbranding.signviewtextcolour() : undefined,
      textfont : this.model.usebranding() ? signviewbranding.signviewtextfont() : undefined,
      onReject: function() {
        if (arrow) {
          arrow.enable();
        }
      },
      fast: this.fast,
      content: this.createContentElems
    });

    // TODO rewrite me, but not on staging
    // Re-adjust the signing modal for small screen devices.
    if (isSmallScreen) {
      var signModal = $('.sign-confirmation-modal .modal-container');
      var modalHeader = signModal.find('.modal-header');
      var modalBody = signModal.find('.modal-body');
      var modalFooter = signModal.find('.modal-footer');
      modalHeader.remove();

      // Add a custom close button
      var close = $('<a class="small-device-go-back-button">' + localization.process.cancel + '</a>');
      close.click(function() { self.confirmation.close(); });

      // Remove the modal footer but keep the button (regular or mobile bankid)
      var signButton = modalFooter.find('.button.signbutton').detach();
      if (signatory.elegAuthentication()) {
        signButton = modalFooter.find('.mbi').detach();
        signButton.addClass("button-green signbutton");
      }
      modalFooter.remove();

      // Styling
      modalBody.append(signButton);
      modalBody.append(close);

      // Check so we didn't put the modal outside of the window, if we can help it.
      // This is a special case that will go away when we refactor the modals / small screen modals.
      if (window.innerHeight > signModal.height()) {
        var modalBottom = signModal.height() + parseInt(signModal.css("margin-top"));
        if (modalBottom > window.innerHeight) {
          signModal.css("margin-top", window.innerHeight - signModal.height() - 100);
        }
      }
    }
  }
});

window.DocumentSignSignSection = Backbone.View.extend({
   initialize : function(args){
      this.render();
   },
   render: function() {
       var model = this.model;
       var signviewbranding = this.model.signviewbranding();
       var document = this.model.document();
       var box = $(this.el).addClass('section').addClass('spacing').addClass('signbuttons');

       var signatory = document.currentSignatory();
       var signatoryHasPlacedSignatures = signatory.hasPlacedSignatures() || DocumentExtraDetails.askForSignature(signatory);
       var sps = {};
       sps['Has user?'] = signatory.hasUser();
       sps['First visit'] = !signatory.seendate();
       mixpanel.register(sps);

       // track signatory properties
       var ps = {};
       ps['Full Name'] = signatory.nameOrEmail();
       ps['$email'] = signatory.email();
       if(signatory.fstname())
           ps['$first_name'] = signatory.fstname();
       if(signatory.sndname())
           ps['$last_name'] = signatory.sndname();
       if(signatory.hasUser())
           ps['$username'] = signatory.email();
       mixpanel.people.set(ps);

       mixpanel.track('View sign view');

       var rejectErrorCallback = function(xhr) {
         if (xhr.status == 403) {
           // session timed out
           ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
         } else {
           new FlashMessage({content: localization.signviewSigningFailed,
                             color: 'red',
                             withReload: true});
         }
       };
       this.rejectButton = new Button({
                                        color: "black"
                                        text: localization.process.rejectbuttontext,
                                        onClick: function() {
                                            mixpanel.track('Click Reject');
                                            var arrow = model.arrow();
                                            if (arrow) { arrow.disable(); }
                                            ConfirmationWithEmail.popup({
                                            title: localization.process.signatorycancelmodaltitle,
                                            mail: document.currentSignatory().rejectMail(),
                                            icon: null,
                                            signview: true,
                                            cssClass: "grey",
                                            acceptText: localization.reject.send,
                                            editText: localization.reject.editMessage,
                                            rejectText: localization.cancel,
                                            acceptColor: "red",
                                            textcolor : model.usebranding() ? signviewbranding.signviewtextcolour() : undefined,
                                            textfont : model.usebranding() ? signviewbranding.signviewtextfont() : undefined,
                                            oneClick : true,
                                            onAccept: function(customtext) {
                                                trackTimeout('Accept',
                                                             {'Accept' : 'reject document'},
                                                             function() {
                                                                 document.currentSignatory().reject(customtext).sendAjax(
                                                                   function() {
                                                                    if (document.currentSignatory().rejectredirect() != undefined && document.currentSignatory().rejectredirect() != "") {
                                                                      window.location = document.currentSignatory().rejectredirect();
                                                                     }
                                                                     else {
                                                                      window.location.reload();
                                                                     }
                                                                  },
                                                                   rejectErrorCallback
                                                                );
                                                             });
                                              },
                                              onReject: function() {
                                                var arrow = model.arrow();
                                                if (arrow) { arrow.enable(); }
                                              }
                                            });
                                        }
                                });

       var signButtonText = localization.process.signbuttontext;
       if (signatoryHasPlacedSignatures) {
         signButtonText = localization.next;
       }

       this.signButton = new Button({
                            color: "green",
                            customcolor: model.usebranding() ? signviewbranding.signviewprimarycolour() : undefined,
                            textcolor: model.usebranding() ? signviewbranding.signviewprimarytextcolour() : undefined,
                            text: signButtonText,
                            onClick: function() {

                                var valid =  model.tasks().notCompletedTasks().length == 1 && model.tasks().notCompletedTasks()[0] == model.signtask();
                                if (!valid) {
                                        model.arrow().blink();
                                        return false;
                                    }
                                mixpanel.track('Click sign');


                                new DocumentSignConfirmation({
                                    model: model,
                                    signview: true,
                                    signaturesPlaced: signatoryHasPlacedSignatures
                                });
                               }
                            });

      var signButton = this.signButton.el();

      if (BrowserInfo.isSmallScreen()) {
        signButton.css({
          'padding-left': '0px',
          'padding-right': '0px',
          'font-size': '100px',
          'height': '100px',
          'max-height': '120px',
          'line-height': '85px',
          'padding-top': '55px',
          'padding-bottom': '55px',
          'margin': '0px',
          'width': '100%'
        });
      }

      if (model.hasRejectOption() && !BrowserInfo.isSmallScreen()) {
        box.append($("<div class='rejectwrapper reject'>").append(this.rejectButton.el()));
        box.append($("<div class='signwrapper sign'>").append(signButton));
      }
      else {
        box.css("text-align","center").append($("<div class='signwrapper sign' style='width:100%;margin-right:0px;'>").append(signButton));
        if (BrowserInfo.isSmallScreen()) {
          box.css("padding", "0px");
          box.css("margin", "10px auto auto");
          box.css("width", "939px");
        }
      }
      box.append($("<div class='clearfix' />"));

    setTimeout(function() {
      document.takeFirstScreenshot();
    }, 1500);
   }
});

});
