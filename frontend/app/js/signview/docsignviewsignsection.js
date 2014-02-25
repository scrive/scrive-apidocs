/* Signatory view of document
 * Usage:
 *
 *   $('body').append(new DocumentSignSignSection(model : document).el);
 */

define(['Backbone', 'legacy_code'], function() {

window.DocumentSignConfirmation = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'popup');
    _.bindAll(this, 'createContentElems');
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
  startBlockingReload : function() {
    window.onbeforeunload = function() {return localization.signingInProgressDontCloseWindow;};
  },
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
      bankSign(document, signatory, function(p) {
        document.checksign(function() {
            self.startBlockingReload();
            self.confirmation.clear();
            self.signinprogressmodal = new SigningInProgressModal({
                                            document : document,
                                            textcolor : self.model.usebranding() ? self.model.signviewbranding().signviewtextcolour() : undefined,
                                            textfont : self.model.usebranding() ? self.model.signviewbranding().signviewtextfont() : undefined
                                       });
            self.screenshotDone = false;
            document.takeSigningScreenshot(function() {
                  self.screenshotDone = true;
            });

            document.sign(errorCallback, self.signinprogressmodal, function(){self.stopBlockingReload();}).addMany(p).sendAjax();

        }, errorCallback).addMany(p).send();

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
  createSignButtonElems: function() {
    var document = this.document();
    var signviewbranding = this.model.signviewbranding();
    var self = this;
    return new Button({
      size: BrowserInfo.isSmallScreen() ? "big" : "small",
      color: "green",
      shape: BrowserInfo.isSmallScreen() ? "" : "rounded",
      customcolor: this.model.usebranding() ? signviewbranding.signviewprimarycolour() : undefined,
      textcolor: this.model.usebranding() ? signviewbranding.signviewprimarytextcolour() : undefined,
      cssClass: 'greybg',
      text: localization.process.signbuttontext,
      oneClick : true,
      onClick: function() {
        self.startBlockingReload();
        var errorCallback = function(xhr) {
            self.stopBlockingReload();
            if (self.confirmation != undefined)         self.confirmation.clear();
            if (self.signinprogressmodal != undefined) self.signinprogressmodal.close();
            if (xhr.status == 403) {
              // session timed out
              ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
            } else {
              self.openSigningFailedAndReloadModal();
            }
        };


        document.checksign(function() {
          self.confirmation.clear();
          self.signinprogressmodal = new SigningInProgressModal({
                                          document : document,
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


            document.sign(errorCallback,self.signinprogressmodal,function(){self.stopBlockingReload();}).send();
          };
          f();
      }, errorCallback).send();
      return false;
      }
    }).el().css('margin-top', '-10px')
              .css('margin-bottom', BrowserInfo.isSmallScreen() ? '10px' : '0px');
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
      else
          content.append(localization.process.signatorysignmodalcontent);

      if (signatory.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    }
  },
  createContentElems: function() {
    var content = $("<div />");
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
    var signatory = document.currentSignatory();
    var self = this;
    var title;
    self.screenshotDone = false;

    if (signatory.elegAuthentication()) {
      title = localization.process.signatorysignmodaltitleeleg;
    } else {
      title = localization.signByAuthor.modalTitle;
    }

    self.confirmation = new Confirmation({
      cssClass: 'grey sign-confirmation-modal',
      title: title,
      acceptButton: signatory.elegAuthentication() ? this.createElegButtonElems() : this.createSignButtonElems(),
      rejectText: localization.cancel,
      // use default width for eleg, as there is less text
      width: signatory.elegAuthentication() ? undefined : (BrowserInfo.isSmallScreen() ? 825 : 520),
      margin : BrowserInfo.isSmallScreen() ? '150px auto 0px' : undefined,
      textcolor : this.model.usebranding() ? signviewbranding.signviewtextcolour() : undefined,
      textfont : this.model.usebranding() ? signviewbranding.signviewtextfont() : undefined,
      content: this.createContentElems
    });

    // TODO rewrite me, but not on staging
    // Re-adjust the signing modal for small screen devices.
    if (BrowserInfo.isSmallScreen()) {
      var signModal = $('.sign-confirmation-modal .modal-container');
      var modalHeader = signModal.find('.modal-header');
      var modalBody = signModal.find('.modal-body');
      var modalFooter = signModal.find('.modal-footer');
      // Remove the modal header but keep the close button
      modalHeader.remove();
      var close = $('<a class="small-device-go-back-button">' + localization.process.cancel + '</a>');
      close.click(function() { self.confirmation.close(); });

      // Remove the modal footer but keep the button (regular or mobile bankid)
      var signButton = modalFooter.find('.button.button-green').detach();
      if (signatory.elegAuthentication()) {
        signButton = modalFooter.find('.mbi').detach();
        signButton.addClass("button-green");
      }
      modalFooter.remove();


      // Styling
      signModal.addClass('small-device');
      
      signModal.find('.modal-content').css('border-bottom', '0px');

      signButton.css({
        'font-size': '80px',
        'height': '100px',
        'width': '80%',
        'max-height': '120px',
        'margin-right': '48px',
        'margin-bottom': '65px',
        'margin-top': '40px',
        'line-height': '105px',
        'padding-bottom': '55px',
        'padding-top': '50px',
        'float': 'none',
        'display': 'block',
        'margin': '0px auto',
        'margin-bottom': '20px',
        'margin-top': '10px'
      });

      modalBody.css({'padding-bottom': '50px'});
      signModal.css({'border-bottom': '0px'});

      modalBody.append(signButton);
      modalBody.append(close);
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
                                        size: "big",
                                        color: "blue",
                                        shape : "rounded",
                                        width: 206,
                                        text: localization.process.rejectbuttontext,
                                        onClick: function() {
                                            mixpanel.track('Click Reject');
                                            ConfirmationWithEmail.popup({
                                            title: localization.process.signatorycancelmodaltitle,
                                            mail: document.currentSignatory().rejectMail(),
                                            icon: null,
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
                                              }
                                            });
                                        }
                                });



       this.signButton = new Button({
                            size: "big",
                            shape : BrowserInfo.isSmallScreen() ? "" : "rounded",
                            color: "green",
                            customcolor: model.usebranding() ? signviewbranding.signviewprimarycolour() : undefined,
                            textcolor: model.usebranding() ? signviewbranding.signviewprimarytextcolour() : undefined,
                            width: BrowserInfo.isSmallScreen() ?  504 : 206,
                            text: localization.process.signbuttontext,
                            icon: BrowserInfo.isSmallScreen() ? undefined : $("<span class='icon cross' style='position: absolute; top: auto;margin-top: -1px;'></span>"),
                            onClick: function() {

                                var valid =  model.tasks().notCompleatedTasks().length == 1 && model.tasks().notCompleatedTasks()[0] == model.signtask();
                                if (!valid) {
                                        model.arrow().blink();
                                        return false;
                                    }
                                mixpanel.track('Click sign');
                                new DocumentSignConfirmation({
                                    model: model
                                    }).popup();
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
