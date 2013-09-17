/* Signatories model + basic view + signatories attachments
 *
 * Instrumented with mixpanel
 */


(function(window) {

var AuthorViewSignatoryModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  destroy : function() {
    this.clear();
  },
  authorviewsignatories : function() {
    return this.get("authorviewsignatories");
  },
  document :function() {
     return this.authorviewsignatories().document();
  },
  signatory : function() {
     return this.get("signatory");
  },
  nameOrEmail : function() {
    return this.signatory().nameOrEmail();
  },
  nameOrEmailOrMobile : function() {
    return this.signatory().nameOrEmailOrMobile();
  },
  status : function() {
    return this.signatory().status();
  },
  signatorySummary: function() {
      var signatory = this.signatory();
      var document = this.document();
      if (signatory.signdate() != undefined)
        return localization.signatoryMessage.signed;
      else if (signatory.datamismatch() == true ||
               document.timedout() ||
               document.canceled() ||
               document.rejected() ||
               document.datamismatch())
          return localization.docsignview.unavailableForSign;
      else if (signatory.rejecteddate() != undefined)
          return localization.signatoryMessage.rejected;
      else if (signatory.status() == 'opened')
          return localization.signatoryMessage.seen;
      else if (signatory.status() == 'sent' && signatory.reachedBySignorder())
          return localization.signatoryMessage.other;
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.waiting;
      else if (localization.signatoryMessage[signatory.status()] != undefined)
          return localization.signatoryMessage[signatory.status()];
      return localization.signatoryMessage["other"];
 },
 signatoryViewerySummary: function() {
   var signatory = this.signatory();
   if (!signatory.isViewer()) {
     return this.signatorySummary(signatory);
   } else if (signatory.status() === 'sent') {
     return localization.signatoryMessage.sentViewer;
   } else if (signatory.status() === 'opened') {
     return localization.signatoryMessage.openedViewer;
   } else {
     return this.signatorySummary(signatory);
   }
  },
 hasRemindOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && !signatory.author()
          && signatory.signs()
          && signatory.reachedBySignorder()
          && (signatory.document().signingInProcess() || signatory.document().closed())
          && !signatory.undeliveredInvitation()
          && !signatory.padDelivery();
 },
 hasChangeEmailOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredMailInvitation()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && (signatory.emailDelivery() || signatory.emailMobileDelivery());
 },
 hasChangePhoneOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredSMSInvitation()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && (signatory.mobileDelivery() || signatory.emailMobileDelivery());
 },
 hasPadOptions : function() {
   var signatory = this.signatory();
   return    signatory.document().currentViewerIsAuthor()
          && signatory.document().signingInProcess()
          && signatory.canSign()
          && signatory.padDelivery();
 },
 hasGiveForSigningOnThisDeviceOption : function() {
   return this.hasPadOptions() && this.signatory().author() && BrowserInfo.isPadDevice();
 },
 hasRemoveFromPadQueueOption : function() {
   return this.hasPadOptions() && this.signatory().inpadqueue() && !BrowserInfo.isPadDevice();
 },
 hasAddToPadQueueOption : function() {
   return this.hasPadOptions() && !this.signatory().inpadqueue() && !BrowserInfo.isPadDevice();
 },
 hasAnyOptions : function() {
    return  this.hasRemindOption()
         || this.hasChangeEmailOption()
         || this.hasChangePhoneOption()
         || this.hasGiveForSigningOnThisDeviceOption()
         || this.hasAddToPadQueueOption()
         || this.hasRemoveFromPadQueueOption();

 }
});

var AuthorViewSignatoryView = Backbone.View.extend({
  initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
  },
  destroy : function() {
    this.stopListening();
    this.model.off();
    this.model.destroy();
    $(this.el).remove();
  },
  statusbox: function() {
      var model = this.model;
      var statusbox  = $('<div  class="statusbox" />');
      var space = $('<div class="spacing butt" />');
      var statusicon = $("<span class='icon status' />").addClass(model.status());
      var status     = $("<span class='status statustext' />").text(model.signatorySummary()).addClass(model.status());
      space.append(statusicon).append(status).addClass(model.status());
      statusbox.append(space);
      return statusbox;
  },
  remiderOption: function() {
         var self = this;
         var signatory = this.model.signatory();
         var text = signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.send;
         var button = new Button({
                            color: "black",
                            text: text,
                            onClick: function() {
                                 mixpanel.track('Click send reminder',
                                                {'Signatory index':signatory.signIndex()});
                                 if( signatory.emailDelivery()) {
                                     ConfirmationWithEmail.popup({
                                         title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
                                         mail: signatory.remindMail(),
                                         acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                                         editText: localization.reminder.formOwnMessage,
                                         rejectText: localization.cancel,
                                         onAccept: function(customtext) {
                                             trackTimeout('Accept',
                                                          {'Accept' : 'send reminder',
                                                           'Signatory index' : signatory.signIndex(),
                                                           'Delivery method' : 'Email'},
                                                          function() {
                                                              LoadingDialog.open();
                                                              signatory.remind(customtext).sendAjax(function() {
                                                                self.model.authorviewsignatories().authorview().reload(true);
                                                              });
                                                          });
                                             return true;
                                         }
                                     });
                                 } else if( signatory.mobileDelivery()) {
                                     Confirmation.popup({
                                         title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
                                         content: $("<div>").text(localization.reminder.mobileQuestion),
                                         acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                                         rejectText: localization.cancel,
                                         onAccept: function(customtext) {
                                             trackTimeout('Accept',
                                                          {'Accept' : 'send reminder',
                                                           'Signatory index' : signatory.signIndex(),
                                                           'Delivery method' : 'Mobile'},
                                                          function() {
                                                              LoadingDialog.open();
                                                              signatory.remind().sendAjax(function() {
                                                                self.model.authorviewsignatories().authorview().reload(true);
                                                              });
                                                          });
                                             return true;
                                         }
                                     });
                                 } else if( signatory.emailMobileDelivery()) {
                                     Confirmation.popup({
                                         title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
                                         content: $("<div>").text(localization.reminder.emailMobileQuestion),
                                         acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                                         rejectText: localization.cancel,
                                         onAccept: function(customtext) {
                                             trackTimeout('Accept',
                                                          {'Accept' : 'send reminder',
                                                           'Signatory index' : signatory.signIndex(),
                                                           'Delivery method' : 'Email and Mobile'},
                                                          function() {
                                                              LoadingDialog.open();
                                                              signatory.remind().sendAjax(function() {
                                                                self.model.authorviewsignatories().authorview().reload(true);
                                                              });
                                                          });
                                             return true;
                                         }
                                     });
                                 }
                        }
         });

         return button.el();
  },
  changeEmailOption : function() {
    var self = this;
    var signatory = this.model.signatory();
    var container = $("<div class='change-email-box'/>");
    var fstbutton = new Button({
                            color: "black",
                            text: localization.changeEmail,
                            style: "width:200px",
                            onClick: function() {
                                mixpanel.track('Click change email',
                                               {'Signatory index':signatory.signIndex()});
                                var input = $("<input type='text'/>");
                                input.val(signatory.email());
                                var sndbutton = new Button({
                                    size: "tiny",
                                    color: "blue",
                                    text: localization.send,
                                    onClick: function() {
                                        trackTimeout('Accept',
                                                     {'Signatory index':signatory.signIndex(),
                                                      'Accept' : 'change email'},
                                                     function() {
                                                         LoadingDialog.open();
                                                         signatory.changeEmail(input.val()).sendAjax(function() {
                                                          self.model.authorviewsignatories().authorview().reload(true);
                                                        });
                                                     });
                                    }
                                    });
                                container.empty().append(input).append(sndbutton.el());
                                return false;
                             }
                          });
    container.append(fstbutton.el());
    return container;
  },
  changePhoneOption : function() {
    var self = this;
    var signatory = this.model.signatory();
    var container = $("<div class='change-mobile-box'/>");
    var fstbutton = new Button({
                            color: "black",
                            text: localization.changePhone,
                            style: "width:200px",
                            onClick: function() {
                                mixpanel.track('Click change phone',
                                               {'Signatory index':signatory.signIndex()});
                                var input = $("<input type='text'/>");
                                input.val(signatory.mobile());
                                var sndbutton = new Button({
                                    size: "tiny",
                                    color: "blue",
                                    text: localization.send,
                                    onClick: function() {
                                        trackTimeout('Accept',
                                                     {'Signatory index':signatory.signIndex(),
                                                      'Accept' : 'change phone'},
                                                     function() {
                                                         LoadingDialog.open();
                                                         signatory.changePhone(input.val()).sendAjax(function() {
                                                            self.model.authorviewsignatories().authorview().reload(true);
                                                          });
                                                     });
                                    }
                                    });
                                container.empty().append(input).append(sndbutton.el());
                                return false;
                             }
                          });
    container.append(fstbutton.el());
    return container;
  },
  giveForSigningOnThisDeviceOption : function() {
                 var self = this;
                 var signatory = this.model.signatory();
                 var button = new Button({
                            color: "black",
                            text: localization.changePhone,
                            onClick: function() {
                                 mixpanel.track('Click give for signing',
                                                {'Signatory index':signatory.signIndex()});
                                     Confirmation.popup({
                                            title : localization.pad.signingOnSameDeviceConfirmHeader,
                                            content : localization.pad.signingOnSameDeviceConfirmText,
                                            acceptText : localization.pad.signingOnSameDevice ,
                                            rejectText : localization.cancel,
                                            onAccept : function() {
                                             mixpanel.track('Accept',
                                                            {'Signatory index':signatory.signIndex(),
                                                             'Accept' : 'give for signing'});
                                                       signatory.addtoPadQueue(function(resp) {
                                                           if (resp.error == undefined)
                                                               window.location = signatory.padSigningURL();
                                                           else
                                                               new FlashMessage({
                                                                   content: localization.pad.addToPadQueueNotAdded,
                                                                   color: "red"
                                                               });
                                                        }).send();
                                                       return true;
                                                    }
                                        });
                                }
                 });
                 return button.el();
    },
    removeFromPadQueueOption :  function() {
        var self = this;
        var signatory = this.model.signatory();
        var button = new Button({
                    color: "black",
                    text: localization.pad.removeFromPadQueue,
                    onClick: function() {
                        mixpanel.track('Click remove from pad queue',
                               {'Signatory index':signatory.signIndex()});
                        LoadingDialog.open();
                        signatory.removeFromPadQueue().sendAjax(function() {
                            self.model.authorviewsignatories().authorview().reload(true);
                        });
                    }
        });
        return button.el();
    },
    addToPadQueueOption : function() {
                 var self = this;
                 var signatory = this.model.signatory();
                 var button = $("<label  class='clickable addToPad'/>");
                 var icon = $("<div class='addToPadIcon'/>");
                 var text = localization.pad.addToPadQueue;
                 var textbox = $("<span/>").text(text);
                 var button = new Button({
                    color: "black",
                    text: localization.pad.addToPadQueue,
                    onClick: function() {
                         mixpanel.track('Click add to pad queue',
                                        {'Signatory index':signatory.signIndex()});
                             Confirmation.popup({
                                    title : localization.pad.addToPadQueueConfirmHeader,
                                    content : localization.pad.addToPadQueueConfirmText,
                                    acceptText : localization.pad.addToPadQueue ,
                                    rejectText : localization.cancel,
                                    onAccept : function()
                                            {
                                                mixpanel.track('Accept',
                                                               {'Accept' : 'add to pad queue',
                                                                'Signatory index':signatory.signIndex()});
                                               signatory.addtoPadQueue(function(resp) {window.location = window.location;}).sendAjax();
                                               LoadingDialog.open();
                                               signatory.addtoPadQueue().sendAjax(function() {
                                                self.model.authorviewsignatories().authorview().reload(true);
                                              });
                                               return true;
                                            }
                            });
                            return false;
                    }
                 });
                 return button.el();
    },
  authorOptions : function() {
    var signatory = this.model.signatory();
    var optionbox = $("<div class='optionbox'/>");
    if (this.model.hasRemindOption())
      optionbox.append(this.remiderOption());
    if (this.model.hasChangeEmailOption())
      optionbox.append(this.changeEmailOption());
    if (this.model.hasChangePhoneOption())
      optionbox.append(this.changePhoneOption());
    if (this.model.hasGiveForSigningOnThisDeviceOption())
      optionbox.append(this.giveForSigningOnThisDeviceOption());
    if (this.model.hasAddToPadQueueOption())
      optionbox.append(this.addToPadQueueOption());
    if (this.model.hasRemoveFromPadQueueOption())
      optionbox.append(this.removeFromPadQueueOption());
    return optionbox;
  },
  render: function() {
      var box = $(this.el);
      box.empty();
      box.addClass('sigbox');
      var signatory = this.model.signatory();
      var titleinfo = $('<div class="titleinfo spacing" />');
      var name      = $('<div class="name" />').text(signatory.nameOrEmailOrMobile());
      titleinfo.append(name);
      box.append(titleinfo);

      var inner   = $('<div class="inner spacing" />');

      var numspace = $('<div class="details" />');
      var company   = $('<div class="company" />').text(signatory.company());
      var orgnum  = $('<div class="orgnum field" />').text(localization.docsignview.companyNumberLabel + ": "
                                                           + (signatory.companynumber().trim() || localization.docsignview.notEntered))
          .attr('title', signatory.companynumber());
      var persnum = $('<div class="persnum field" />').text(localization.docsignview.personalNumberLabel + ": "
                                                            + (signatory.personalnumber().trim() || localization.docsignview.notEntered))
        .attr('title', signatory.personalnumber());

      numspace.append(company);

      if (signatory.email() != '') {
        var email   = $('<div class="email field" />').text(localization.email + ": " + signatory.email()).attr('title', signatory.email());
        numspace.append(email);
      }

      if (signatory.mobile() != '') {
        var mobile   = $('<div class="mobile field" />').text(signatory.mobile()).attr('title', signatory.mobile());
        numspace.append(mobile);
      }

      numspace.append(orgnum);
      numspace.append(persnum);

      inner.append(numspace);
      box.append(inner);
      box.append(this.statusbox());
      if (this.model.hasAnyOptions())
        box.append(this.authorOptions());
      return this;
  }
});

window.AuthorViewSignatory = function(args) {
          var model = new AuthorViewSignatoryModel(args);
          var view =  new AuthorViewSignatoryView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.signatorySummary = function() {return model.signatorySummary();};
          this.signatoryViewerySummary = function() {return model.signatoryViewerySummary();};
          this.nameOrEmail = function() {return model.nameOrEmail();};
          this.nameOrEmailOrMobile = function() {return model.nameOrEmailOrMobile();};
          this.status = function() {return model.status();};
          this.destroy = function() {view.destroy();};
};


})(window);
