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
          && (signatory.document().signingInProcess() || signatory.document().closed())
          && !signatory.undeliveredEmail()
          && !signatory.document().padDelivery();
 },
 hasChangeEmailOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredEmail()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
 },
 hasPadOptions : function() {
   var signatory = this.signatory();
   return    signatory.document().currentViewerIsAuthor()
          && signatory.document().signingInProcess()
          && signatory.canSign()
          && signatory.document().padDelivery()
 },
 hasGiveForSigningOnThisDeviceOption : function() {
   return this.hasPadOptions() && this.signatory().author() && BrowserInfo.isPadDevice()
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
         || this.hasGiveForSigningOnThisDeviceOption()
         || this.hasAddToPadQueueOption()
         || this.hasRemoveFromPadQueueOption()

 }
});

var AuthorViewSignatoryView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
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
  remidenMailOption: function() {
         var signatory = this.model.signatory();
         var button = $("<label class='clickable prepareToSendReminderMail'/>");
         var icon = $("<div/>").addClass(signatory.hasSigned() ? "reminderForSignedIcon" : "reminderForSendIcon");
         var text = signatory.hasSigned() ? signatory.document().process().processLocalization().remindagainbuttontext : localization.reminder.send;
         var textbox = $("<span/>").text(text);
         button.append(icon).append(textbox);
         button.click(function() {
             mixpanel.track('Click send reminder',
                            {'Signatory index':signatory.signIndex()});
             ConfirmationWithEmail.popup({
                title: signatory.hasSigned() ? signatory.document().process().processLocalization().remindagainbuttontext : localization.reminder.formHead,
                mail: signatory.remindMail(),
                acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                editText: localization.reminder.formOwnMessage,
                rejectText: localization.cancel,
                onAccept: function(customtext) {
                    trackTimeout('Accept', 
                                 {'Accept' : 'send reminder',
                                  'Signatory index' : signatory.signIndex()},
                                 function() {
                                     signatory.remind(customtext).send();
                                 });
                }
            });
         });
         return button;

  },
  changeEmailOption : function() {
    var signatory = this.model.signatory();
    var container = $("<div class='change-email-box'/>");
    var fstbutton = Button.init({
                            size: "tiny",
                            color: "blue",
                            text: localization.changeEmail,
                            onClick: function() {
                                mixpanel.track('Click change email',
                                               {'Signatory index':signatory.signIndex()});
                                var input = $("<input type='text'/>");
                                input.val(signatory.email());
                                var sndbutton = Button.init({
                                    size: "tiny",
                                    color: "blue",
                                    text: localization.send,
                                    onClick: function() { 
                                        trackTimeout('Accept',
                                                     {'Signatory index':signatory.signIndex(),
                                                      'Accept' : 'change email'},
                                                     function() {
                                                         signatory.changeEmail(input.val()).send(); 
                                                     });
                                    }
                                    });
                                container.empty().append(input).append(sndbutton.input());
                                return false;
                             }
                          });
    container.append(fstbutton.input());
    return container;
  },
  giveForSigningOnThisDeviceOption : function() {
                 var signatory = this.model.signatory();
                 var button = $("<label  class='clickable giveForSigning'/>");
                 var icon = $("<div class='giveForSigningIcon'/>");
                 var text = localization.pad.signingOnSameDevice;
                 var textbox = $("<span/>").text(text);
                 button.append(icon).append(textbox);
                 button.click(function() {
                     mixpanel.track('Click give for signing',
                                    {'Signatory index':signatory.signIndex()});
                         Confirmation.popup({
                                title : localization.pad.signingOnSameDeviceConfirmHeader,
                                content : localization.pad.signingOnSameDeviceConfirmText,
                                acceptText : localization.pad.signingOnSameDevice ,
                                rejectText : localization.cancel,
                                onAccept : function()
                             {
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
                 });
                 return button;
    },
    removeFromPadQueueOption :  function() {
        var signatory = this.model.signatory();
        var button = $("<label class='clickable removeFromPad'/>");
        var icon = $("<div class='removeFromPadIcon'/>");
        var text = localization.pad.removeFromPadQueue;
        var textbox = $("<span/>").text(text);
        button.append(icon).append(textbox);
        button.click(function() {
            mixpanel.track('Click remove from pad queue',
                           {'Signatory index':signatory.signIndex()});
            signatory.removeFromPadQueue().sendAjax( function() { window.location = window.location;});
        });
        return button;

    },
    addToPadQueueOption : function() {
                 var signatory = this.model.signatory();
                 var button = $("<label  class='clickable addToPad'/>");
                 var icon = $("<div class='addToPadIcon'/>");
                 var text = localization.pad.addToPadQueue;
                 var textbox = $("<span/>").text(text);
                 button.append(icon).append(textbox);
                 button.click(function() {
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
                                           return true;

                                        }
                        });
                        return false;
                 });
                 return button;
    },
  authorOptions : function() {
    var signatory = this.model.signatory();
    var optionbox = $("<div class='optionbox'/>"); 
    if (this.model.hasRemindOption())
      optionbox.append(this.remidenMailOption());
    if (this.model.hasChangeEmailOption())
      optionbox.append(this.changeEmailOption());
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
      var name      = $('<div class="name" />').text(signatory.name());
      var company   = $('<div class="company" />').text(signatory.company());
      titleinfo.append(name).append(company);
      box.append(titleinfo);

      var inner   = $('<div class="inner spacing" />');

      var face    = $('<div class="face" />');

      var numspace = $('<div class="details" />');
      var orgnum  = $('<div class="orgnum field" />').text(localization.docsignview.companyNumberLabel + ": "
                                                           + (signatory.companynumber().trim() || localization.docsignview.notEntered))
          .attr('title', signatory.companynumber());
      var persnum = $('<div class="persnum field" />').text(localization.docsignview.personalNumberLabel + ": "
                                                            + (signatory.personalnumber().trim() || localization.docsignview.notEntered))
        .attr('title', signatory.personalnumber());
      var contactspace = $('<div class="spacing contactspace" />');
      var email   = $('<div class="email field" />').text(signatory.email()).attr('title', signatory.email());

      numspace.append(orgnum);
      numspace.append(persnum);

      numspace.append(email);

      inner.append(face);

      inner.append(numspace);
      inner.append(contactspace);
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
          this.status = function() {return model.status();};
};


})(window);
