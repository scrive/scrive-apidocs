/* Signatories model + basic view + signatories attachments
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
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.other;
      return localization.signatoryMessage[signatory.status()];
 },
 hasRemindOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && !signatory.author()
          && signatory.signs()
          && !signatory.undeliveredEmail()
          && !signatory.document().padDelivery();
 },
 hasChangeEmailOption: function() {
   var signatory = this.signatory();
   return    (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredEmail()
          && signatory.document().pending()
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
         var button = $("<a  class='btn-tiny green prepareToSendReminderMail'/>");
         var icon = $("<div/>").addClass(signatory.hasSigned() ? "reminderForSignedIcon" : "reminderForSendIcon");
         var text = signatory.hasSigned() ? signatory.document().process().localization().remindagainbuttontext : localization.reminder.send;
         var textbox = $("<div class='sendLinkText'/>").text(text);
         button.append(icon).append(textbox);
         button.click(function() {
             ConfirmationWithEmail.popup({
                title: signatory.hasSigned() ? signatory.document().process().localization().remindagainbuttontext : localization.reminder.formHead,
                mail: signatory.remindMail(),
                acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                editText: localization.reminder.formOwnMessage,
                rejectText: localization.cancel,
                onAccept: function(customtext) {
                      signatory.remind(customtext).send();
                }
            });
         });
         return button;

  },
  changeEmailOption : function() {
    var signatory = this.model;
    var container = $("<div style='margin-top: 10px'/>");
    var fstbutton = Button.init({
                            size: "tiny",
                            color: "blue",
                            text: localization.changeEmail,
                            onClick: function() {
                                container.empty();
                                var inputwrapper = $("<div class='field float-left' style='width:150px'/>");
                                var input = $("<input type='text' class='fieldvalue' />");
                                input.val(signatory.email());
                                var sndbutton = Button.init({
                                    cssClass: "float-right",
                                    size: "tiny",
                                    color: "blue",
                                    text: localization.send,
                                    onClick: function() { signatory.changeEmail(input.val()).send(); }
                                    });
                                inputwrapper.append(input);
                                container.append(inputwrapper);
                                container.append(sndbutton.input());
                                return false;
                             }
                          });
    container.append(fstbutton.input());
    return container;
  },  
  authorOptions : function() {
    var signatory = this.model.signatory();
    var optionbox = $("<div class='optionbox'/>"); 
    if (this.model.hasRemindOption())
      optionbox.append(this.remidenMailOption());
    if (this.model.hasChangeEmailOption())
      optionbox.append(this.changeEmailOption());
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

      var numspace = $('<div class="spacing numspace" />');
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
      box.append(this.authorOptions());
      box.append(this.statusbox());
      return this;
  }
});

window.AuthorViewSignatory = function(args) {
          var model = new AuthorViewSignatoryModel(args);
          var view =  new AuthorViewSignatoryView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.signatorySummary = function() {return model.signatorySummary();};
          this.nameOrEmail = function() {return model.nameOrEmail();};
          this.status = function() {return model.status();};

};



// Relict, rewrite this  
window.SignatoryStandardView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.render();
    },
    signatorySummary: function() {
          var signatory = this.model;
          var timePP = function(t) {return t.getFullYear() + "-" + (t.getMonth() < 9 ? "0" + (t.getMonth() + 1) : (t.getMonth()+1)) + "-" + t.getDate();}
          var document = signatory.document();
          if (signatory.signdate() != undefined)
               return localization.signatoryMessage.signed + " " + timePP(signatory.signdate());
          else if (signatory.datamismatch() == true)
               return localization.signatoryMessage.datamismatch;
          else if (document.timedout())
               return localization.signatoryMessage.timedout;
          else if (document.canceled() || document.rejected())
               return localization.signatoryMessage.cancelled;
          else if (document.datamismatch())
               return " ";
          else if (signatory.rejecteddate() != undefined)
               return localization.signatoryMessage.rejected + " " + timePP(signatory.rejecteddate());
          else if (signatory.seendate() != undefined)
               return localization.signatoryMessage.seen + " " + timePP(signatory.seendate());
          else if (signatory.readdate() != undefined)
               return localization.signatoryMessage.read + " " + timePP(signatory.readdate());
          else if (signatory.deliveredEmail())
               return localization.signatoryMessage.delivered;
          else
              return localization.signatoryMessage.other;
    },



    changeEmailOption: function() {
        var signatory = this.model;
        var container = $("<div style='margin-top: 10px'/>");
        var fstbutton = Button.init({
                            size: "tiny",
                            color: "blue",
                            text: localization.changeEmail,
                            onClick: function() {
                                container.empty();
                                var inputwrapper = $("<div class='field float-left' style='width:150px'/>");
                                var input = $("<input type='text' class='fieldvalue' />");
                                input.val(signatory.email());
                                var sndbutton = Button.init({
                                    cssClass: "float-right",
                                    size: "tiny",
                                    color: "blue",
                                    text: localization.send,
                                    onClick: function() { signatory.changeEmail(input.val()).send(); }
                                    });
                                inputwrapper.append(input);
                                container.append(inputwrapper);
                                container.append(sndbutton.input());
                                return false;
                             }
                          });
        container.append(fstbutton.input());
        return container;
    },
    giveForSigningOnThisDeviceOption : function() {
                 var signatory = this.model;
                 var button = $("<a  class='giveForSigning'/>");
                 var icon = $("<div class='giveForSigningIcon'/>");
                 var text = localization.pad.signingOnSameDevice;
                 var textbox = $("<div class='sendLinkText'/>").text(text);
                 button.append(icon).append(textbox);
                 button.click(function() {
                         Confirmation.popup({
                                title : localization.pad.signingOnSameDeviceConfirmHeader,
                                content : localization.pad.signingOnSameDeviceConfirmText,
                                acceptText : localization.pad.signingOnSameDevice ,
                                rejectText : localization.cancel,
                                onAccept : function()
                                        {
                                           signatory.addtoPadQueue(function(resp) {
                                               if (resp.error == undefined)
                                                   window.location = signatory.padSigningURL();
                                               else
                                                   FlashMessages.add({
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
        var signatory = this.model;
        var button = $("<a  class='removeFromPad'/>");
        var icon = $("<div class='removeFromPadIcon'/>");
        var text = localization.pad.removeFromPadQueue;
        var textbox = $("<div class='sendLinkText'/>").text(text);
        button.append(icon).append(textbox);
        button.click(function() {
            signatory.removeFromPadQueue().sendAjax( function() { window.location = window.location;}); // Fix at some point not to reload the page
        });
        return button;

    },
    addToPadQueueOption : function() {
                 var signatory = this.model;
                 var button = $("<a  class='addToPad'/>");
                 var icon = $("<div class='addToPadIcon'/>");
                 var text = localization.pad.addToPadQueue;
                 var textbox = $("<div class='sendLinkText'/>").text(text);
                 button.append(icon).append(textbox);
                 button.click(function() {
                         Confirmation.popup({
                                title : localization.pad.addToPadQueueConfirmHeader,
                                content : localization.pad.addToPadQueueConfirmText,
                                acceptText : localization.pad.addToPadQueue ,
                                rejectText : localization.cancel,
                                onAccept : function()
                                        {
                                           signatory.addtoPadQueue(function(resp) {
                                               if (resp.error == undefined)
                                               {    FlashMessages.add({
                                                       content: localization.pad.addToPadQueueAdded,
                                                       color: "green"
                                                    }); }
                                               else {
                                                   FlashMessages.add({
                                                       content: localization.pad.addToPadQueueNotAdded,
                                                       color: "red"
                                                   }); };
                                            }).sendAjax();
                                           window.location = window.location; // Fix at some point not to reload the page
                                           return true;

                                        }
                        });
                        return false;
                 });
                 return button;
    },
    remidenMailOption: function() {
         var signatory = this.model;
         var button = $("<a  class='btn-tiny green prepareToSendReminderMail'/>");
         var icon = $("<div/>").addClass(signatory.hasSigned() ? "reminderForSignedIcon" : "reminderForSendIcon");
         var text = signatory.hasSigned() ? signatory.document().process().localization().remindagainbuttontext : localization.reminder.send;
         var textbox = $("<div class='sendLinkText'/>").text(text);
         button.append(icon).append(textbox);
         button.click(function() {
             ConfirmationWithEmail.popup({
                title: signatory.hasSigned() ? signatory.document().process().localization().remindagainbuttontext : localization.reminder.formHead,
                mail: signatory.remindMail(),
                acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
                editText: localization.reminder.formOwnMessage,
                rejectText: localization.cancel,
                onAccept: function(customtext) {
                      signatory.remind(customtext).send();
                }
            });
         });
         return button;

    },
    render: function() {
        var signatory = this.model;
        $(this.el).addClass("signViewBodyRight");
        $(this.el).children().detach();
        var container = $("<div class='signViewBodyRightTextContainer'/>");
        $(this.el).append(container);
        var header = $("<div class='header'/>").text(signatory.name());
        container.append(header);
        var fieldsbox = $("<div class='signViewBodyForms'/>");
        _.each(signatory.fields(), function(field) {
            if (field.name() == "fstname" ||
                field.name() == "sndname" ||
                field.name() == "email")
            return;
            if (field.canBeIgnored() || field.isSignature() || field.isCheckbox())
            return;
            var fieldview = new FieldStandardView(
            { model: field,
              el: $("<div/>")
            });
            fieldsbox.append($(fieldview.el));
        });
        container.append(fieldsbox);
        var emailview = new FieldStandardView(
            { model: signatory.emailField(),
              el: $("<div/>")
            });
        container.append($(emailview.el));

        var textsummary = $("<div class='text'/>");
        if (signatory.signs()) {
            textsummary.append($("<div class='icon status'/>").addClass(signatory.status()));
            textsummary.append($("<span class='textstatus'/>").text(this.signatorySummary()));
        }
        else {
            textsummary.text(signatory.document().process().localization().authorissecretarytext);
        }
        container.append(textsummary);


       if ((signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin()) &&
               !signatory.author() &&
               ((signatory.document().signingInProcess() && signatory.canSign()) ||
                   signatory.document().closed()) && !signatory.document().padDelivery())
          container.append(this.remidenMailOption());

        if (signatory.undeliveredEmail() && signatory.document().currentViewerIsAuthor() && signatory.document().pending())
          container.append(this.changeEmailOption());

        if ((signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
            && signatory.document().signingInProcess()
            && signatory.canSign()
            && signatory.document().padDelivery() && !signatory.author()) {
                  if (!signatory.author() && BrowserInfo.isPadDevice())
                      container.append(this.giveForSigningOnThisDeviceOption());
                  if (!BrowserInfo.isPadDevice()) {
                    if (signatory.inpadqueue())
                      container.append(this.removeFromPadQueueOption());
                    else
                      container.append(this.addToPadQueueOption());
                  }
           }


        return this;
    }
});

})(window);
