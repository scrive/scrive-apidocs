/* Signatories model + basic view + signatories attachments
 */


(function(window) {

window.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name: "",
        description: "",
        loading: false,
        reviewed: false
    },
    initialize: function(args) {
        if (args.file != undefined) {
            var document = args.signatory.document();
            this.set({"file": new File(_.extend(args.file, {document: document,
                                                            documentid: document.documentid()
                                                           }))});
        }
        return this;
    },
    file: function() {
        return this.get("file");
    },
    setFile: function(file) {
        return this.set({'file': file, 'reviewed':false});
    },
    description: function() {
        return this.get("description");
    },
    name: function() {
        return this.get("name");
    },
    hasFile: function() {
        return this.file() != undefined;
    },
    isReviewed: function() {
        return this.get('reviewed');
    },
    review: function() {
        this.set({'reviewed':true}, {silent : true});
        this.trigger('change');
        return this;
    },
    signatory: function() {
        return this.get("signatory");
    },
    loading: function() {
        this.set({loading: true});
    },
    notLoading: function() {
        this.set({loading: false});
    },
    isLoading: function() {
        return this.get('loading');
    },
    document: function() {
        return this.signatory().document();
    },
    draftData: function() {
        return {
              name: this.name(),
              description: this.description()
        };
    }
});

window.SignatoryAttachmentUploadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  apiURL: function() {
    var path = document.location.pathname.split("/");
    return "/api/document/" + path[2] + "/signatory/" + path[3] + "/attachment/" + this.model.name() + "/file" + this.model.document().viewer().urlPart();
  },
  removeLink: function() {
    var attachment = this.model;
    var deleteurl = this.apiURL();
    var removelink = $("<a href='' />").text(localization.deletePDF);
    removelink.click(function() {
        attachment.loading();
        $.ajax(deleteurl, {
          type: 'DELETE',
          success: function(d) {
            attachment.unset('file');
            attachment.notLoading();
          },
          error: function() {
            attachment.notLoading();
            console.log("error");
          }
        });
        return false;
      });
    return removelink;
  },
    // Review attachment change
    // please delete on or after May 1, 2012
    // -- Eric
    /*
    fileLink: function() {
        return $("<a target='_blank'/>").text(localization.reviewPDF).attr("href", this.model.file().downloadLink());
    },
    */
  uploadButton: function() {
    var attachment = this.model;
    var uploadurl = this.apiURL();
    return UploadButton.init({
      width: 200,
      name: "file",
      text: localization.signatoryAttachmentUploadButton,
      submitOnUpload: true,
      showLoadingDialog: false,
      onClick: function() {
        attachment.loading();
      },
      onError: function() {
        attachment.notLoading();
        attachment.trigger('change');
      },
      submit: new Submit({
        method: "POST",
        url: uploadurl,
        attachname: attachment.name(),
        sigattachment: "YES",
        ajax: true,
        expectedType: 'json',
        onSend: function() {
          attachment.loading();
        },
        ajaxerror: function(d, a) {
          if (a === 'parsererror') // file too large
            FlashMessages.add({content: localization.fileTooLarge, color: "red"});
          else
            FlashMessages.add({content: localization.couldNotUpload, color: "red"});
          attachment.notLoading();
        },
        ajaxsuccess: function(d) {
          if (d) {
            attachment.setFile(new File(_.extend(d.file, {document: attachment.signatory().document() })));
            attachment.notLoading();
          }
        }
      })
    });
  },
  reviewButton: function() {
      var model = this.model;
      var button = Button.init({color: "green", text: localization.reviewPDF, width: 90, size:'small', onClick: function() {
          model.review();
          window.open(model.file().downloadLink(), '_blank');
          }});
      return button;
  },
  render: function() {
      var attachment = this.model;
      var container = $("<div class='upload' />");
      if (attachment.get('loading')) {
          container.append($("<img class='loading'>").attr('src', "/img/wait30trans.gif"));
      } else if (attachment.hasFile()) {
          container.append($("<div class='icon' />"));
          var label = $("<div class='file' />");
          label.append($("<div class='name' />").text(this.model.file().name() + ".pdf"));
          var actions = $("<div />");
          //review button change
          //please delete this line after May 1, 2012
          // -- Eric
          //actions.append($("<div class='action' />").append(this.fileLink()));
          if (!attachment.signatory().hasSigned()) {
              actions.append($("<div class='action' />").append(this.removeLink()));
          }
          actions.append($("<div class='clearfix' />"));
          label.append(actions);
          label.append($("<div class='clearfix' />"));
          container.append(label);
          var buttonbox = $('<div class="buttonbox" />');
          buttonbox.append(this.reviewButton().input());
          container.append(buttonbox);

      } else {
          container.append(this.uploadButton().input());
      }
      container.append($("<div class='clearfix' />"));

      $(this.el).empty();
      $(this.el).append(container);

      return this;
  }
});

window.SignatoryAttachmentView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.view = this;
    this.render();
  },
  uploadView: function() {
    return new SignatoryAttachmentUploadView({
      model: this.model,
      el: $("<div />")
    });
  },
  render: function() {
    $(this.el).empty();

    var firstcol = $("<div class='first column'/>");
    firstcol.append($("<div class='name' />").text(this.model.name()));
    firstcol.append($("<div class='description' />").text(this.model.description()));

    var container = $("<div class='item' />");
    container.append(firstcol);
    this.uploadElems = $("<div class='second column'/>").append($(this.uploadView().el));
    container.append(this.uploadElems);

    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);
    return this;
  }
});

window.UploadedSignatoryAttachmentView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  fileLink: function() {
    return $("<a target='_blank'/>").text(localization.reviewPDF).attr("href", this.model.file().downloadLink());
  },
  render: function() {
    $(this.el).empty();

    var container = $("<div class='item' />");
    container.append($("<div class='icon' />"));

    var label = $("<div class='label' />");
    label.append($("<div class='name' />").text(this.model.name() + ".pdf"));
    label.append($("<div class='from' />").text(this.model.signatory().name()));
    var secondcol = $("<div class='column'/>");
    if (this.model.hasFile()) {
      label.append(this.fileLink());
    } else {
      label.text(localization.waitingForAttachment);
    }

    container.append(label);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);
    return this;
  }
});

window.Signatory = Backbone.Model.extend({
    defaults: {
        id: 0,
        signed: false,
        signs: false,
        author: false,
        fields: [{name: "fstname",   type : "standard"},
                 {name: "sndname",   type : "standard"},
                 {name: "email",     type : "standard"},
                 {name: "sigco",     type : "standard"},
                 {name: "sigpersnr", type : "standard"},
                 {name: "sigcompnr", type : "standard"},
                 {name: "signature", type : "signature"}
        ],
        current: false,
        attachments: [],
        signorder: 1,
        csv: undefined,
        saved: false,
        ispadqueue : false
    },

    initialize: function(args) {
        var signatory = this;
        var extendedWithSignatory = function(hash) {
                    hash.signatory = signatory;
                    return hash;
        };
        var fields = _.map(args.fields, function(field) {
                return new Field(extendedWithSignatory(field));
        });
        // Make sure that all basic fields are initiated
        for(var i = 0; i < this.defaults.fields.length; i++)
        {   var isSet = false;
            for(var j=0;j < args.fields.length;j++ )
                if (this.defaults.fields[i].name == args.fields[j].name)
                    isSet = true;
            if (!isSet)
                fields.push(new Field(extendedWithSignatory(this.defaults.fields[i])));
        }

        var attachments = _.map(args.attachments, function(attachment) {
                return new SignatoryAttachment(extendedWithSignatory(attachment));
        });
        this.set({"fields": fields,
                  "attachments": attachments
        });

        this.bind("change", function() {signatory.document().trigger("change:signatories")});

    },
    document: function() {
        return this.get("document");
    },
    saveurl: function() {
      return "/s/" + this.document().id + "/" + this.signatoryid() + "/" + this.document().viewer().magichash();
    },
    signIndex: function() {
        var allSignatories = this.document().signatories();
        var index = 1;
        for (var i = 0; i < allSignatories.length; i++)
        {
            if (allSignatories[i] === this)
                return index;
            if (allSignatories[i].signs()) index++;
        }
        return 0;
    },
    signatoryid: function() {
        return this.get("id");
    },
    author: function() {
        return this.get("author");
    },
    current: function() {
        return this.get("current");
    },
    status: function() {
        return this.get("status");
    },
    fields: function() {
        return this.get("fields");
    },
    emailField : function() {
        return this.field("email", "standard");
    },
    fstnameField : function() {
        return this.field("fstname", "standard");
    },
    sndnameField : function() {
        return this.field("sndname", "standard");
    },
    companyField : function() {
        return this.field("sigco", "standard");
    },
    companynumberField : function() {
        return this.field("sigcompnr", "standard");
    }, 
    personalnumberField : function() {
        return this.field("sigpersnr", "standard");
    },
    email: function() {
        return this.emailField().value();
    },
    fstname: function() {
        return this.fstnameField().value();
    },
    sndname: function() {
        return this.sndnameField().value();
    },
    personalnumber : function() {
        return this.personalnumberField() != undefined ? (this.personalnumberField().value() != undefined ? this.personalnumberField().value() : "") : "";
    },
    company: function() {
        return this.companyField() != undefined ? (this.companyField().value() != undefined ? this.companyField().value() : "") : "";
    },
    companynumber: function() {
        return this.companynumberField() != undefined ? (this.companynumberField().value() != undefined ? this.companynumberField().value() : "") : "";
    },
    field: function(name, type) {
        var fields = this.fields();
        for (var i = 0; i < fields.length; i++)
            if (fields[i].name() == name && fields[i].type() == type)
                return fields[i];
    },
    readyFields: function() {
        return _.filter(this.fields(), function(f) {return f.isReady();});
    },
    customFields: function() {
        var cf = new Array();
        var fields = this.fields();
        for (var i = 0; i < fields.length; i++)
            if (fields[i].isCustom()) cf.push(fields[i]);
        return cf;
    },  
    name: function() {
        var name = this.fstname() + " " + this.sndname();
        if (name != undefined && name != " ")
            return name;
        else
            return "";
    },
    smartname: function() {
        if (this.current())
         return localization.you;
        else
         return this.nameOrEmail();
    },
    nameOrEmail: function() {
         if (this.name() != "")
         return this.name();
        else
         return this.email();
    },
    saved: function() {
      return this.get("saved");
    },
    signdate: function() {
        return this.get("signdate");
    },
    datamismatch: function() {
        return this.get("datamismatch");
    },
    rejecteddate: function() {
        return this.get("rejecteddate");
    },
    seendate: function() {
        return this.get("seendate");
    },
    readdate: function() {
        return this.get("readdate");
    },
    deliveredEmail: function() {
        return this.get("deliveredEmail");
    },
    undeliveredEmail: function() {
          return this.get("undeliveredEmail");
    },
    signorder: function() {
         return this.get("signorder");
    },
    setSignOrder: function(i) {
         this.set({signorder: parseInt(i + "")});
    },
    signs: function() {
         return this.get("signs");
    },
    makeSignatory: function() {
        this.set({ signs: true });
        this.trigger("change:role");
    },
    makeViewer: function() {
        this.set({signs: false});
        if (this.signature() != undefined)
           this.signature().removeAllPlacements();
        this.trigger("change:role");
    },
    hasSigned: function() {
        return this.signdate() != undefined;
    },
    attachments: function() {
        return this.get("attachments");
    },
    addAttachment: function(att) {
        this.get("attachments").push(att);
        this.document().trigger("change:attachments");
    },
    clearAttachments: function() {
        this.set({attachments: []});
    },
    canSign: function() {
        var canSign = this.document().signingInProcess() &&
            this.signs() &&
            !this.hasSigned() &&
            this.signorder() == this.document().signorder();
        return canSign;
    },
    ableToSign : function() { // Same as can sign but does not check document state.
        return   this.signs() &&
                !this.hasSigned() &&
                 this.signorder() == this.document().signorder();
    },
    canPadSignQuickSign : function() {
       return this.document().padAuthorization() && this.canSign() && !this.document().hasAnyAttachments() && this.allFieldsButSignatureDontRequiredFilling();
    },
    allAttachemntHaveFile: function() {
        return _.all(this.attachments(), function(attachment) {
            return attachment.hasFile();
        });
    },
    allFieldsReadyForSign: function() {
        return _.all(this.fields(), function(field) {
            return field.readyForSign();
        });
    },
    allFieldsButSignatureDontRequiredFilling: function() {
        return _.all(this.fields(), function(field) {
            return (field.isClosed() || field.placements().length == 0) || field.isSignature();
        });
    },
    signatureReadyForSign: function() {
        return this.signature() == undefined || this.signature().readyForSign();
    },
    signature: function() {
        return this.field("signature", "signature");
    },
    remind: function(customtext) {
        return new Submit({
              url: "/resend/" + this.document().documentid() + "/" + this.signatoryid(),
              method: "POST",
              customtext: customtext
          });
    },
    addtoPadQueue : function(callback) {
        return new Submit({
              url: "/padqueue/add/"+ this.document().documentid() + "/" + this.signatoryid(),
              method: "POST",
              ajax : true,
              ajaxsuccess : callback
          });
    },
    removeFromPadQueue : function(callback) {
       return new Submit({
              url: "/padqueue/clear",
              method: "POST",
              ajax : true,
              ajaxsuccess : callback
       });
    },
    reject: function(customtext) {
        return new Submit({
              url: "/s/" + this.document().documentid() + "/" + this.document().viewer().signatoryid(),
              magichash: this.document().viewer().magichash(),
              method: "POST",
              customtext: customtext,
              reject: "YES"
          });
    },
    padSigningURL : function() {
        return "/padqueue";
    },
    changeEmail: function(email) {
        return new Submit({
                url: "/changeemail/" + this.document().documentid() + "/" + this.signatoryid(),
                method: "POST",
                email: email
         });
    },
    remindMail: function() {
        return new Mail({
                document: this.document(),
                signatory: this,
                type: "remind",
                editWidth: (this.canSign() && !this.hasSigned()) ? 300 : undefined
        });
    },
    rejectMail: function() {
        return new Mail({
                        document: this.document(),
                        signatory: this,
                        type: "reject"
                       });
    },
    addNewField : function(t) {
        var field = this.newField(t);
        this.addField(field);
        return field;
    },
    addNewCustomField: function() {
       return this.addNewField("custom");
    },
    newCheckbox: function() {
       var checkbox =  this.author() ? this.newField("checkbox-optional") : this.newField("checkbox-obligatory");
       //var allfields = _.flatten(_.map(this.document().signatories(), function(s) {return s.fields();}));
       //var i = 1;
       //while(_.any(allfields, function(f) {f.name() == (localization.designview.checkboxes.checkbox + " " + i)})) i++;
       //checkbox.setName("Checkbox " + i);
       return checkbox;
    },
    newField : function(t) {
        return new Field({signatory: this, fresh: true, type : t});
    },
    addField : function(f) {
        var fields = this.fields();
        fields.push(f);
        this.set({"fields": fields});
        this.trigger("change:fields");
    },
    deleteField: function(field) {
       var newfields = new Array();
       for (var i = 0; i < this.fields().length; i++)
          if (field !== this.fields()[i])
             newfields.push(this.fields()[i]);
       this.set({fields: newfields});

    },
    csv: function() {
        return this.get("csv");
    },
    isCsv: function() {
        return this.csv() != undefined;
    },
    makeCsv: function(csv) {
         this.set({"csv": csv});
         this.trigger("change:csv");

    },
    inpadqueue : function() {
       return this.get("inpadqueue");
    },
    removed : function() {
        this.trigger("removed"); 
        this.off();
    },
    hasUser: function() {
        return this.get("hasUser");
    },
    draftData : function() {
        return {
              fields: _.map(this.readyFields(), function(field) {
                  return field.draftData();
              }),
              author: this.author(),
              signs: this.signs(),
              signorder: this.signorder(),
              attachments: _.map(this.attachments(), function(att) {
                  return att.draftData();
              }),
              csv: this.csv()

        };
    }

});

window.SignatoryStandardView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.render();
    },
    signatorySummary: function() {
          var signatory = this.model;
          var document = signatory.document();
          if (signatory.signdate() != undefined)
               return localization.signatoryMessage.signed + " " + this.model.signdate();
          else if (signatory.datamismatch() == true)
               return localization.signatoryMessage.datamismatch;
          else if (document.timedout())
               return localization.signatoryMessage.timedout;
          else if (document.canceled())
               return localization.signatoryMessage.cancelled;
          else if (document.datamismatch())
               return " ";
          else if (signatory.rejecteddate() != undefined)
               return localization.signatoryMessage.rejected + " " + this.model.rejecteddate();
          else if (signatory.seendate() != undefined)
               return localization.signatoryMessage.seen + " " + this.model.seendate();
          else if (signatory.readdate() != undefined)
               return localization.signatoryMessage.read + " " + this.model.readdate();
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
         var text = signatory.hasSigned() ? signatory.document().process().remindagainbuttontext() : localization.reminder.send;
         var textbox = $("<div class='sendLinkText'/>").text(text);
         button.append(icon).append(textbox);
         button.click(function() {
             ConfirmationWithEmail.popup({
                title: signatory.hasSigned() ? signatory.document().process().remindagainbuttontext() : localization.reminder.formHead,
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
            if (signatory.undeliveredEmail() && signatory.seendate() == undefined) textsummary.append("<span style='color:#000000;position:relative;left:-4px;'>!</span> ");
            textsummary.append($("<span class='textstatus'/>").text(this.signatorySummary()));
        }
        else {
            textsummary.text(signatory.document().process().authorissecretarytext());
        }
        container.append(textsummary);


       if ((signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin()) &&
               !signatory.author() &&
               ((signatory.document().signingInProcess() && signatory.canSign()) ||
                   signatory.document().closed()) && !signatory.document().padAuthorization())
          container.append(this.remidenMailOption());

        if (signatory.undeliveredEmail() && signatory.document().currentViewerIsAuthor() && signatory.document().pending())
          container.append(this.changeEmailOption());

        if ((signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
            && signatory.document().signingInProcess()
            && signatory.canSign()
            && signatory.document().padAuthorization() && !signatory.author()) {
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
