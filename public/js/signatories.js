/* Signatories model + basic view + signatories attachments
 */


(function(window) {

window.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name: "",
        description: "",
        loading: false
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
        return this.set({'file': file});
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

window.Signatory = Backbone.Model.extend({
    defaults: {
        id: 0,
        signed: false,
        signs: false,
        author: false,
        fields: [{name: "fstname",   type : "standard"},
                 {name: "sndname",   type : "standard"},
                 {name: "email",     type : "standard"},
                 {name: "mobile",    type : "standard"},
                 {name: "sigco",     type : "standard"},
                 {name: "sigcompnr", type : "standard"}
        ],
        current: false,
        attachments: [],
        signorder: 1,
        csv: undefined,
        saved: false,
        ispadqueue : false,
        authentication: "standard",
        delivery: "email"
    },

    initialize: function(args) {
        var signatory = this;
        _.bindAll(signatory);
        var extendedWithSignatory = function(hash) {
                    hash.signatory = signatory;
                    return hash;
        };
        var fields = _.map(signatory.get('fields'), function(field) {
            var f = new Field(extendedWithSignatory(field));
            if(f.obligatory() && f.shouldbefilledbysender())
                f.authorObligatory = 'sender';
            else if(f.obligatory())
                f.authorObligatory = 'signatory';
            else
                f.authorObligatory = 'optional';
            return f;
        });

        var attachments = _.map(args.attachments, function(attachment) {
                return new SignatoryAttachment(extendedWithSignatory(attachment));
        });
        signatory.set({"fields": fields,
                       "attachments": attachments
                      });

        signatory.bindBubble();
    },
    document: function() {
        return this.get("document");
    },
    saveurl: function() {
      return "/s/" + this.document().id + "/" + this.signatoryid();
    },
    signIndex: function() {
        var allSignatories = this.document().signatories();
        var index = 1;
        for (var i = 0; i < allSignatories.length; i++) {
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
    mobileField : function() {
        return this.field("mobile", "standard");
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
    mobile: function() {
        return this.mobileField().value();
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
        for (var i = 0; i < fields.length; i++) {
            if ((name == undefined || fields[i].name() == name) &&
                (type == undefined || fields[i].type() == type)) {
                return fields[i];
            }
        }
    },
    fieldsByType: function(type) {
        return _.filter(this.fields(), function(field) {
            return field.type() == type;
        });
    },
    readyFields: function() {
        return _.filter(this.fields(), function(f) {return f.isReady() && !f.isFake();});
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
    nameInDocument : function() {
       var signatory = this;
       var process = signatory.document().process();
       if (signatory.isCsv())
        return localization.csv.title;
       if (signatory.signs() &&  signatory.author())
        return process.processLocalization().authorsignatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else if(signatory.author())
        return process.processLocalization().authorname;
       else if (signatory.signs())
        return process.processLocalization().signatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
       else
        return process.processLocalization().nonsignatoryname;
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
        if (this.get("signdate"))
          return new Date(Date.parse(this.get("signdate")));
        return undefined;
    },
    datamismatch: function() {
        return this.get("datamismatch");
    },
    rejecteddate: function() {
        if (this.get("rejecteddate"))
          return new Date(Date.parse(this.get("rejecteddate")));
        return undefined;
    },
    seendate: function() {
        if (this.get("seendate"))
          return new Date(Date.parse(this.get("seendate")));
        return undefined;

    },
    readdate: function() {
        if (this.get("readdate"))
          return new Date(Date.parse(this.get("readdate")));
        return undefined;
    },
    deliveredInvitation: function() {
        return this.get("deliveredInvitation");
    },
    undeliveredInvitation: function() {
          return this.get("undeliveredInvitation");
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
    signsuccessredirect : function() {
          return this.get("signsuccessredirect");
    },
    makeSignatory: function() {
      var isAuthor = this.author();
      var authorNotSignsMode = this.document().authorNotSignsMode();
      this.set({ signs: true });
      if( isAuthor && authorNotSignsMode) {
        /* We need to renumber all other signatories from group 1 to group 2 */
        _.each(this.document().signatories(),function(sig) {
          if( !sig.author()) {
            sig.setSignOrder(2);
          }
        });
      }
      if(!isAuthor && this.document().author().signs() && this.document().author().signorder() == 1 )
         this.setSignOrder(2);
      else
         this.setSignOrder(1);
      this.trigger("change:role");
      this.document().fixSignorder();
    },
    makeViewer: function() {
      var authorSignsFirstMode = this.document().authorSignsFirstMode();
      this.set({signs: false});
      _.each(this.signatures(),function(s) {
         s.removeAllPlacements();
      });
      _.each(this.fields(), function(field) {
          if (field.isCheckbox()) field.remove();
      });

      if( this.author() && authorSignsFirstMode) {
        /* We need to renumber all other signatories from group 2 to group 1 */
        _.each(this.document().signatories(),function(sig) {
          sig.setSignOrder(1);
        });
      }
      this.trigger("change:role");
      this.document().fixSignorder();
    },
    hasSigned: function() {
        return this.signdate() != undefined;
    },
    attachments: function() {
        return this.get("attachments");
    },
    removeAttachment : function(a) {
        this.set({"attachments": _.without(this.attachments(),[a]) });
        this.document().trigger('change:attachments');
    },
    addAttachment: function(att) {
        this.get("attachments").push(att);
    },
    clearAttachments: function() {
        this.set({attachments: []});
    },
    reachedBySignorder : function() {
        return this.signorder() <= this.document().signorder();
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
    isViewer : function() {
        return !this.author() && !this.signs();
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
    signatureReadyForSign: function() {
        return _.all(this.signatures(), function(signature) {
            return signature.readyForSign();
        });
    },
    signatures: function() {
        return this.fieldsByType("signature");
    },
    hasSignatureField : function() {
      return this.signatures().lenght > 0 ;
    },
    hasPlacedSignatures: function() {
        return _.any(this.signatures(), function(signature) {
            return signature.hasPlacements();
        });
    },
    anySignatureHasImageOrPlacement : function() {
      return _.any(this.signatures(), function (field) {
           return field.value() != "" || field.hasPlacements();
       });
    },
    standardAuthentication: function() {
          return this.get("authentication") == "standard";
    },
    elegAuthentication: function() {
          return this.get("authentication") == "eleg";
    },
    emailDelivery: function() {
          return this.get("delivery") == "email";
    },
    padDelivery : function() {
          return this.get("delivery") == "pad";
    },
    mobileDelivery : function() {
          return this.get("delivery") == "mobile";
    },
    emailMobileDelivery : function() {
          return this.get("delivery") == "email_mobile";
    },
    apiDelivery : function() {
          return this.get("delivery") == "api";
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
              url: "/api/frontend/padqueue/add/"+ this.document().documentid() + "/" + this.signatoryid(),
              method: "POST",
              ajax : true,
              ajaxsuccess : callback
          });
    },
    removeFromPadQueue : function(callback) {
       return new Submit({
              url: "/api/frontend/padqueue/clear",
              method: "POST",
              ajax : true,
              ajaxsuccess : callback
       });
    },
    reject: function(customtext) {
        return new Submit({
              url: "/api/frontend/reject/" + this.document().documentid() + "/" + this.document().viewer().signatoryid(),
              method: "POST",
              ajax : true,
              customtext: customtext
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
    changePhone: function(phone) {
        return new Submit({
                url: "/changephone/" + this.document().documentid() + "/" + this.signatoryid(),
                method: "POST",
                phone: phone
         });
    },
    remindMail: function() {
        return new Mail({
                document: this.document(),
                signatory: this,
                type: "remind",
                editWidth: (this.canSign() && !this.hasSigned()) ? 300 : 540
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
       return this.addNewField("custom", true);
    },
    newField : function(t,f) {
        return new Field({signatory: this, fresh: (f != undefined ? f : true) , type : t});
    },
    addField : function(f) {
        this.fields().push(f);
        this.trigger("change change:fields");
    },
    deleteField: function(field) {
        this.set({fields : _.without(this.fields(), field)});
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
    signsuccessredirect : function() {
        return this.get("signsuccessredirect");
    },
    inpadqueue : function() {
       return this.get("inpadqueue");
    },
    removed : function() {
        this.isRemoved = true;
        this.off();
        this.trigger("removed");
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
              signorder: this.signs() ? this.signorder() : 1,
              attachments: _.map(this.attachments(), function(att) {
                  return att.draftData();
              }),
              csv: this.csv(),
              signsuccessredirect : this.signsuccessredirect(),
              authentication: this.authentication(),
              delivery: this.delivery()

        };
    },
    delivery: function() {
        return this.get('delivery');
    },
    setDelivery: function(d) {
        // TODO: check values of d
        // email, pad, api
        this.set({delivery:d});
        return this;
    },
    authentication: function() {
        return this.get('authentication');
    },
    setAuthentication: function(a) {
        // TODO: check values of a
        // standard, eleg
        this.set({authentication:a});
        return this;
    },
    hasProblems: function(forSigning) {
        return this.hasFieldProblems(forSigning);
    },
    hasFieldProblems: function(forSigning) {
        return _.some(this.fields(), function(field) {
            return !field.isValid(forSigning) || field.hasNotReadyPlacements();
        });
    },
    role: function() {
        if(this.signs())
            return 'signatory';
        else
            return 'viewer';
    },
    // called when creating a multisend signatory
    giveStandardFields: function() {
        var signatory = this;
        var stdfields = [{name: "fstname",   type : "standard"},
                         {name: "sndname",   type : "standard"},
                         {name: "email",     type : "standard"},
                         {name: "sigco",     type : "standard"},
                         {name: "sigpersnr", type : "standard"},
                         {name: "sigcompnr", type : "standard"},
                         {name: "mobile"   , type : "standard"}
                        ];

        var fields = _.map(stdfields, function(f) {
            f.signatory = signatory;
            return new Field(f);
        });
        signatory.set({fields:fields});
        return signatory;
    },
    ensurePersNr: function() {
        var signatory = this;
        var pn = signatory.personalnumberField();
        if(signatory.needsPersonalNumber()) {
            if(!pn) {
                var f = new Field({name:'sigpersnr',
                                   type: 'standard',
                                   obligatory: true,
                                   shouldbefilledbysender: signatory.author(),
                                   signatory: signatory})
                f.addedByMe = true;
                signatory.addField(f);
            } else {
                pn.makeObligatory();
            }
        } else {
            if(pn && pn.addedByMe && pn.value() === '' && !pn.hasPlacements()) {
                signatory.deleteField(pn);
            } else if(pn && pn.authorObligatory == 'sender') {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender(true);
            } else if(pn && pn.authorObligatory == 'signatory') {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender(false);
            } else if(pn && pn.authorObligatory == 'optional') {
                pn.makeOptional();
            }
        }
    },
    needsPersonalNumber: function() {
        return this.elegAuthentication();
    },
    ensureMobile: function() {
        var signatory = this;
        var pn = signatory.mobileField();
        if(signatory.needsMobile()) {
            if(!pn) {
                var f = new Field({name:'mobile',
                                   type: 'standard',
                                   obligatory: true,
                                   shouldbefilledbysender: true,
                                   signatory: signatory});
                f.addedByMe = true;
                signatory.addField(f);
            } else {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender(true);
            }
        } else {
            if(pn && pn.addedByMe && pn.value() === '' && !pn.hasPlacements()) {
                signatory.deleteField(pn);
            } else if(pn && pn.authorObligatory == 'sender') {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender(true);
            } else if(pn && pn.authorObligatory == 'signatory') {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender(false);
            } else if(pn && pn.authorObligatory == 'optional') {
                pn.makeOptional();
            }
        }
    },
    needsMobile: function() {
        return this.mobileDelivery() || this.emailMobileDelivery();
    },
    color: function() {
        return this.get('color');
    },
    setColor: function(c) {
        this.set({color:c});
        return this;
    },
    needsEmail: function() {
        return this.emailDelivery() || this.emailMobileDelivery();
    },
    ensureEmail: function() {
        var signatory = this;
        var email = signatory.emailField();
        if(signatory.needsEmail()) {
            email.makeObligatory();
            email.setShouldBeFilledBySender(true);
        } else if(email.authorObligatory == 'sender') {
            email.makeObligatory();
            email.setShouldBeFilledBySender(true);
        } else if(email.authorObligatory == 'signatory') {
            email.makeObligatory();
            email.setShouldBeFilledBySender(false);
        } else if(email.authorObligatory == 'optional') {
            email.makeOptional();
        }
    },
    needsSignature: function() {
        return this.padDelivery();
    },
    ensureSignature: function() {
        var signatory = this;
        var document = signatory.document();
        var signatures = signatory.signatures();
        // remove the unplaced signatures
        _.each(signatures, function(s) {
            if(!s.hasPlacements())
                signatory.deleteField(s);
        });
        signatures = signatory.signatures();

        if(signatory.needsSignature()) {
            if(signatures.length === 0)
                signatory.addField(new Field({name:document.newSignatureName(),
                                              type: 'signature',
                                              obligatory: true,
                                              shouldbefilledbysender: signatory.author(),
                                              signatory: signatory}));
        }
    },
    bindBubble: function() {
        var signatory = this;
        signatory.bind('change', signatory.bubbleSelf);
        signatory.bind('bubble', signatory.triggerBubble);
    },
    bubbleSelf: function() {
        var signatory = this;
        signatory.trigger('bubble');
    },
    triggerBubble: function() {
        var signatory = this;
        var document = signatory.document();
        if(document)
            document.trigger('bubble');
    }

});

})(window);
