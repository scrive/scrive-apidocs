/* Signatories model + basic view + signatories attachments
 */

define(['Backbone', 'moment', 'legacy_code'], function(Backbone, moment) {

window.SignatoryAttachment = Backbone.Model.extend({
    defaults: {
        name: "",
        description: "",
        loading: false,
        hasChanged: false
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
        this.set({ hasChanged: true }, { silent: true });
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
                 {name: "email",     type : "standard", shouldbefilledbysender : true, obligatory : true}, // We need to set it since default delivery is email
                 {name: "mobile",    type : "standard", shouldbefilledbysender : false, obligatory : false},
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
        delivery: "email",
        confirmationdelivery : "email",
        // Internal properties used by design view for "goldfish" memory
        changedDelivery : false,
        changedConfirmationDelivery : false,
        deliveryGoldfishMemory : null,
        confirmationDeliveryWasNone  : false,
        isLastViewer : false
    },

    initialize: function(args) {
        var signatory = this;
        signatory.desynchronizeDeliveryAndConfirmationDeliveryIfNeeded();
        _.bindAll(this, 'bubbleSelf', 'triggerBubble');
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
      return "/s/acceptaccount/" + this.document().id + "/" + this.signatoryid();
    },
    signIndex: function() {
        console.log(this.document().signatories().length);
        var allSignatories = this.document().signatories();
        var index = 1;
        for (var i = 0; i < allSignatories.length; i++) {
            if (allSignatories[i] === this)
                return index;
            if (allSignatories[i].signs()) index++;
        }
        return 0;
    },
    participantIndex : function() {
        return _.indexOf(this.document().signatories(),this) + 1;
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
        if (this.emailField() != undefined) {
          return this.emailField().value();
        } else {
          return '';
        }
    },
    mobile: function() {
        if (this.mobileField() != undefined) {
          return this.mobileField().value();
        } else {
          return '';
        }
    },
    fstname: function() {
        return this.fstnameField().value();
    },
    sndname: function() {
        return this.sndnameField() != undefined ? (this.sndnameField().value() != undefined ? this.sndnameField().value() : "") : "";
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
        var name = (this.fstname() + " " + this.sndname()).trim();
        if (name != undefined && name != " ")
            return name;
        else
            return "";
    },
    nameInDocument : function() {
       var signatory = this;
       if (signatory.isCsv())
        return localization.csv.title;
       if (signatory.signs() &&  signatory.author())
        return localization.process.authorsignatoryname + " " + signatory.signIndex();
       else if(signatory.author())
        return localization.process.authorname;
       else if (signatory.signs())
        return localization.process.signatoryname + " " + signatory.signIndex();
       else
        return localization.process.nonsignatoryname;
    },
    smartname: function() {
        if (this.current())
         return localization.you;
        else
         return this.nameOrEmailOrMobile();
    },
    nameForLists : function() {
        if (this.isCsv()) {
          return localization.csvFilePersons;
        } else if (this.smartname().trim() != "") {
          return this.smartname();
        } else {
          return this.nameInDocument();
        }
    },
    nameOrEmail: function() {
         if (this.name() != "")
         return this.name();
        else
         return this.email();
    },
    nameOrEmailOrMobile: function() {
         if (this.name() != '') {
           return this.name();
         } else if (this.email() != '') {
           return this.email();
         } else {
           // no name&email, signatory must be identified by just phone number
           return this.mobile();
         }
    },
    saved: function() {
      return this.get("saved");
    },
    signdate: function() {
        if (this.get("signdate"))
          return moment(this.get("signdate")).toDate();
        return undefined;
    },
    rejecteddate: function() {
        if (this.get("rejecteddate"))
          return moment(this.get("rejecteddate")).toDate();
        return undefined;
    },
    seendate: function() {
        if (this.get("seendate"))
          return moment(this.get("seendate")).toDate();
        return undefined;

    },
    readdate: function() {
        if (this.get("readdate"))
          return moment(this.get("readdate")).toDate();
        return undefined;
    },
    deliveredInvitation: function() {
        return this.get("deliveredInvitation");
    },
    undeliveredInvitation: function() {
          return this.get("undeliveredInvitation");
    },
    undeliveredMailInvitation: function() {
          return this.get("undeliveredMailInvitation");
    },
    undeliveredSMSInvitation: function() {
          return this.get("undeliveredSMSInvitation");
    },
    signorder: function() {
         return this.get("signorder");
    },
    setSignOrder: function(i) {
         this.set({signorder: parseInt(i + "")});
         this.document().checkLastViewerChange();
    },
    signs: function() {
         return this.get("signs");
    },
    signsuccessredirect : function() {
          return this.get("signsuccessredirect");
    },
    rejectredirect : function() {
          return this.get("rejectredirect");
    },
    makeSignatory: function() {
      this.set({ signs: true });
      this.document().checkLastViewerChange();
      this.trigger("change:role");
    },
    makeViewer: function() {
      this.set({signs: false});
      this.document().checkLastViewerChange();
      _.each(this.signatures(),function(s) {
         s.removeAllPlacements();
      });
      _.each(this.fields(), function(field) {
          if (field.isCheckbox()) field.remove();
      });

      this.trigger("change:role");
    },
    hasSigned: function() {
        return this.signdate() != undefined;
    },
    attachments: function() {
        return this.get("attachments");
    },
    removeAttachment : function(a) {
        this.set({"attachments": _.without(this.attachments(), a)}, {silent: true});
        this.document().trigger('change');
    },
    addAttachment: function(att) {
        this.get("attachments").push(att);
        this.document().trigger('change');
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
    isLastViewer : function() {
      return this.get("isLastViewer");
    },
    updateLastViewer : function() {
      var self = this;
      this.set({isLastViewer : !this.signs() &&
                               _.all(this.document().signatories(), function(s) {
                                        return !s.signs() || s.signorder() < self.signorder();
                               })});
    },
    checkLastViewerChange : function() {
      var previousLastViewerState = this.isLastViewer();
      this.updateLastViewer();
      var lastViewerState = this.isLastViewer();

      if (!previousLastViewerState && lastViewerState) {
        this.set({ deliveryGoldfishMemory : this.get("delivery") });
        this.set({ confirmationDeliveryWasNone : this.get("confirmationdelivery") == "none" });
        if (this.get("confirmationDeliveryWasNone")) {
          this.set({ confirmationdelivery : _.contains(["email", "mobile", "email_mobile"], this.get("delivery")) ? this.get("delivery") : "email"});
        }
        this.trigger("change:delivery");
      } else if (previousLastViewerState && !lastViewerState) {
        this.set({ delivery : this.get("deliveryGoldfishMemory") == null
                              ? "email"
                              : this.get("deliveryGoldfishMemory") });

        if (this.get("confirmationDeliveryWasNone")) {
          this.set({confirmationdelivery: "none"});
        }
        this.trigger("change:delivery");
      }
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
      return this.signatures().length > 0 ;
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
          return this.get("authentication") == "standard" && !this.isLastViewer();
    },
    elegAuthentication: function() {
          return this.get("authentication") == "eleg" && !this.isLastViewer();
    },
    smsPinAuthentication: function() {
          return this.get("authentication") == "sms_pin" && !this.isLastViewer();
    },
    emailDelivery: function() {
          return this.get("delivery") == "email" && !this.isLastViewer();
    },
    padDelivery : function() {
          return this.get("delivery") == "pad" && !this.isLastViewer();
    },
    mobileDelivery : function() {
          return this.get("delivery") == "mobile" && !this.isLastViewer();
    },
    emailMobileDelivery : function() {
          return this.get("delivery") == "email_mobile" && !this.isLastViewer();
    },
    apiDelivery : function() {
          return this.get("delivery") == "api" && !this.isLastViewer();
    },
    noneDelivery : function() {
          return this.get("delivery") == this.isLastViewer();
    },
    emailConfirmationDelivery: function() {
          return this.get("confirmationdelivery") == "email";
    },
    mobileConfirmationDelivery: function() {
          return this.get("confirmationdelivery") == "mobile";
    },
    emailMobileConfirmationDelivery: function() {
          return this.get("confirmationdelivery") == "email_mobile";
    },
    noneConfirmationDelivery: function() {
          return this.get("confirmationdelivery") == "none";
    },
    remind: function(customtext) {
        return new Submit({
              url: "/resend/" + this.document().documentid() + "/" + this.signatoryid(),
              method: "POST",
              customtext: customtext
          });
    },
    giveForPadSigning : function(callback) {
        return new Submit({
              url: "/padsign/"+ this.document().documentid() + "/" + this.signatoryid(),
              method: "POST"
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
    changeAuthentication: function(type, value) {
        return new Submit({
                url: "/api/frontend/changeauthentication/" + this.document().documentid() + "/" + this.signatoryid(),
                method: "POST",
                authentication_type: type,
                authentication_value: value
        });
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
    csvHeader: function() {
        if (this.isCsv())
          return this.csv()[0];
        return undefined;
    },
    hasCsvField: function(name) {
        if (this.csvHeader() != undefined)
          return _.contains(this.csvHeader(), name);
        return false;
    },
    hasTextFieldWithName : function(name) {
        return (this.field(name,"standard") != undefined || this.field(name,"custom") != undefined);
    },
    isCsv: function() {
        return this.csv() != undefined;
    },
    setCsv : function(csv) {
        this.set({"csv" : csv});
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
        return this.userid() != undefined;
    },
    userid : function() {
        return this.get("userid");
    },
    draftData : function() {
        return {
              id : this.signatoryid(),
              fields: _.map(this.readyFields(), function(field) {
                  return field.draftData();
              }),
              author: this.author(),
              signs: this.signs(),
              signorder: this.signorder(),
              attachments: _.map(this.attachments(), function(att) {
                  return att.draftData();
              }),
              csv: this.csv(),
              signsuccessredirect : this.signsuccessredirect(),
              rejectredirect : this.rejectredirect(),
              authentication: this.authentication(),
              delivery: this.delivery(),
              confirmationdelivery : this.confirmationdelivery()
        };
    },
    delivery: function() {
        return this.get('delivery');
    },
    confirmationdelivery : function() {
        return this.get('confirmationdelivery');
    },
    setDelivery: function(d) {
        this.set({delivery : d, changedDelivery : true});
        if (!this.get("changedConfirmationDelivery"))
          this.synchConfirmationDelivery();
        return this;
    },
    setConfirmationDelivery: function(d) {
        this.set({confirmationdelivery:d, changedConfirmationDelivery : true});
        if (!this.get("changedDelivery"))
          this.synchDelivery();
        return this;
    },
    desynchronizeDeliveryAndConfirmationDeliveryIfNeeded : function() {
      if (this.emailDelivery() && this.emailConfirmationDelivery())
        return;
      if (this.mobileDelivery() && this.mobileConfirmationDelivery())
        return;
      if (this.emailMobileDelivery() && this.emailMobileConfirmationDelivery())
        return;
      if (this.padDelivery() && this.emailDelivery())
        return;

      this.set({changedDelivery : true, changedConfirmationDelivery : true});
    },
    synchConfirmationDelivery : function() {
      if (this.emailDelivery()) {
        this.set({confirmationdelivery:"email"});
      }
      else if (this.mobileDelivery()) {
        this.set({confirmationdelivery:"mobile"});
      }
      else if (this.emailMobileDelivery()) {
        this.set({confirmationdelivery:"email_mobile"});
      }
      else if (this.padDelivery()) {
        this.set({confirmationdelivery:"email"});
      }
    },
    synchDelivery : function() {
      if (this.emailConfirmationDelivery()) {
        this.set({delivery:"email"});
      }
      else if (this.mobileConfirmationDelivery()) {
        this.set({delivery:"mobile"});
      }
      else if (this.emailMobileConfirmationDelivery()) {
        this.set({delivery:"email_mobile"});
      }
      else if (this.noneConfirmationDelivery()) {
        this.set({delivery:"email"});
      }
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
    authenticationFieldValue: function() {
        if(this.elegAuthentication()) {
            return this.personalnumber();
        }
        else if(this.smsPinAuthentication()) {
            return this.mobile();
        }
        return '';
    },
    hasProblems: function() {
        return _.some(this.fields(), function(field) {
            return !field.isValid() || field.hasNotReadyPlacements();
        });
    },
    role: function() {
        if(this.signs())
            return 'signatory';
        else
            return 'viewer';
    },
    ensureAllFields : function() {
      var signatory = this;
      if (!signatory.document().preparation()) return; // We only do this checks in design view. We will need to factor this out one day. MR

      // This is a reset of obligatorynes of all fields that could be included in extra details section
      // Explaination: If email, phone or personal number has no placements, it will be interpreted as optional,
      // unless it will be required to be obligatory by one of ensureXXX functions.

      if (signatory.fstnameField() != undefined && !signatory.fstnameField().hasPlacements())
      {
        signatory.fstnameField().authorObligatory = 'optional';
        signatory.fstnameField().set({"obligatory":true, "shouldbefilledbysender" : false});
      }
      if (signatory.sndnameField() != undefined && !signatory.sndnameField().hasPlacements())
      {
        signatory.sndnameField().authorObligatory = 'optional';
        signatory.sndnameField().set({"obligatory":true, "shouldbefilledbysender" : false});
      }

      if (signatory.personalnumberField() != undefined && !signatory.personalnumberField().hasPlacements())
      {
        signatory.personalnumberField().authorObligatory = 'optional';
        signatory.personalnumberField().set({"obligatory":false},{silent:true});
      }
      if (signatory.emailField() != undefined && !signatory.emailField().hasPlacements())
      {
        signatory.emailField().authorObligatory = 'optional';
        signatory.emailField().set({"obligatory":false},{silent:true});
      }
      if (signatory.mobileField() != undefined && !signatory.mobileField().hasPlacements())
      {
        signatory.mobileField().authorObligatory = 'optional';
        signatory.mobileField().set({"obligatory":false},{silent:true});
      }

      this.ensureEmail();
      this.ensureMobile();
      this.ensurePersNr();

    },
    ensurePersNr: function() {
        var signatory = this;
        var pn = signatory.personalnumberField();
        if(signatory.needsPersonalNumber()) {
            if(!pn) {
                var f = new Field({name:'sigpersnr',
                                   type: 'standard',
                                   obligatory: true,
                                   shouldbefilledbysender: false,
                                   signatory: signatory});
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
                pn.setShouldBeFilledBySender(false);
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
                                   shouldbefilledbysender: signatory.author(),
                                   signatory: signatory});
                f.addedByMe = true;
                signatory.addField(f);
            } else {
                pn.makeObligatory();
                pn.setShouldBeFilledBySender((!pn.canBeSetByRecipent()) || signatory.author() || (pn.authorObligatory == 'sender'));
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
                pn.setShouldBeFilledBySender(false);
            }
        }
    },
    needsMobile: function() {
        return this.mobileDelivery() || this.emailMobileDelivery() || this.mobileConfirmationDelivery() || this.emailMobileConfirmationDelivery() || this.smsPinAuthentication();
    },
    needsEmail: function() {
        return this.emailDelivery() || this.emailMobileDelivery() || this.emailConfirmationDelivery() || this.emailMobileConfirmationDelivery();
    },
    ensureEmail: function() {
        var signatory = this;
        var email = signatory.emailField();
        if(signatory.needsEmail()) {
          if (!email) {
            var f = new Field({name:'email',
                                 type: 'standard',
                                 obligatory: true,
                                 shouldbefilledbysender: true,
                                 signatory: signatory});
            f.addedByMe = true;
            signatory.addField(f);
          } else {
            email.makeObligatory();
            email.setShouldBeFilledBySender((!email.canBeSetByRecipent()) || (email.authorObligatory == 'sender'));
          }
        }
       else {
         if(email && email.addedByMe && email.value() === '' && !email.hasPlacements()) {
            signatory.deleteField(email);
         } else if(email && email.authorObligatory == 'sender') {
            email.makeObligatory();
            email.setShouldBeFilledBySender(true);
         } else if(email && email.authorObligatory == 'signatory') {
            email.makeObligatory();
            email.setShouldBeFilledBySender(false);
         } else if(email && email.authorObligatory == 'optional') {
            email.makeOptional();
            email.setShouldBeFilledBySender(false);
         }
      }
    },
    bindBubble: function() {
        var signatory = this;
        signatory.bind('change', signatory.bubbleSelf);
        signatory.bind('bubble', signatory.triggerBubble);
        signatory.listenTo(signatory,'change', function() {signatory.ensureAllFields();});

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
    },
    normalizeWithFirstCSVLine : function() {
      if (!this.isCsv()) return;

      var csv = this.csv();
      var name = "";
      for(var i=0;i<csv[0].length;i++)
      {
        var field = this.field(csv[0][i],i < 7 ? "standard" : "custom");
        if (field != undefined) field.setValue(csv[1][i]);
        if (i ==0 || i == 1)  name +=  " " + csv[1][i];
      }
      this.setCsv(undefined);
      return name;
    },
    dropFirstCSVLine : function() {
       this.csv()[1] = this.csv()[0];
       this.csv().shift();
    }
});

});
