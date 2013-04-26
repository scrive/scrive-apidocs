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
                 {name: "sigpersnr", type : "standard"},
                 {name: "sigcompnr", type : "standard"},
                 {name: "signature", type : "signature"}
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
        this.bind("change:role", function() {signatory.document().trigger("change:signatories-roles")});
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
    remindSMS: function() {
        return new SMS({
                document: this.document(),
                signatory: this,
                type: "remind",
                editWidth: 300
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
    newCheckbox: function() {
       var checkbox = this.newField("checkbox", false);
       if(this.author())
           checkbox.makeOptional();
       else
           checkbox.makeObligatory();

       /* Generate unique name for a checkbox. Checkbox names are really important for API,
          for humans checkbox names are pure annoyance.
        */
       var allfields = _.flatten(_.map(this.document().signatories(), function(s) {return s.fields();}));
       var i = 1;
       while(_.any(allfields, function(f) {
           return f.name() == "checkbox-" + i;
       })) {
           i++;
       }
       checkbox.setName("checkbox-" + i);
       return checkbox;
    },
    newField : function(t,f) {
        return new Field({signatory: this, fresh: (f != undefined ? f : true) , type : t});
    },
    newSignature: function() {
       var signature = this.newField("signature", false);
       signature.makeObligatory();

       /* Generate unique name for a signature. Signature names are really important for API,
          for humans signature names are pure annoyance.
        */
       var allfields = _.flatten(_.map(this.document().signatories(), function(s) {return s.fields();}));
       var i = 1;
       while(_.any(allfields, function(f) {
           return f.name() == "signature-" + i;
       })) {
           i++;
       }
       signature.setName("signature-" + i);
       return signature;
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
    signsuccessredirect : function() {
        return this.get("signsuccessredirect");
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
              signorder: this.signs() ? this.signorder() : 1,
              attachments: _.map(this.attachments(), function(att) {
                  return att.draftData();
              }),
              csv: this.csv(),
              signsuccessredirect : this.signsuccessredirect()

        };
    }

});

})(window);
