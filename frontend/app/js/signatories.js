var Backbone = require("backbone");
var moment = require("moment");
var _ = require("underscore");
var Field = require("./fields.js").Field;
var SignatoryAttachment = require("./signatoryattachment.js").SignatoryAttachment;
var HighlightedPage = require("./highlightedpage.js").HighlightedPage;
var Submit = require("./submits.js").Submit;
var Mail = require("./confirmationsWithEmails.js").Mail;
var ConsentModule = require("./consentmodule").ConsentModule;
var Subscription = require("../scripts/account/subscription");

/* Signatories model */


var Signatory = exports.Signatory = Backbone.Model.extend({
    defaults: {
        id: 0,
        signatory_role: "signing_party",
        is_author: false,
        fields: [{type: "name",   order : 1},
                 {type: "name",   order : 2},
                 {type: "email",  should_be_filled_by_sender : true, is_obligatory : true}, // We need to set it since default delivery is email
                 {type: "mobile", should_be_filled_by_sender : false, is_obligatory : false},
                 {type: "company"}
        ],
        current: false,
        attachments: [],
        sign_order: 1,
        csv: undefined,
        user_id: undefined,
        authentication_method_to_sign: "standard",
        authentication_method_to_view: "standard",
        authentication_method_to_view_archived: "standard",
        delivery_method: "email",
        confirmation_delivery_method: "email",
        notification_delivery_method: "none",
        allows_highlighting: false,
        hide_personal_number: false,
        can_forward: false,
        consent_module: null,
        // Internal properties used by design view for "goldfish" memory
        deliverySynchedWithConfirmationDelivery : true,
        confirmationDeliverySynchedWithDelivery : true,
        deliveryGoldfishMemory : null,
        confirmationDeliveryWasNone  : false,
        highlightedPages : [],
        willGetHighlighedPageSoon: false,
    },

    initialize: function(args) {
        var signatory = this;
        signatory.initDeliveryAndConfirmationDeliverySynchFlags();
        var extendedWithSignatory = function(hash) {
                    hash.signatory = signatory;
                    return hash;
        };
        var fields = signatory.get('fields');
        fields = _.map(fields, function(field) {
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

        if(args.consent_module != null) {
          signatory.set({
            "consent_module": new ConsentModule(args.consent_module)
          });
        }

        var highlightedPages =  _.map(args.highlighted_pages, function(attachment) {
                return new HighlightedPage(extendedWithSignatory(attachment));
        });

        signatory.set({"highlighted_pages": highlightedPages });

        signatory.bindBubble();
    },
    document: function() {
        return this.get("document");
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
    participantIndex : function() {
        return _.indexOf(this.document().signatories(),this) + 1;
    },
    signatoryid: function() {
        return this.get("id");
    },
    author: function() {
        return this.get("is_author");
    },
    current: function() {
        return this.get("current");
    },
    status: function() {
      var signatory = this;
      var document = signatory.document();
      if (document.status() === "document_error") {
        return "problem";
      } else if (document.status() === "preparation") {
        return "draft";
      } else if (signatory.undeliveredMailConfirmation()) {
        return "confirmationdeliveryproblem";
      } else if (signatory.signs() && signatory.hasSigned()) {
        return "signed";
      } else if (signatory.approves() && signatory.hasSigned()) {
        return "approved";
      } else if (document.status() === "canceled") {
        return "cancelled";
      } else if (document.status() === "timedout") {
        return "timeouted";
      } else if (document.status() === "rejected") {
        return "rejected";
      } else if (signatory.seendate()) {
        return "opened";
      } else if (signatory.readdate()) {
        return "read";
      } else if (signatory.undeliveredMailInvitation() || signatory.undeliveredSMSInvitation()) {
        return "deliveryproblem";
      } else if (signatory.deliveredMailInvitation() || signatory.deliveredSMSInvitation()) {
        return "delivered";
      } else {
        return "sent";
      }

    },
    fields: function() {
        return this.get("fields");
    },
    emailField : function() {
      return _.find(this.fields(), function(f) {return f.isEmail();});
    },
    mobileField : function() {
      return _.find(this.fields(), function(f) {return f.isMobile();});
    },
    fstnameField : function() {
      return _.find(this.fields(), function(f) {return f.isFstName();});
    },
    sndnameField : function() {
      return _.find(this.fields(), function(f) {return f.isSndName();});
    },
    companyField : function() {
      return _.find(this.fields(), function(f) {return f.isCompanyName();});
    },
    companynumberField : function() {
      return _.find(this.fields(), function(f) {return f.isCompanyNumber();});
    },
    personalnumberField : function() {
      return _.find(this.fields(), function(f) {return f.isSSN();});
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
      return this.fstnameField() != undefined ? (this.fstnameField().value() != undefined ? this.fstnameField().value() : "") : "";
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
    hasField: function(type,order,name) {
      return _.any(this.fields(), function(f) {
         return f.type() == type && (!order || f.order() == order) && (!name || f.name() == name);
      });
    },
    field: function(name, type, order) {
        var fields = this.fields();
        for (var i = 0; i < fields.length; i++) {
            if ((name == undefined || fields[i].name() == name) &&
                (type == undefined || fields[i].type() == type) &&
                (order == undefined || fields[i].order() == order)) {
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
        return _.filter(this.fields(), function(f) {return f.isReady()});
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
       if (signatory.signs() && signatory.author())
        return localization.process.authorsignatoryname + " " + signatory.signIndex();
       else if(signatory.author())
        return localization.process.authorname;
       else if (signatory.signs())
        return localization.process.signatoryname + " " + signatory.signIndex();
       else
        return localization.process.nonsignatoryname;
    },
    smartname: function() {
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
    userid: function() {
      return this.get("user_id");
    },
    signdate: function() {
        if (this.get("sign_time"))
          return moment(this.get("sign_time")).toDate();
        return undefined;
    },
    rejecteddate: function() {
        if (this.get("rejected_time"))
          return moment(this.get("rejected_time")).toDate();
        return undefined;
    },
    seendate: function() {
        if (this.get("seen_time"))
          return moment(this.get("seen_time")).toDate();
        return undefined;

    },
    readdate: function() {
        if (this.get("read_invitation_time"))
          return moment(this.get("read_invitation_time")).toDate();
        return undefined;
    },
    deliveredInvitation: function() {
        return this.deliveredMailInvitation() || this.deliveredSMSInvitation();
    },
    deliveredMailInvitation: function() {
          return this.get("email_delivery_status") === "delivered";
    },
    deliveredSMSInvitation: function() {
          return this.get("mobile_delivery_status") === "delivered";
    },
    undeliveredInvitation: function() {
          return this.undeliveredMailInvitation() || this.undeliveredSMSInvitation();
    },
    undeliveredMailInvitation: function() {
          return this.get("email_delivery_status") === "not_delivered";
    },
    undeliveredSMSInvitation: function() {
          return this.get("mobile_delivery_status") === "not_delivered";
    },
    undeliveredMailConfirmation: function() {
      return this.get("confirmation_email_delivery_status") === "not_delivered";
    },
    apideliveryurl: function() {
          return this.get("api_delivery_url");
    },
    signorder: function() {
         return this.get("sign_order");
    },
    setSignOrder: function(i) {
         this.set({sign_order: parseInt(i + "")});
         this.document().checkLastViewerChange();
    },
    signatoryRole: function() {
         return this.get("signatory_role");
    },
    setSignatoryRole: function(v) {
         return this.set({"signatory_role":v});
    },
    signs: function() {
         return this.signatoryRole() == "signing_party";
    },
    views: function() {
         return this.signatoryRole() == "viewer";
    },
    approves: function() {
         return this.signatoryRole() == "approver";
    },
    isForwarded: function() {
         return this.signatoryRole() == "forwarded_party";
    },
    signsuccessredirect : function() {
          return this.get("sign_success_redirect_url");
    },
    rejectredirect : function() {
          return this.get("reject_redirect_url");
    },
    makeSignatory: function(args) {
      this.setSignatoryRole("signing_party");
      this.document().checkLastViewerChange();
      if (args != undefined
          && args.authenticationToView != undefined
          && args.authenticationToSign != undefined
          && args.deliveryMethod != undefined
         ) {
        // Note:
        // If auth options are incompatible, it will default to standard
        // See this.setAuthenticationTo[View,Sign] implementation
        this.setAuthenticationToView(args.authenticationToView);
        this.setAuthenticationToSign(args.authenticationToSign);
        this.setDelivery(args.deliveryMethod);
      }
    },
    makeViewer: function() {
      this.setSignatoryRole("viewer");
      this.document().checkLastViewerChange();
      _.each(this.signatures(),function(s) {
         s.removeAllPlacements();
      });
      _.each(this.fields(), function(field) {
          if (field.isCheckbox()) field.remove();
      });

      this.setAuthenticationToSign("standard");
      this.setAllowsHighlighting(false);
    },
    makeApprover: function() {
      this.setSignatoryRole("approver");
      this.document().checkLastViewerChange();
      this.setAuthenticationToSign("standard");
      this.setAllowsHighlighting(false);
      _.each(this.fields(),function(s) {
         s.removeAllPlacements();
      });

      this.setAuthenticationToSign("standard");
    },
    hasAuthenticatedToView: function() {
        return this.get("has_authenticated_to_view");
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
    highlightedPages: function() {
       return this.get("highlighted_pages");
    },
    // This is used to mark that addHighlightedPage will be called soon.
    // Frontend will have a chance to render stuff before it
    markThatWillGetHighlighedPageSoon: function() {
      this.set("willGetHighlighedPageSoon", true);
      this.triggerMainFileChange();
    },
    willGetHighlighedPageSoon : function() {
      return this.get("willGetHighlighedPageSoon");
    },
    addHighlightedPage: function(pageno,file_id) {
      this.get("highlighted_pages").push(new HighlightedPage({
        signatory: this,
        page:pageno,
        file_id:file_id
      }));
      this.set("willGetHighlighedPageSoon", false);
      this.triggerMainFileChange();
    },
    updateHighlightedPage : function(pageno,file_id) {
      _.each(this.get("highlighted_pages"), function(hp) {
        if (pageno == hp.page()) {
          console.log("Setting file " + file_id);
          hp.setFile(file_id);
        }
      });
      this.set("willGetHighlighedPageSoon", false);
      this.triggerMainFileChange();
    },
    removeHighlightedPage: function(pageno) {
      var newHighlightedPages = _.filter(this.highlightedPages(), function(hp) {
        return pageno != hp.page();
      });
      this.set("highlighted_pages", newHighlightedPages);
      this.triggerMainFileChange();
    },
    /* FIXME: Fileview in signview  doesn't listen on document change events - only on mainfile events
     * I don't want to make it listen to whole document right now - this is why I trigger change
     * of mainfile directly.
     */
    triggerMainFileChange: function() {
      if (this.document() && this.document().mainfile()) {
        this.document().mainfile().trigger("change");
      }
    },
    reachedBySignorder : function() {
        return this.signorder() <= this.document().signorder();
    },
    canSign: function() {
        var canSign = this.document().pending() &&
            this.signs() &&
            !this.hasSigned() &&
            this.signorder() == this.document().signorder();
        return canSign;
    },
    canApprove: function() {
        var canApprove = this.document().pending() &&
            this.approves() &&
            !this.hasSigned() &&
            this.signorder() == this.document().signorder();
        return canApprove;
    },
    canSignOrApprove: function() {
        return this.canSign() || this.canApprove();
    },
    ableToSign : function() {
        return   (this.signs()) &&
                !this.hasSigned() &&
                 this.signorder() == this.document().signorder();
    },
    ableToSignOrApprove : function() {
        return   (this.signs() || this.approves()) &&
                !this.hasSigned() &&
                 this.signorder() == this.document().signorder();
    },
    isLastViewer : function() {
      var self = this;
      return (this.views() &&
              _.all(this.document().signatories(), function(s) {
                return !(s.signs() || s.approves()) || s.signorder() < self.signorder();
              }));
    },
    checkLastViewerChange : function() {
      var previousLastViewerState = this.lastViewerState;
      this.lastViewerState = this.isLastViewer();

      if (!previousLastViewerState && this.lastViewerState) {
        this.set({ deliveryGoldfishMemory : this.get("delivery_method") });
        this.set({ confirmationDeliveryWasNone : this.get("confirmation_delivery_method") == "none" });
        if (this.get("confirmationDeliveryWasNone")) {
          this.set({ confirmationdelivery : _.contains(["email", "email_link", "mobile", "email_mobile", "email_link_mobile"], this.get("delivery_method")) ? this.get("delivery_method") : "email"});
        }
        this.set({delivery_method: "pad"});
      } else if (previousLastViewerState && !this.lastViewerState) {
        this.set({ delivery_method : this.get("deliveryGoldfishMemory") == null
                              ? "email"
                              : this.get("deliveryGoldfishMemory") });

        if (this.get("confirmationDeliveryWasNone")) {
          this.set({confirmationdelivery: "none"});
        }
      }
    },
    signatures: function() {
        return this.fieldsByType("signature");
    },
    hasPlacedSignatures: function() {
        return _.any(this.signatures(), function(signature) {
            return signature.hasPlacements();
        });
    },
    hasPlacedObligatorySignatures: function() {
        return _.any(this.signatures(), function(signature) {
            return signature.obligatory() && signature.hasPlacements();
        });
    },
    hasPlacedRadioGroups: function() {
        return (this.fieldsByType("radiogroup").length > 0);
    },
    standardAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "standard";
    },
    seBankIDAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "se_bankid";
    },
    noBankIDAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "no_bankid";
    },
    dkNemIDAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "dk_nemid";
    },
    fiTupasAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "fi_tupas";
    },
    smsPinAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "sms_pin";
    },
    verimiAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "verimi";
    },
    idinAuthenticationToView: function() {
          return this.get("authentication_method_to_view") == "nl_idin";
    },
    standardAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "standard";
    },
    seBankIDAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "se_bankid";
    },
    noBankIDAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "no_bankid";
    },
    dkNemIDAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "dk_nemid";
    },
    fiTupasAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "fi_tupas";
    },
    smsPinAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "sms_pin";
    },
    verimiAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "verimi";
    },
    idinAuthenticationToViewArchived: function() {
        return this.get("authentication_method_to_view_archived") == "nl_idin";
    },
    canHaveAuthenticationToSign: function() {
        // Not supported for Viewers or Approvers
        return this.signs();
    },
    standardAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "standard" && this.canHaveAuthenticationToSign();
    },
    seBankIDAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "se_bankid" && this.canHaveAuthenticationToSign();
    },
    noBankIDAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "no_bankid" && this.canHaveAuthenticationToSign();
    },
    dkNemIDAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "dk_nemid" && this.canHaveAuthenticationToSign();
    },
    noBankIDAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "no_bankid" && this.canHaveAuthenticationToSign();
    },
    dkNemIDAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "dk_nemid" && this.canHaveAuthenticationToSign();
    },
    smsPinAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "sms_pin" && this.canHaveAuthenticationToSign();
    },
    nlIDINAuthenticationToSign: function() {
          return this.get("authentication_method_to_sign") == "nl_idin" && this.canHaveAuthenticationToSign();
    },
    fiTupasAuthenticationToSign: function() {
        return this.get("authentication_method_to_sign") == "fi_tupas";
    },
    onfidoDocumentCheckAuthenticationToSign: function() {
      return this.get("authentication_method_to_sign") == "onfido_document_check";
    },
    onfidoDocumentAndPhotoCheckAuthenticationToSign: function() {
      return this.get("authentication_method_to_sign") == "onfido_document_and_photo_check";
    },
    emailDelivery: function() {
          return this.get("delivery_method") == "email";
    },
    padDelivery : function() {
          return this.get("delivery_method") == "pad";
    },
    mobileDelivery : function() {
          return this.get("delivery_method") == "mobile";
    },
    emailMobileDelivery : function() {
          return this.get("delivery_method") == "email_mobile";
    },
    apiDelivery : function() {
          return this.get("delivery_method") == "api";
    },
    portalDelivery : function() {
          return this.get("delivery_method") == "portal";
    },
    noneDelivery : function() {
          return this.isLastViewer();
    },

    emailConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "email";
    },

    emailLinkConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "email_link";
    },

    anyEmailConfirmationDelivery: function() {
      return this.emailConfirmationDelivery()
        || this.emailLinkConfirmationDelivery();
    },

    mobileConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "mobile";
    },

    emailMobileConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "email_mobile";
    },

    emailLinkMobileConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "email_link_mobile";
    },

    anyEmailMobileConfirmationDelivery: function() {
      return this.emailMobileConfirmationDelivery()
        || this.emailLinkMobileConfirmationDelivery();
    },

    noneConfirmationDelivery: function() {
      return this.get("confirmation_delivery_method") == "none";
    },

    hasConfirmationEmail: function() {
      return this.anyEmailConfirmationDelivery()
        || this.anyEmailMobileConfirmationDelivery();
    },

    hasConfirmationMobile: function() {
      return this.mobileConfirmationDelivery()
        || this.anyEmailMobileConfirmationDelivery();
    },

    hasConfirmationEmailLink: function() {
      return this.emailLinkConfirmationDelivery()
        || this.emailLinkMobileConfirmationDelivery();
    },

    hasConfirmationEmailAttachments: function() {
      return this.emailConfirmationDelivery()
        || this.emailMobileConfirmationDelivery();
    },

    synchDelivery: function () {
      if (this.anyEmailConfirmationDelivery()) {
        this.setDeliverySynchedWithConfirmationDelivery("email");
      } else if (this.mobileConfirmationDelivery()
                 && Subscription.currentSubscription().currentUserFeatures().canUseSMSInvitations()) {
        this.setDeliverySynchedWithConfirmationDelivery("mobile");
      } else if (this.anyEmailMobileConfirmationDelivery()
                 && Subscription.currentSubscription().currentUserFeatures().canUseSMSInvitations()) {
        this.setDeliverySynchedWithConfirmationDelivery("email_mobile");
      } else if (this.noneConfirmationDelivery()) {
        this.setDeliverySynchedWithConfirmationDelivery("email");
      }
    },

    hasNotificationEmailAndMobile: function() {
      return this.notificationDelivery() == "email_mobile";
    },

    hasNotificationEmail: function() {
      var deliveryMethod = this.notificationDelivery();
      return deliveryMethod == "email" || deliveryMethod == "email_mobile";
    },

    hasNotificationMobile: function() {
      var deliveryMethod = this.notificationDelivery();
      return deliveryMethod == "mobile" || deliveryMethod == "email_mobile";
    },

    hasNotification: function() {
      return this.hasNotificationEmail() || this.hasNotificationMobile();
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
              url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.document().currentSignatory().signatoryid() + "/reject",
              method: "POST",
              ajax : true,
              reason: customtext.trim() || undefined
          });
    },
    forward: function(customtext,textFieldsWithValues) {
        var textFieldsWithValues = textFieldsWithValues.map(function(fwv) {
          return {
            type : fwv.field.type()
          , order : fwv.field.order() || undefined
          , name : fwv.field.name() || undefined
          , value : fwv.newValue
          }
        });

        return new Submit({
              url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.document().currentSignatory().signatoryid() + "/forwardsigning",
              method: "POST",
              ajax : true,
              fields : JSON.stringify(textFieldsWithValues),
              message: customtext.trim() || undefined
          });
    },
    changeAuthenticationToView: function(authenticationType, personalNumber, mobileNumber) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/setauthenticationtoview",
                method: "POST",
                authentication_type: authenticationType,
                personal_number: personalNumber,
                mobile_number: mobileNumber
        });
    },
    changeAuthenticationToViewArchived: function(authenticationType, personalNumber, mobileNumber) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/setauthenticationtoviewarchived",
                method: "POST",
                authentication_type: authenticationType,
                personal_number: personalNumber,
                mobile_number: mobileNumber
        });
    },
    changeAuthenticationToSign: function(authenticationType, personalNumber, mobileNumber) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/setauthenticationtosign",
                method: "POST",
                authentication_type: authenticationType,
                personal_number: personalNumber,
                mobile_number: mobileNumber
        });
    },
    changeEmailAndPhone: function(newValue) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/changeemailandmobile",
                method: "POST",
                email: newValue.email,
                mobile_number: newValue.mobile
         });
    },
    changeEmail: function(email) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/changeemailandmobile",
                method: "POST",
                email: email
         });
    },
    changePhone: function(phone) {
        return new Submit({
                url: "/api/frontend/documents/" + this.document().documentid() + "/" + this.signatoryid() + "/changeemailandmobile",
                method: "POST",
                mobile_number: phone
         });
    },
    remindMail: function() {
        return new Mail({
                document: this.document(),
                signatory: this,
                type: "remind",
                editWidth: 300
        });
    },
    newField : function(t,f) {
        return new Field({signatory: this, type : t || ""});
    },
    addField : function(f) {
        this.fields().push(f);
        this.trigger("change");
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
        return (this.field(name,"standard") != undefined || this.field(name,"text") != undefined);
    },
    isCsv: function() {
        return this.csv() != undefined;
    },
    setCsv : function(csv) {
        this.set({"csv" : csv});
    },
    removed : function() {
        this.isRemoved = true;
        this.off();
        this.trigger("removed");
    },
    hasUser: function() {
        return this.get("userid") != undefined;
    },
    draftData : function() {
        return {
              id : this.signatoryid(),
              fields: _.map(this.readyFields(), function(field) {
                  return field.draftData();
              }),
              is_author: this.author(),
              signatory_role: this.signatoryRole(),
              sign_order: this.signorder(),
              attachments: _.map(this.attachments(), function(att) {
                  return att.draftData();
              }),
              csv: this.csv(),
              sign_success_redirect_url : this.signsuccessredirect(),
              reject_redirect_url : this.rejectredirect(),
              authentication_method_to_sign: this.authenticationToSign(),
              authentication_method_to_view: this.authenticationToView(),
              authentication_method_to_view_archived: this.authenticationToViewArchived(),
              delivery_method: this.delivery(),
              confirmation_delivery_method : this.confirmationdelivery(),
              notification_delivery_method : this.notificationDelivery(),
              allows_highlighting : this.allowsHighlighting(),
              hide_personal_number: this.hidePN(),
              can_forward: this.canForward(),
              consent_module: this.consentModule() ? this.consentModule().draftData() : null
        };
    },
    delivery: function() {
        return this.get('delivery_method');
    },
    confirmationdelivery : function() {
        return this.get('confirmation_delivery_method');
    },
    setDelivery: function(d, silent) {
        this.set({delivery_method : d, deliverySynchedWithConfirmationDelivery : false});
    },
    setDeliverySynchedWithConfirmationDelivery: function(d) {
        this.set({delivery_method:d});
    },
    isDeliverySynchedWithConfirmationDelivery : function() {
      return this.get("deliverySynchedWithConfirmationDelivery");
    },
    setConfirmationDelivery: function(d) {
        this.set({confirmation_delivery_method:d, confirmationDeliverySynchedWithDelivery : false});
    },
    setConfirmationDeliverySynchedWithDelivery: function(dm) {
      var cdm_deadbeef = dm;
      if(dm == "email" && this.hasConfirmationEmailLink()) {
        cdm_deadbeef = "email_link";
      } else if(dm == "email_mobile" && this.hasConfirmationEmailLink()) {
        cdm_deadbeef = "email_link_mobile";
      }
      this.set({confirmation_delivery_method: cdm_deadbeef});
    },
    isConfirmationDeliverySynchedWithDelivery : function() {
      return this.get("confirmationDeliverySynchedWithDelivery");
    },
    initDeliveryAndConfirmationDeliverySynchFlags : function() {
      if (
        (this.emailDelivery() && this.emailConfirmationDelivery()) ||
        (this.emailDelivery() && this.emailLinkConfirmationDelivery()) ||
        (this.mobileDelivery() && this.mobileConfirmationDelivery()) ||
        (this.emailMobileDelivery() && this.emailMobileConfirmationDelivery()) ||
        (this.emailMobileDelivery() && this.emailLinkMobileConfirmationDelivery()) ||
        (this.padDelivery() && this.emailDelivery())
      ) {
        this.set({deliverySynchedWithConfirmationDelivery : true, confirmationDeliverySynchedWithDelivery : true});
      } else {
        this.set({deliverySynchedWithConfirmationDelivery : false, confirmationDeliverySynchedWithDelivery : false});
      }
    },
    setNotificationDelivery: function(method) {
      console.log("setting notification_delivery_method: " + method);
      this.set({'notification_delivery_method': method});
    },
    notificationDelivery: function() {
      return this.get('notification_delivery_method');
    },
    authenticationToView: function() {
      return this.get('authentication_method_to_view');
    },
    authenticationToViewArchived: function() {
      return this.get('authentication_method_to_view_archived');
    },
  authenticationMethodsCanMix: function(authToView, authToSign, authToViewArchived) {
    var intersect = function(as, bs) {
      return as.filter(function (item) {
        return bs.indexOf(item) > -1;
      });
    }
    // maximum of 1 national EID is allowed per signatory
    var eids = ["se_bankid", "no_bankid", "dk_nemid", "fi_tupas", "nl_idin", "verimi"];
    return (1 >= intersect( eids, [authToSign, authToView, authToViewArchived]).length);
    },
    setAuthenticationToView: function(a) {
        var canMix = this.authenticationMethodsCanMix(a, this.authenticationToSign(), this.authenticationToViewArchived());
        this.set({
          "authentication_method_to_view" : a,
          "authentication_method_to_sign" : canMix
            ? this.authenticationToSign()
            : "standard",
          "authentication_method_to_view_archived" : canMix
            ? this.authenticationToViewArchived()
            : "standard"
        });
    },
    setAuthenticationToViewArchived: function(a) {
        var canMix = this.authenticationMethodsCanMix(this.authenticationToView(), this.authenticationToSign(), a);
        this.set({
          "authentication_method_to_view" : canMix
            ? this.authenticationToView()
            : "standard",
          "authentication_method_to_sign" : canMix
            ? this.authenticationToSign()
            : "standard",
          "authentication_method_to_view_archived" : a,
        });
    },
    authenticationToSign: function() {
        return this.get('authentication_method_to_sign');
    },
    setAuthenticationToSign: function(a) {
        var canMix = this.authenticationMethodsCanMix(this.authenticationToView(), a, this.authenticationToViewArchived());
        this.set({
          "authentication_method_to_view" : canMix
            ? this.authenticationToView()
            : "standard",
          "authentication_method_to_sign" : a,
          "authentication_method_to_view_archived" : canMix
            ? this.authenticationToViewArchived()
            : "standard"
        });
    },
    authenticationToSignFieldValue: function() {
        if(this.seBankIDAuthenticationToSign()) {
            return this.personalnumber();
        }
        else if(this.noBankIDAuthenticationToSign()) {
            return ''; // personal number is not checked, when signing with NOBankID
        }
        else if(this.dkNemIDAuthenticationToSign()) {
            return this.personalnumber();
        }
        else if(this.smsPinAuthenticationToSign()) {
            return this.mobile();
        }
        return '';
    },
    hasProblems: function() {
        return _.some(this.fields(), function(field) {
            return !field.isValid();
        });
    },
    ensureAllFields : function() {
      var signatory = this;
      if (!signatory.document().preparation()) return; // We only do this checks in design view. We will need to factor this out one day. MR

      // This is a reset of obligatorynes of all fields that could be included in extra details section
      // Explaination: If email, phone or personal number has no placements, it will be interpreted as optional,
      // unless it will be required to be obligatory by one of ensureXXX functions.

      if (signatory.fstnameField() != undefined && !signatory.fstnameField().hasPlacements())
      {
        var shouldBeFilledBySender = this.onfidoDocumentCheckAuthenticationToSign()
              || this.onfidoDocumentAndPhotoCheckAuthenticationToSign();
        signatory.fstnameField().authorObligatory = shouldBeFilledBySender ? 'sender' : 'signatory';
        signatory.fstnameField().setObligatoryAndShouldBeFilledBySender(true,shouldBeFilledBySender);
      }

      if (signatory.sndnameField() != undefined && !signatory.sndnameField().hasPlacements())
      {
        var shouldBeFilledBySender = this.onfidoDocumentCheckAuthenticationToSign()
              || this.onfidoDocumentAndPhotoCheckAuthenticationToSign();
        signatory.sndnameField().authorObligatory = shouldBeFilledBySender ? 'sender' : 'signatory';
        signatory.sndnameField().setObligatoryAndShouldBeFilledBySender(true,shouldBeFilledBySender);
      }

      if (signatory.personalnumberField() != undefined && !signatory.personalnumberField().hasPlacements())
      {
        signatory.personalnumberField().authorObligatory = 'optional';
      }
      if (signatory.emailField() != undefined && !signatory.emailField().hasPlacements())
      {
        signatory.emailField().authorObligatory = 'optional';
      }
      if (signatory.mobileField() != undefined && !signatory.mobileField().hasPlacements())
      {
        signatory.mobileField().authorObligatory = 'optional';
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
                var f = new Field({type: 'personal_number',
                                   obligatory: true,
                                   shouldbefilledbysender: signatory.needsPersonalNumberFilledByAuthor(),
                                   signatory: signatory});
                f.addedByMe = true;
                signatory.addField(f);
            } else {
               pn.setObligatoryAndShouldBeFilledBySender(true, signatory.needsPersonalNumberFilledByAuthor() || (pn.hasPlacements() && pn.shouldbefilledbysender()));
            }
        } else {
            if(pn && pn.addedByMe && pn.value() === '' && !pn.hasPlacements()) {
                signatory.deleteField(pn);
            } else if(pn && pn.authorObligatory == 'sender') {
                pn.setObligatoryAndShouldBeFilledBySender(true,true);
            } else if(pn && pn.authorObligatory == 'signatory') {
                pn.setObligatoryAndShouldBeFilledBySender(true,false);
            } else if(pn && pn.authorObligatory == 'optional') {
                pn.setObligatoryAndShouldBeFilledBySender(false,false);

            }
        }
    },
    needsPersonalNumber: function() {
      return this.seBankIDAuthenticationToSign()
        || this.seBankIDAuthenticationToView()
        || this.seBankIDAuthenticationToViewArchived()
        || this.noBankIDAuthenticationToView()
        || this.noBankIDAuthenticationToViewArchived()
        || this.dkNemIDAuthenticationToSign()
        || this.dkNemIDAuthenticationToView()
        || this.dkNemIDAuthenticationToViewArchived()
        || this.fiTupasAuthenticationToSign()
        || this.fiTupasAuthenticationToView()
        || this.fiTupasAuthenticationToViewArchived();
    },
    needsPersonalNumberFilledByAuthor: function() {
        return (this.seBankIDAuthenticationToView()
             || this.noBankIDAuthenticationToView()
             || this.dkNemIDAuthenticationToView()
             || (this.views() && this.seBankIDAuthenticationToViewArchived())
             || (this.views() && this.noBankIDAuthenticationToViewArchived())
             || (this.views() && this.dkNemIDAuthenticationToViewArchived())
             || (this.views() && this.fiTupasAuthenticationToViewArchived()));
    },
    ensureMobile: function() {
        var signatory = this;
        var pn = signatory.mobileField();
        if(signatory.needsMobile()) {
            if(!pn) {
                var f = new Field({type: 'mobile',
                                   is_obligatory: this.mobileIsObligatory(),
                                   should_be_filled_by_sender: signatory.author(),
                                   signatory: signatory});
                f.addedByMe = true;
                signatory.addField(f);
            } else {
                pn.setObligatoryAndShouldBeFilledBySender(this.mobileIsObligatory(), (!pn.canBeSetByRecipent()) || signatory.author() || (pn.authorObligatory == 'sender'));
            }
        } else {
            if(pn && pn.addedByMe && pn.value() === '' && !pn.hasPlacements()) {
                signatory.deleteField(pn);
            } else if(pn && pn.authorObligatory == 'sender') {
                pn.setObligatoryAndShouldBeFilledBySender(true,true);
            } else if(pn && pn.authorObligatory == 'signatory') {
                pn.setObligatoryAndShouldBeFilledBySender(true,false);
            } else if(pn && pn.authorObligatory == 'optional') {
                pn.setObligatoryAndShouldBeFilledBySender(false,false);
            }
        }
    },
    needsMobile: function() {
      return this.mobileDelivery()
        || this.emailMobileDelivery()
        || this.hasConfirmationMobile()
        || this.hasNotificationMobile()
        || this.noBankIDAuthenticationToView()
        || this.noBankIDAuthenticationToViewArchived()
        || this.smsPinAuthenticationToView()
        || this.smsPinAuthenticationToViewArchived()
        || this.smsPinAuthenticationToSign();
    },
    mobileIsObligatory: function() {
      // Mobile number is needed for NO BankID, but is not obligatory
      return this.mobileDelivery()
        || this.emailMobileDelivery()
        || this.hasConfirmationMobile()
        || this.hasNotificationMobile()
        || this.smsPinAuthenticationToView()
        || this.smsPinAuthenticationToViewArchived()
        || this.smsPinAuthenticationToSign();
    },
    needsEmail: function() {
        return this.emailDelivery()
        || this.emailMobileDelivery()
        || this.hasNotificationEmail()
        || this.hasConfirmationEmail()
        || this.verimiAuthenticationToView()
        || this.verimiAuthenticationToViewArchived();
    },
    ensureEmail: function() {
        var signatory = this;
        var email = signatory.emailField();
        if(signatory.needsEmail()) {
          if (!email) {
            var f = new Field({type: 'email',
                                 is_obligatory: true,
                                 should_be_filled_by_sender: true,
                                 signatory: signatory});
            f.addedByMe = true;
            signatory.addField(f);
          } else {
            email.setObligatoryAndShouldBeFilledBySender(true,(!email.canBeSetByRecipent()) || (email.authorObligatory == 'sender'));
          }
        }
       else {
         if(email && email.addedByMe && email.value() === '' && !email.hasPlacements()) {
            signatory.deleteField(email);
         } else if(email && email.authorObligatory == 'sender') {
            email.setObligatoryAndShouldBeFilledBySender(true,true);
         } else if(email && email.authorObligatory == 'signatory') {
            email.setObligatoryAndShouldBeFilledBySender(true,false);
         } else if(email && email.authorObligatory == 'optional') {
            email.setObligatoryAndShouldBeFilledBySender(false,false);
         }
      }
    },
    allowsHighlighting: function() {
      return this.get("allows_highlighting");
    },
    setAllowsHighlighting: function(v) {
      this.set({allows_highlighting: v});
    },
    hidePN: function() {
      return this.get("hide_personal_number");
    },
    canForward: function() {
      return this.get("can_forward");
    },
    setCanForward: function(v) {
      return this.set({"can_forward":v});
    },
    bindBubble: function() {
        var signatory = this;
        signatory.listenTo(signatory,'change', function() {
          signatory.ensureAllFields();
          if (signatory.document()) {
            signatory.document().trigger("change");
          }
        });
    },
    normalizeWithFirstCSVLine : function() {
      if (!this.isCsv()) return;

      var csv = this.csv();
      var name = "";
      for(var i=0;i<csv[0].length;i++)
      {
        var field = _.find(this.fields(), function(f) { return f.csvname() == csv[0][i];});
        if (field != undefined) field.setValue(csv[1][i]);
        if (i ==0 || i == 1)  name +=  " " + csv[1][i];
      }
      this.setCsv(undefined);
      return name;
    },
    dropFirstCSVLine : function() {
       this.csv()[1] = this.csv()[0];
       this.csv().shift();
    },
    consentModule: function() {
        return this.get("consent_module");
    }
});
