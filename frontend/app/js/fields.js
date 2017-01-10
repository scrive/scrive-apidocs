var Backbone = require("backbone");
var _ = require("underscore");
var FieldPlacement = require("./placements.js").FieldPlacement;
var EmailValidation = require("./validation.js").EmailValidation;
var NotEmptyValidation = require("./validation.js").NotEmptyValidation;
var SSNForNOBankIDValidation = require("./validation.js").SSNForNOBankIDValidation;
var SSNForSEBankIDValidation = require("./validation.js").SSNForSEBankIDValidation;
var PhoneValidation = require("./validation.js").PhoneValidation;
var Validation = require("./validation.js").Validation;
var NoValidation = require("./validation.js").NoValidation;
var EmptyValidation = require("./validation.js").EmptyValidation;
var PhoneValidationNO = require("./validation.js").PhoneValidationNO;

/* Fields of signatories */


var Field = exports.Field = Backbone.Model.extend({
    defaults: {
        name : "",
        value : "",
        is_checked : false,
        signature : undefined,
        hadValueWhenCreated : false,
        placements : [],
        is_obligatory : true,
        should_be_filled_by_sender : false,
        hasChanged: false
    },
    initialize : function(args){
        var field = this;
        _.bindAll(this, 'remove');
        var extendedWithField =   function(hash){
            hash.field = field;
            return hash;
        };
        var placements =  _.map(args.placements, function(placement){
                return new FieldPlacement(extendedWithField(placement));
        });
        this.set({"placements": placements, "hadValueWhenCreated": this.hadValueWhenCreated(args)});
        if(args.signatory)
            args.signatory.bind("removed", field.remove);
        field.bindBubble();
    },
    hadValueWhenCreated: function(args) {
      if (args.type === "checkbox" && args.is_checked) {
        return true;
      } else if (args.type === "signature" && args.signature) {
        return true;
      } else if (args.type !== "checkbox" && args.type !== "signature" && args.value != "") { // ~ this.isText
        return true;
      } else {
        return false;
      }
    },
    type : function() {
        return this.get("type");
    },
    setType : function(t) {
        this.set({type:t});
    },
    order: function() {
       return this.get("order");
    },
    name : function() {
        return this.get("name");
    },
    value : function() {
        return this.get("value");
    },
    isChecked : function() {
      return this.get("is_checked");
    },
    signatureFile : function() {
      return this.get("signature");
    },
    obligatory : function() {
        return this.get("is_obligatory");
    },
    shouldbefilledbysender : function() {
        return this.get("should_be_filled_by_sender");
    },
    setShouldBeFilledBySender : function(s) {
        this.set({should_be_filled_by_sender:s});
        return this;
    },
    setObligatoryAndShouldBeFilledBySender : function(obl,sbs) {
      this.set({"is_obligatory" : obl, "should_be_filled_by_sender": sbs});
    },
    setValue : function(value, options) {
        this.set({ hasChanged: true }, { silent: true });
        this.set({"value" : value}, options);
    },
    setChecked : function(value, options) {
        this.set({ hasChanged: true }, { silent: true });
        this.set({"is_checked" : value}, options);
    },
    hasChanged: function() {
      return this.get("hasChanged");
    },
    setName : function(name) {
        return this.set({"name" : name});
    },
    isInPendingDocument: function() {
      return this.signatory() && this.signatory().document() && this.signatory().document().pending();
    },
    isClosed : function() {
        return this.isInPendingDocument() && this.get("hadValueWhenCreated");
    },
    hasDataForSigning: function() {
        if (this.isInPendingDocument() && this.isOptionalUncheckedCheckbox()) {
          // if optional, pre-checked checkbox was unchecked by signatory in signview
          return true;
        } else {
          return !this.isClosed() && (!this.isSignature() || this.value());
        }
    },
    placements : function() {
        return this.get("placements");
    },
    isPlaced: function() {
      return this.placements().length > 0;
    },
    signatory : function(){
        return this.get("signatory");
    },
    setSignatory: function(sig) {
        var oldsig = this.signatory();
        if(oldsig)
            oldsig.unbind('removed', this.remove);
        this.set({signatory:sig});
        sig.bind('removed', this.remove);
        return this;
    },
    moveToSignatory: function (sig) {
      var oldSig = this.signatory();
      sig.addField(this);
      this.setSignatory(sig);
      oldSig.deleteField(this);
    },
    canBeIgnored: function(){
        return this.value() == "" && this.placements().length == 0 && (this.isStandard() || this.isSignature());
    },
    readyForSign : function(){
        if (this.isEmail() && this.isOptional() && this.value() != "") {
          return new EmailValidation().validateData(this.value());
        } else if (this.isOptional()) {
            return true;
        } else if (this.isFstName() || this.isSndName()) {
            return new NotEmptyValidation().validateData(this.value());
        } else if (this.isEmail()) {
            return new EmailValidation().validateData(this.value());
        }
        if (this.isSSN() && (this.signatory().noBankIDAuthenticationToView())) {
            return new SSNForNOBankIDValidation().validateData(this.value());
        }
        if (this.isSSN() && (this.signatory().seBankIDAuthenticationToSign() || this.signatory().seBankIDAuthenticationToView())) {
            return new SSNForSEBankIDValidation().validateData(this.value());
        }
        if (this.isMobile() &&
              (   this.signatory().mobileConfirmationDelivery()
               || this.signatory().emailMobileConfirmationDelivery()
               || this.signatory().smsPinAuthenticationToSign()
              )
           ) {
            return new PhoneValidation().validateData(this.value());
        } else if (this.isText() && (this.value() != "")) {
            return true;
        } else if (this.canBeIgnored()) {
            return true;
        } else if (this.isSignature()) {
            return (this.value() != "" || this.placements().length == 0);
        } else if (this.isText() && this.isObligatory() && this.value() != "") {
            return true;
        } else if (this.isCheckbox() && this.isObligatory() && this.isChecked()) {
            return true;
        }
        return false;
    },
    nicename : function() {
      if (this.isFstName()) {
        return localization.fstname;
      } else if  (this.isSndName()) {
        return localization.sndname;
      } else if (this.isEmail()) {
        return localization.email;
      } else if  (this.isCompanyName()) {
        return localization.company;
      } else if  (this.isSSN()) {
        return localization.personalNumber;
      } else if (this.isCompanyNumber()) {
        return localization.companyNumber;
      } else if (this.isMobile()) {
        return localization.phone;
      } else {
        return this.name();
      }
    },
    nicetext : function() {
        var res = this.value() || this.nicename();

        // If text is empty add magic space to force some height
        if (res.trim() == "")
          res = '\xa0';

        return res;
    },
    // Validate the state of the field (not blank, etc) and the input
    validation: function() {
      var field = this;
      var signatory = field.signatory();

      if(field.isBlank()) {
        return new Validation({validates: function() {
          return field.type() && field.name();
        }});
      }
      if(field.noName()) {
        return new Validation({validates: function() {
          return !field.noName();
        }});
      }

      var concatValidations = new Validation();
      var senderMustFill = field.isObligatory() && field.shouldbefilledbysender();
      var viewerNeedsFieldForDelivery = !signatory.signs()
                                     && (field.isEmail() && (signatory.emailConfirmationDelivery() || signatory.emailMobileConfirmationDelivery()) ||
                                         field.isMobile() && (signatory.mobileConfirmationDelivery() || signatory.emailMobileConfirmationDelivery()));
      var willSignNowAndFieldNeeded = signatory.author()
        && signatory.ableToSign()
        && (!signatory.seBankIDAuthenticationToSign())
        && (!signatory.seBankIDAuthenticationToView())
        && (!signatory.hasPlacedSignatures())
        && field.isObligatory()
        && (field.isText() || field.isCheckbox());
      if(senderMustFill || willSignNowAndFieldNeeded || viewerNeedsFieldForDelivery) {
        concatValidations = this.validateFilled();
      }

      if(signatory.author() && (field.isFstName() || field.isSndName())) {
        return new NoValidation();
      }
      if(field.isEmail()) {
        concatValidations.concat(field.validateEmail());
      }
      if(field.isMobile()) {
        concatValidations.concat(field.validateMobile());
      }
      if(field.isSSN()) {
        concatValidations.concat(field.validateSSN());
      }
      if(field.isCheckbox()) {
        concatValidations.concat(field.validateCheckbox());
      }

      return concatValidations;
    },
    validateFilled: function() {
      var field = this;
      var validation = new Validation({
        validates: function(v) {
          if (field.isText()) {
            return v != undefined && v != "";
          } else if (field.isSignature()) {
            return field.signatureFile() != undefined || (field.value() != undefined && field.value() != "");
          } else if (field.isCheckbox()) {
            return field.isChecked();
          }
        },
        message: localization.designview.validation.notReadyField
      });
      return validation;
    },
    validateSSN: function() {
      if (this.signatory().seBankIDAuthenticationToSign() || this.signatory().seBankIDAuthenticationToView()) {
        return new EmptyValidation().or(new SSNForSEBankIDValidation());
      } else if (this.signatory().noBankIDAuthenticationToView()) {
        return new EmptyValidation().or(new SSNForNOBankIDValidation());
      } else {
        return new Validation();
      }
    },
    validateCheckbox: function() {
      var field = this;
      var validation = new Validation({ validates: function() {
          return field.name() != undefined && field.name() != "";
        }, message: localization.designview.validation.notReadyField});
      return validation;
    },
    validateMobile: function() {
      var signatory = this.signatory();
      var validation = new Validation();
      // If we have NOBankID to view we need Norwegian phone validation
      if (signatory.noBankIDAuthenticationToView()) {
        validation = new PhoneValidationNO();
      }
      // Else If we have anything that needs a valid phone, then of course we need phone validation!
      else if (signatory.mobileDelivery()
              || signatory.emailMobileDelivery()
              || signatory.smsPinAuthenticationToSign()
              || signatory.mobileConfirmationDelivery()
              || signatory.emailMobileConfirmationDelivery()
      ) {
        validation = new PhoneValidation();
      }
      // If we have mobile delivery, we need a non-empty mobile
      if (signatory.mobileDelivery() || signatory.emailMobileDelivery()) {
        validation = validation.concat(new NotEmptyValidation());
      }
      // Else it can be empty
      else {
        validation = validation.or(new EmptyValidation());
      }
      return validation;
    },
    validateEmail: function() {
      var field = this;
      var signatory = this.signatory();

      if(signatory.emailDelivery() || signatory.emailMobileDelivery()) {
        return new EmailValidation().concat(new NotEmptyValidation());
      }
      if (field.value() != undefined && field.value() != "") {
        return new EmailValidation();
      }
      return new Validation();
    },
    isEmail: function() {
        return this.type() == "email";
    },
    isMobile: function() {
        return this.type() == "mobile";
    },
    isName : function() {
        return  this.type() == "name";
    },
    isFstName: function() {
        return  this.isName() && this.order() == 1;
    },
    isSndName: function() {
        return this.isName() && this.order() == 2
    },
    isSSN : function() {
        return  this.type() == "personal_number";
    },
    isCompanyName : function() {
        return  this.type() == "company";
    },
    isCompanyNumber : function() {
        return  this.type() == "company_number";
    },
    isBlank: function() {
        return this.type() === '' && this.name() === '';
    },
    noName: function() {
        return this.name() === '' && this.type() == "text";
    },
    isStandard: function() {
        return  this.isName() || this.isEmail() || this.isMobile() || this.isSSN() || this.isCompanyName() || this.isCompanyNumber();
    },
    isCustom: function() {
        return this.type() == "text";
    },      
    isText : function() {
        return this.isStandard() || this.isCustom();
    },
    isSignature : function() {
        return this.type() == "signature";
    },
    isCheckbox : function() {
        return this.type() == "checkbox";
    },
    isOptionalUncheckedCheckbox: function () {
        return this.isCheckbox() && !this.isObligatory() && !this.isChecked();
    },
    isOptional : function() {
        return !this.obligatory();
    },
    isObligatory : function() {
        return this.obligatory();
    },
    isCsvField : function() {
        return this.isText() && this.signatory().isCsv() && this.signatory().hasCsvField(this.csvname());
    },
    csvname : function() {
      if (this.isFstName()) {
        return "fstname";
      } else if (this.isSndName()) {
        return "sndname";
      } else if (this.isEmail()) {
        return "email";
      } else if (this.isMobile()) {
        return "mobile";
      } else if (this.isSSN()) {
        return "sigpersnr";
      } else if (this.isCompanyName()) {
        return "sigco";
      } else if (this.isCompanyNumber()) {
        return "sigcompnr";
      } else {
        return this.name();
      }
    },
    isAuthorUnchangeableField :function() {
        return (this.isFstName() || this.isSndName() || this.isEmail()) && this.signatory().author();
    },
    csvFieldValues : function() {
       var csv = this.signatory().csv();
       var index = _.indexOf(csv[0],this.csvname());
       var res = [];
       for(var i = 1;i< csv.length;i++)
         res.push(csv[i][index]);
       return res;
    },
    defaultTip : function() {
      if (this.isCheckbox())
        return "left";
      return "right";
    },
    makeOptional : function() {
        this.set({"is_obligatory":false});
    },
    makeObligatory : function() {
        this.set({"is_obligatory":true});
    },
    canBeOptional : function() {
      if (this.signatory().author() && (this.isEmail() ||this.isFstName() || this.isSndName()))
        return false;
      if (this.isFstName() || this.isSndName())
        return false;
      if (this.isEmail())
        return !this.signatory().needsEmail();
      if (this.isMobile())
        return !this.signatory().needsMobile();
      if (this.isSSN())
        return !this.signatory().needsPersonalNumber();

      return true;
    },
    canBeSetByRecipent : function() {
      if (this.signatory().isViewer()) {
        return false;
      } else if (this.isEmail()) {
        return !(this.signatory().emailDelivery() || this.signatory().emailMobileDelivery());
      } else if (this.isMobile()) {
        return !(this.signatory().mobileDelivery() || this.signatory().emailMobileDelivery());
      } else if (this.isSSN()) {
        return !this.signatory().seBankIDAuthenticationToView();
      } else {
        return true;
      }
    },
    isReady: function(){
      return this.type() && (this.type() !== "text" || this.name() !== "");
    },
    makeReady : function() {
      this.trigger("ready");
    },
    remove: function(){
        this.signatory().deleteField(this);
        this.off();
        this.trigger("removed");
    },
    draftData : function() {
      return {
            type : this.type()
          , order : this.order()
          , name : this.name()
          , value : this.isText() ? this.value() : undefined
          , is_checked :  this.isCheckbox() ? this.isChecked() : undefined
          , placements : _.invoke(this.placements(), 'draftData')
          , is_obligatory : this.obligatory()
          , should_be_filled_by_sender : this.shouldbefilledbysender()
      };
    },
   dataForSigning: function() {
     if (this.isName()) {
       return {type: this.type(), order: this.order(), value: this.value()};
     } else if (this.isStandard()) {
       return {type: this.type(), value: this.value()};
     } else if (this.isText()) {
       return {type: this.type(), name: this.name(), value: this.value()};
     } else if (this.isCheckbox()) {
       return {type: this.type(), name: this.name(), is_checked: this.isChecked()};
     } else if (this.isSignature()) {
       return {type: this.type(), name: this.name(), signature: this.value()};
     }
   },
   hasPlacements : function() {
      return this.get("placements").length > 0;
   },
    addPlacement : function(placement) {
        if(!_.contains(this.placements(), placement)) {
            this.placements().push(placement);
            this.trigger('change');
        }
    },
   removePlacement : function(placement) {
       var newplacements = _.without(this.placements(), placement);
       this.set({placements : newplacements});
       if (newplacements.length == 0 && (this.isCheckbox() || this.isSignature()))
           this.signatory().deleteField(this);
       else if ((this.isFstName() || this.isSndName()) && newplacements.length == 0){
           this.signatory().trigger('change'); // Signatory will fallback to defaults obligatoriness if names don't have placements
       }
    },
    removeAllPlacements : function() {
        _.each(this.placements(), function(p) {p.remove();});
    },
    needsSenderAction : function() {
      var textNeedsValue = this.isText()
          && this.isObligatory()
          && this.shouldbefilledbysender()
          && !this.value();
      var checkboxNeedsToBeCheckedByAuthor = this.isCheckbox()
          && this.isObligatory()
          && this.signatory().author()
          && !this.isChecked();
      return textNeedsValue || checkboxNeedsToBeCheckedByAuthor || !this.isValid();
    },
    isValid: function() {
        var self = this;
        if (!this.isCsvField())
          return this.validation().validateData(this.value());
        else {
            return _.all(this.csvFieldValues(),function(v) {return self.validation().validateData(v); });
        }
    },
    requiredForParticipation: function() {
        var field = this;
        var sig = field.signatory();
        if(field.isEmail() && (sig.needsEmail() || sig.author()))
            return true;
        if(field.isSSN() && sig.needsPersonalNumber())
            return true;
        if(field.isMobile() && sig.needsMobile())
            return true;
        return false;
    },
    canBeRemoved: function() {
        if(this.isFstName() || this.isSndName())
            return false;
        else if(this.requiredForParticipation())
            return false;
        else if (this.isCsvField())
            return false;
        return true;
    },
    bindBubble: function() {
        var field = this;
        field.bind('change', function() {
          if(field.signatory()) {
            field.signatory().trigger('change');
          }
        });
    }
});



