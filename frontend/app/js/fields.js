/* Fields of signatories */

define(['Backbone', 'legacy_code'], function() {

window.Field = Backbone.Model.extend({
    defaults: {
        name : "",
        value : "",
        closed : false,
        placements : [],
        obligatory : true,
        shouldbefilledbysender : false,
        fresh : false,
        ddSignature : false
    },
    initialize : function(args){
        var field = this;
        _.bindAll(field);
        var extendedWithField =   function(hash){
            hash.field = field;
            return hash;
        };
        var placements =  _.map(args.placements, function(placement){
                return new FieldPlacement(extendedWithField(placement));
        });
        this.set({"placements": placements});
        this.bind("change:value",function() {
            field.signatory().document().trigger("change-signatories-field-values");
        });
        if(args.signatory)
            args.signatory.bind("removed", field.remove);
        field.bindBubble();
    },
    type : function() {
        return this.get("type");
    },
    setType : function(t) {
        this.set({type:t});
    },
    name : function() {
        return this.get("name");
    },
    value : function() {
        return this.get("value");
    },
    valueTMP : function() {
        return this.get("valueTMP");
    },
    nameTMP : function() {
        return this.get("nameTMP");
    },
    obligatory : function() {
        return this.get("obligatory");
    },
    shouldbefilledbysender : function() {
        return this.get("shouldbefilledbysender");
    },
    setShouldBeFilledBySender : function(s) {
        this.set({shouldbefilledbysender:s});
        return this;
    },
    setValue : function(value, options) {
        this.set({"value" : value}, options);
        this.triggerSignatoryPostChanges();
    },
    triggerSignatoryPostChanges : function() {
        var name = this.name();
        if (this.isStandard()) {
            if (name == "fstname")
                this.signatory().trigger('change:name');
            if (name == "sndname" )
                this.signatory().trigger('change:name');
            if (name == "email")
                this.signatory().trigger('change:email');
            if (name == "sigco")
                this.signatory().trigger('change:company');
        }


    },
    setValueTMP : function(value) {
        this.set({"valueTMP" : value});
    },
    setName : function(name) {
        return this.set({"name" : name});
    },
    setNameTMP : function(value) {
        this.set({"nameTMP" : value});
    },
    isClosed : function() {
        return this.get("closed");
    },
    isFake : function() {
        return this.type() == 'fake';
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
    canBeIgnored: function(){
        return this.value() == "" && this.placements().length == 0 && (this.isStandard() || this.isSignature());
    },
    hasNotReadyPlacements : function() {
      return _.any(this.placements(), function(p) {if (p.step() != 'field' && p.step != 'edit') return (p.step() != 'field' && p.step() != 'edit');});
    },
    readyForSign : function(){
        if (this.isEmail())
            return new EmailValidation().validateData(this.value());
        else if (this.isText() && (this.value() != ""))
            return true;
        else if (this.isOptional())
            return true;
        else if (this.canBeIgnored())
            return true;
        else if (this.isSignature())
            return (this.value() != "" || this.placements().length == 0);
        else if (this.isObligatory() && this.value() != "")
            return true;

        return false;
    },
    nicename : function() {
        var name = this.name();
        if (this.isStandard()) {
            if (name == "fstname")
                return localization.fstname;
            if (name == "sndname" )
                return localization.sndname;
            if (name == "email")
                return localization.email;
            if (name == "sigco")
                return localization.company;
            if (name == "sigpersnr" )
                return localization.personalNumber;
            if (name == "sigcompnr")
                return localization.companyNumber;
            if (name == "mobile")
                return localization.phone;
        }
        if (this.isCustom() && name == "" && this.nameTMP() != undefined)
          name = this.nameTMP();
        return name;
    },
    nicetext : function() {
        var res = this.value() || this.nicename();

        // If text is empty add magic space to force some height
        if (res.trim() == "")
          res = '\xa0';

        return res;
    },
    // Validate the state of the field (not fake, not blank, etc) and the input
    validation: function() {
      var field = this;
      var signatory = field.signatory();

      if(field.isFake()) {
        return new NoValidation();
      }
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
      var senderMustFill = field.isObligatory() && field.shouldbefilledbysender()
       || signatory.isLastViewer() && (field.isEmail() && (signatory.emailConfirmationDelivery() || signatory.emailMobileConfirmationDelivery()) ||
                                       field.isMobile() && (signatory.mobileConfirmationDelivery() || signatory.emailMobileConfirmationDelivery()));
      var willSignNowAndFieldNeeded = signatory.author()
        && signatory.ableToSign()
        && (!signatory.hasPlacedSignatures())
        && field.isObligatory()
        && (field.isText() || field.isCheckbox());
      if(senderMustFill || willSignNowAndFieldNeeded) {
        concatValidations = new NotEmptyValidation();
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
    validateSSN: function() {
      var signatory = this.signatory();
      if(signatory.ableToSign() && signatory.author() && signatory.elegAuthentication()) {
        return new NotEmptyValidation();
      }
      return new Validation();
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
      if(signatory.mobileDelivery() || signatory.emailMobileDelivery()) {
        return new PhoneValidation().concat(new NotEmptyValidation());
      }
      if(signatory.smsPinAuthentication()            ||
         signatory.emailMobileConfirmationDelivery() ||
         signatory.mobileConfirmationDelivery()
        ) {
        return new PhoneValidation().or(new EmptyValidation());
      }
      return new Validation();
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
        return  this.isStandard() && this.name() == "email";
    },
    isMobile: function() {
        return  this.isStandard() && this.name() == "mobile";
    },
    isFstName: function() {
        return  this.isStandard() && this.name() == "fstname";
    },
    isSndName: function() {
        return  this.isStandard() && this.name() == "sndname";
    },
    isSSN : function() {
        return  this.isStandard() && this.name() == "sigpersnr";
    },
    isBlank: function() {
        return this.type() === '' && this.name() === '';
    },
    noName: function() {
        return this.name() === '';
    },
    isStandard: function() {
        return  this.type() == "standard";
    },
    isCustom: function() {
        return this.type() == "custom";
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
    isOptional : function() {
        return !this.obligatory();
    },
    isObligatory : function() {
        return this.obligatory();
    },
    isCsvField : function() {
        return this.isText() && this.signatory().isCsv() && this.signatory().hasCsvField(this.name());
    },
    isAuthorUnchangeableField :function() {
        return (this.isFstName() || this.isSndName() || this.isEmail()) && this.signatory().author();
    },
    csvFieldValues : function() {
       var csv = this.signatory().csv();
       var index = _.indexOf(csv[0],this.name());
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
        this.set({"obligatory":false});
    },
    makeObligatory : function() {
        this.set({"obligatory":true});
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
      if (this.isEmail())
        return !(this.signatory().emailDelivery() || this.signatory().emailMobileDelivery());
      if (this.isMobile())
        return !(this.signatory().mobileDelivery() || this.signatory().emailMobileDelivery());
      return true;
    },
    isReady: function(){
      return this.get("fresh") == false && this.name() !== '' && this.type() !== '';
    },
    isDDSignature : function() {
      return this.get("ddSignature");
    },
    makeReady : function() {
      this.set({fresh: false});
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
          , name : this.name()
          , value : this.value()
          , placements : _.invoke(this.placements(), 'draftData')
          , obligatory : this.obligatory()
          , shouldbefilledbysender : this.shouldbefilledbysender()
      };
    },
   hasPlacements : function() {
      return this.get("placements").length > 0;
   },
    addPlacement : function(placement) {
        if(!_.contains(this.placements(), placement)) {
            this.placements().push(placement);
            this.trigger('change change:placements');
        }
    },
   removePlacement : function(placement) {
       var newplacements = _.without(this.placements(), placement);
       this.set({placements : newplacements});
       if (this.isCheckbox() && newplacements.length == 0)
           this.signatory().deleteField(this);
       else if ((this.isFstName() || this.isSndName()) && newplacements.length == 0){
           this.signatory().trigger('change'); // Signatory will fallback to defaults obligatoriness if names don't have placements
       }
    },
    removeAllPlacements : function() {
        _.each(this.placements(), function(p) {p.remove();});
    },
    needsSenderAction : function() {
      return (this.isObligatory()
          && this.shouldbefilledbysender()
          && !this.value()
          ) || !this.isValid();
    },
    isValid: function() {
        var self = this;
        if (!this.isCsvField())
          return this.validation().validateData(this.value());
        else {
            var csvValues = this.csvFieldValues();
            console.log("Validating " + self.name() + " res: " + _.all(this.csvFieldValues(),function(v) {return self.validation().validateData(v); }));
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
    isIdentificationField : function() {
      return this.isEmail() || this.isMobile() || this.isSSN();
    },
    isLastIdentificationField : function() {
       return this.isIdentificationField() && (this.signatory().identificationFields().length == 1);
    },
    isLastNonOptionalIdentificationField : function() {
       var nonOptionalIdentificationFieldCount = _.filter(this.signatory().identificationFields(),function(f) {return !f.isOptional()}).length;
       return this.isIdentificationField() && !this.isOptional() && (nonOptionalIdentificationFieldCount == 1);
    },
    bindBubble: function() {
        var field = this;
        field.bind('change', field.bubbleSelf);
        field.bind('bubble', field.triggerBubble);
    },
    bubbleSelf: function() {
        var field = this;
        field.trigger('bubble');
    },
    triggerBubble: function() {
        var field = this;
        var signatory = field.signatory();
        if(signatory)
            signatory.trigger('bubble');
    }
});



});
