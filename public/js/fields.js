/* Fields of signatories */


(function(window){

window.Field = Backbone.Model.extend({
    defaults: {
        name : "",
        value : "",
        closed : false,
        placements : [],
        obligatory : true,
        shouldbefilledbysender : false,
        fresh : false
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
    setValueSilent : function(value) {
        this.set({"value" : value}, {silent: true});
    },
    setValue : function(value) {
        this.set({"value" : value});
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
                return localization.personamNumber;
            if (name == "sigcompnr")
                return localization.companyNumber;
            if (name == "mobile")
                return localization.phone;
        }
        return name;
    },
    nicetext : function() {
        if (this.value() != "")
          return this.value();
        else
          return this.nicename();
    },
    validation: function(forSigning) {
        var field = this;
        var name  = this.name();

        if (field.isFake())
           return new NoValidation();

        if( field.isBlank() ) {
            var msg = localization.designview.validation.pleaseSelectField;
            return new Validation({validates: function() {
                return field.type() && field.name();
            }, message: msg});
        }

        if(field.noName()) {
            var msg = localization.designview.validation.notReadyField;
            return new Validation({validates: function() {
                return !field.noName();
            }, message: msg});

        }

        if (   this.isEmail()
            && (this.signatory().emailDelivery() || this.signatory().emailMobileDelivery())
           ){
            var msg = localization.designview.validation.missingOrWrongEmail;
            return new EmailValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }
        if ((this.isFstName() || this.isSndName()) && this.signatory().author())
            return new NoValidation();

        if (this.isCheckbox() && this.signatory().author())
            return new NoValidation();

        if ( this.isEmail() && this.value() != undefined && this.value() != "") {
            var msg = localization.designview.validation.missingOrWrongEmail;
            console.log("Email validation");
            return new EmailValidation({message: msg});
        }

        if (   this.isMobile()
            && this.signatory().needsMobile()
           ){
            var msg = localization.designview.validation.missingOrWrongMobile;
            return new PhoneValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if (forSigning && this.signatory().author() && this.signatory().elegAuthentication() && this.isSSN()) {
            var msg = localization.designview.validation.missingOrWrongPersonalNumber;
            return new NotEmptyValidation({message: msg});
        }

        if(this.isObligatory() && forSigning && this.shouldbefilledbysender()) {
            var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
            return new NotEmptyValidation({message: msg});
        }

        if (this.signatory().author() && (this.isCheckbox() || this.isText()) && this.hasPlacements() && this.isObligatory() && forSigning) {
          var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
          return new NotEmptyValidation({message: msg});
        }

        if (this.isCheckbox()) {
            var validation = new Validation({validates: function() {return field.name() != undefined && field.name() != "" }, message: localization.designview.validation.notReadyField});
            return validation;
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
    isReady: function(){
      return this.get("fresh") == false && this.name() !== '' && this.type() !== '';
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

    },
    removeAllPlacements : function() {
        _.each(this.placements(), function(p) {p.remove();});
    },
    isValid: function(forSigning) {
        var self = this;
        if (!this.isCsvField())
          return this.validation(forSigning).validateData(this.value());
        else {
            var csvValues = this.csvFieldValues();
            console.log("Validating " + self.name() + " res: " + _.all(this.csvFieldValues(),function(v) {return self.validation(forSigning).validateData(v); }));
            return _.all(this.csvFieldValues(),function(v) {return self.validation(forSigning).validateData(v); })
        }
    },
    doValidate: function(forSigning, callback) {
        return this.validation(forSigning)
            .setCallback(callback)
            .validateData(this.value());
    },
    basicFields: ['fstname', 'sndname', 'email'],
    isBasic: function() {
        var field = this;
        return field.isStandard() && _.contains(field.basicFields, field.name());
    },
    requiredForParticipation: function() {
        var field = this;
        var sig = field.signatory();

        if(field.isSSN() && sig.needsPersonalNumber())
            return true;
        if(field.isMobile() && sig.needsMobile())
            return true;
        return false;
    },
    canBeRemoved: function() {
        if(this.isBasic())
            return false;
        else if(this.requiredForParticipation())
            return false;
        else if (this.isCsvField())
            return false;
        return true;
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



})(window);
