/* Fields of signatories
 * Placement of fields + support for moving them to files
 * View of field in sign view
 */


(function(window){

window.FieldPlacement = Backbone.Model.extend({
    defaults: {
      placed : false,
      tip: undefined,
      wrel: 0,
      hrel: 0,
      fsrel: 0,
      withTypeSetter : false
    },
    initialize : function(args){
        var placement = this;
        _.bindAll(placement);
        placement.addToPage();
        if (this.tip() === undefined)
          this.set({"tip" : args.field.defaultTip()});
        this.setField(args.field);
    },
    placed : function() {
        return this.get("placed");
    },
    addToPage : function() {
        if (this.placed())
            return;
        var placement = this;
        var field = placement.field();
        var signatory = field.signatory();
        var document = signatory.document();
        var pageno = this.get("page");
        var tryToAddToPage = function() {
            if (document.file() && document.file().page(pageno) != undefined) {
                document.file().page(pageno).addPlacement(placement);
                placement.set({placed : true});
                document.off('file:change',tryToAddToPage);
            }
        };
        document.bind('file:change',tryToAddToPage);
        setTimeout(tryToAddToPage,0);
    },
    xrel : function() {
        return this.get("xrel");
    },
    yrel : function() {
        return this.get("yrel");
    },
    wrel : function() {
        return this.get("wrel");
    },
    hrel : function() {
        return this.get("hrel");
    },
    fsrel : function() {
        return this.get("fsrel");
    },
    page : function() {
        return this.get("page");
    },
    field : function(){
        return this.get("field");
    },
    setField: function(f) {
        var oldfield = this.field();
        if(oldfield !== f) {
            if(f) {
                f.bind('removed', this.remove);
                f.addPlacement(this);
            }
            if(oldfield) {
                oldfield.unbind('removed', this.remove);
                oldfield.removePlacement(this);
            }
            this.set({field:f});
        }
    },
    file : function(){
        return this.get("file");
    },
    tip : function(){
        return this.get("tip");
    },
    withTypeSetter : function() {
      return this.get("withTypeSetter") == true;
    },
    cleanTypeSetter : function() {
       this.set({"withTypeSetter" : false}, {silent: true});
    },
    remove : function() {
       this.trigger("removed");
       var document = this.field().signatory().document();
       var page = document.file().page(this.get("page"));
       page.removePlacement(this);
       this.field().removePlacement(this);
       this.off();
    },
    fixWHRel : function(w,h) {
      var page =  this.field().signatory().document().mainfile().page(this.get("page"));
      if (page != undefined && page.width() != undefined && page.height() != undefined)
          this.set({ wrel : (w / page.width()) , hrel : (h / page.height()) },    {silent : true});
    },
    draftData : function() {
      var document = this.field().signatory().document();
      var page = document.mainfile() != undefined ? document.mainfile().page(this.get("page")) : undefined;
      var draft = {
        xrel : this.xrel(),
        yrel : this.yrel(),
        wrel : this.wrel(),
        hrel : this.hrel(),
        fsrel : this.fsrel(),
        page : page != undefined ? page.number() : this.get("page"),
        tip : this.get("tip")
      };
      return draft;
    }
});

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
        //this.bind("change",function() {
        //    field.signatory().document().trigger("change:signatories");
        //});
        if(args.signatory)
            args.signatory.bind("removed", field.remove);
    },
    type : function() {
        return this.get("type");
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
    setValue : function(value) {
        this.set({"value" : value});
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

        if (   this.isEmail()
            && !this.signatory().isCsv()
            && (this.signatory().emailDelivery())){
            var msg = localization.designview.validation.missingOrWrongEmail;
            this.setValue(this.value().trim());
            return new EmailValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if ( this.isEmail() && this.value() != undefined && this.value() != "") {
            var msg = localization.designview.validation.missingOrWrongEmail;
            this.setValue(this.value().trim());
            return new EmailValidation({message: msg});
        }

        if (   this.isMobile()
            && !this.signatory().isCsv()
            && (this.signatory().mobileDelivery())){
            var msg = localization.designview.validation.missingOrWrongMobile;
            this.setValue(this.value().trim());
            return new PhoneValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if ( this.isMobile() && this.value() != undefined && this.value() != "") {
            var msg = localization.designview.validation.missingOrWrongMobile;
            this.setValue(this.value().trim());
            return new PhoneValidation({message: msg});
        }

        if (forSigning && this.signatory().author() && this.signatory().document().elegAuthentication() && this.isSSN()) {
            var msg = localization.designview.validation.missingOrWrongPersonalNumber;
            return new NotEmptyValidation({message: msg});
        }

        if (this.signatory().author() && this.isStandard() && this.hasPlacements() && forSigning) {
          var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
          return new NotEmptyValidation({message: msg});
        }
        if (this.signatory().author() && (this.isCheckbox() || this.isText()) && this.hasPlacements() && this.isObligatory() && forSigning) {
          var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
          return new NotEmptyValidation({message: msg});
        }

        if (this.isCustom()) {
          var msg1 = localization.designview.validation.notReadyField;
          var msg2 = localization.designview.validation.notPlacedField;
          var validation = new Validation({validates: function() {return field.isReady()}, message: msg1}).concat(new Validation({validates: function() {return field.isPlaced()}, message: msg2}));
          if (this.signatory().author() && forSigning) {
            return validation.concat(new NotEmptyValidation({message: localization.designview.validation.missingOrWrongPlacedAuthorField}));
          } else {
            return validation;
          }
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
    defaultTip : function() {
      if (this.isCheckbox())
        return "left";
      return "right";
    },
    makeOptionalM : function() {
        this.set({"obligatory":false});
    },
    makeObligatoryM : function() {
        this.set({"obligatory":true});
    },
    makeOptional : function() {
        this.set({"obligatory":false}, {silent: true});
    },
    makeObligatory : function() {
        this.set({"obligatory":true}, {silent: true});
    },
    isReady: function(){
      return this.get("fresh") == false;
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
        this.placements().push(placement);
        this.trigger('change:placements');
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
        return this.validation(forSigning).validateData(this.value());
    },
    doValidate: function(forSigning, callback) {
        return this.validation(forSigning)
            .setCallback(callback)
            .validateData(this.value());
    },
    basicFields: ['fstname', 'sndname', 'email', 'sigco'],
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
        else
            return true;
    }
});

window.FieldStandardView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'cleanredborder');
         this.model.bind('change:inlineedited', this.render);
        this.model.bind('change', this.cleanredborder);
        this.model.view = this;
        this.render();
    },
    render: function(){
        var field = this.model;
        $(this.el).empty();
        var signatory = field.signatory();
        if (!signatory.canSign() || field.isClosed() || !signatory.current()) {
            $(this.el).addClass("field");
            input = $("<input class='fieldvalue' autocomplete='off' readonly=''/>");
            if (field.value() != "")
                input.val(field.value());
            else
                {
                input.val(field.nicename());
                $(this.el).addClass("required");
                input.addClass("grayed");
                }
        $(this.el).append(input);
        }
        else
        {
            $(this.el).addClass("dragfield");
            var wrapper = $("<div style='border: medium none ! important;' class='dragfield'/>");
            if (field.value() ==  "" && SessionStorage.get(signatory.document().documentid(),field.name()) != undefined ) {
                field.setValue(SessionStorage.get(signatory.document().documentid(),field.name()));
            }
            var input = InfoTextInput.init({
                                 infotext: field.nicename(),
                                 value: field.value(),
                                 cssClass :'fieldvalue',
                                 onChange : function(value) {
                                    field.setValue(value);
                                    SessionStorage.set(signatory.document().documentid(),field.name(),value);
                                  }
                            }).input();
            this.redborderhere = input;
            input.attr("autocomplete","off");
            wrapper.append(input);
            $(this.el).append(wrapper);
        }

        return this;
    },
    redborder : function() {
        if (this.redborderhere != undefined)
            this.redborderhere.css("border","1px solid red");
    },
    cleanredborder : function() {
        if (this.redborderhere != undefined)
            this.redborderhere.css("border","");
    }
});


// this is unused in new design view
// Eric
window.FieldDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'cleanredborder');
        this.model.bind('change', this.cleanredborder);
        this.model.bind('ready', this.render);
        this.model.signatory().document().bind('change:authenticationdelivery', this.cleanredborder);
        this.model.view = this;
        this.render();
    },
    ddIcon : function() {
        var icon =  $("<div class='ddIcon' />");
        var field = this.model;
        var document = field.signatory().document();
        if (document.mainfile() != undefined && document.mainfile().view != undefined) {
            draggebleField(icon, field);
        }
        return icon;
    },
    prepIcon : function() {
        return $("<a class='prepIcon' href='#'/>");
    },
    setNameIcon : function() {
        var view = this;
        var field = this.model;
        var input = this.input;
        var icon =  $("<a class='setNameIcon' href='#'/>");
        var fn = function(){
            if (field.name() != undefined && field.name() != "")
               field.makeReady();
            else
               view.redborder();
            return false;
        };
        icon.click(fn);
        input.keypress(function(event) {
          if(event.which === 13)
            return fn();
        });

        // I'm not putting a blur handler that sets the name
        // because it's a little odd. Neither is perfect. Eric

        return icon;
    },
    removeIcon: function() {
        var field = this.model;
        var icon = $("<a class='removeField' href='#'/>");
        icon.click(function(){
            field.remove();
            return false;
        });
        return icon;
    },
    render: function(){
        var field = this.model;
        $(this.el).empty();
        var signatory = field.signatory();
        $(this.el).addClass("field");
        if (field.isReady()) {
          $(this.el).append(this.ddIcon());
          this.input = InfoTextInput.init({
                                 infotext: field.nicename(),
                                 value: field.value(),
                                 cssClass :'fieldvalue',
                                 inputname : field.name(), //Added only for selenium tests
                                 onChange : function(value) {
                                    field.setValue(value);
                                  }
                            }).input();
          if  ( field.isClosed() ||
              ((field.isEmail() || field.isFstName() || field.isSndName()) && field.signatory().author()) ||
              ( field.isStandard() && field.signatory().author() && field.signatory().document().isTemplate()))
            {
                $(this.el).addClass('closed');
                this.input.attr("readonly","yes");
            }
          else if (!field.isStandard())
           {
               this.input.addClass("shorter");
               $(this.el).append(this.removeIcon());
           }
          $(this.el).append(this.input);
        }
        else {
           $(this.el).append(this.prepIcon());
           this.input = InfoTextInput.init({
                                 infotext: localization.fieldName,
                                 value: field.name(),
                                 cssClass :'fieldvalue',
                                 inputname : field.name(), //Added only for selenium tests
                                 onChange : function(value) {
                                    field.setName(value);
                                  }
               }).input();
           this.input.addClass("much-shorter");
           $(this.el).append(this.input);
           $(this.el).append(this.removeIcon());
           $(this.el).append(this.setNameIcon());

        }
        return this;
    },
    redborder : function() {
        $(this.el).addClass("redborder");
    },
    cleanredborder : function() {
        $(this.el).removeClass("redborder");
    }
});

})(window);
