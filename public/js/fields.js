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
      fsrel: 0
    },
    initialize : function(args){
        var placement = this;
        placement.addToPage();
        if (this.tip() == undefined)
          this.set({"tip" : args.field.defaultTip()});
        args.field.bind('removed', function() {
            placement.trigger("removed");
            placement.remove();
        });
    },
    placed : function() {
          return this.get("placed");
    },
    addToPage : function() {
         if (this.placed()) return;
         var placement = this;
         var field = placement.field();
         var signatory = field.signatory();
         var document = signatory.document();
         var pageno = this.get("page");
         var tryToAddToPage = function() {
                if (document.file() && document.file().page(pageno) != undefined) {
                    placement.set({placed : true});
                    document.file().page(pageno).addPlacement(placement);
                    document.off('file:change',tryToAddToPage);
                }
         };
         document.bind('file:change',tryToAddToPage);
         tryToAddToPage();
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
    file : function(){
        return this.get("file");
    },
    tip : function(){
        return this.get("tip");
    },
    remove : function() {
       var document = this.field().signatory().document();
       var page = document.file().page(this.get("page"));
       page.removePlacement(this);
       this.field().removePlacement(this);
       this.off();
    },
    draftData : function() {
<<<<<<< HEAD
      var document = this.field().signatory().document();
      var fileid = this.get("fileid");
      var page = document.getFile(fileid).page(this.get("page"));
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
=======
        var document = this.field().signatory().document();
        var page = document.file().page(this.get("page"));
        return {
            x : parseInt(this.x()),
            y : parseInt(this.y()),
            pagewidth : page != undefined ? page.width() : 943,
            pageheight : page != undefined ? page.height() : 1335,
            page : page != undefined ? page.number() : this.get("page"),
            tip : this.get("tip")
        };
>>>>>>> Rough design view with support for documentless templates
    }
});

window.Field = Backbone.Model.extend({
    defaults: {
        name : "",
        value : "",
        closed : false,
        placements : [],
        fresh : false
    },
    initialize : function(args){
        var field = this;
        var extendedWithField =   function(hash){
                    hash.field = field;
                    return hash;
        };
        var placements =  _.map(args.placements, function(placement){
                return new FieldPlacement(extendedWithField(placement));
        });
        this.set({"placements": placements});
        this.bind("change",function() {
            field.signatory().document().trigger("change:signatories");
        });
        args.signatory.bind("removed",function() {
            field.trigger("removed");
            field.off();
        });
        if (this.isSignature())
            this.set({"signature" : new Signature({field: this}, {silent : true})});
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
    setValue : function(value) {
        this.set({"value" : value});
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
    canBeIgnored: function(){
        return this.value() == "" && this.placements().length == 0 && (this.isStandard() || this.isSignature());
    },
    readyForSign : function(){
        if (this.isText() && (this.value() != ""))
            return true;
        else if (this.canBeIgnored())
            return true;
        else if (this.isSignature() && (this.signature().hasImage() || this.placements().length == 0))
            return true;
        else if (this.isOptionalCheckbox())
            return true;
        else if (this.isObligatoryCheckbox() && this.value() != "")
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

        if (!this.signatory().author() && this.isStandard() && (name == "fstname" ||name == "sndname") && !this.signatory().isCsv()) {
            return new NotEmptyValidation({message: localization.designview.validation.missingNames})
                   .concat(new NameValidation({message: localization.designview.validation.wrongNames}));
        }

        if (!this.signatory().author() && this.isStandard() && name == "email" && !this.signatory().isCsv() ){
            var msg = localization.designview.validation.missingOrWrongEmail;
            this.setValue(this.value().trim());
            return new EmailValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if (this.signatory().document().elegAuthentication() && this.isStandard() && name == "sigpersnr" && this.signatory().signs()  && !this.signatory().isCsv() ) {
            var msg = localization.designview.validation.missingOrWrongPersonalNumber;
            return new NotEmptyValidation({message: msg});
        }

        if (this.signatory().author() && this.isStandard() && this.hasPlacements() && forSigning) {
          var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
          return new NotEmptyValidation({message: msg});
        }
        if (this.signatory().author() && this.isObligatoryCheckbox() && forSigning) {
          var msg = localization.designview.validation.missingOrWrongPlacedAuthorField;
          return new NotEmptyValidation({message: msg});
        }
        
        if (this.isCustom()) {
          var msg1 = localization.designview.validation.notReadyField;
          var msg2 = localization.designview.validation.notPlacedField;
          var validation = new Validation({validates: function() {return field.isReady()}, message: msg1}).concat(new Validation({validates: function() {return field.isPlaced()}, message: msg2}));
          if (this.signatory().author() && forSigning) {
            return validation.concat(new NotEmptyValidation({message: localization.designview.validation.missingOrWrongCustomFieldValue}));
          } else {
            return validation;
          }
        }

        if (this.isCheckbox()) {
            var validation = new Validation({validates: function() {return field.name() != undefined && field.name() != "" }, message: localization.designview.validation.notReadyField});
            return validation;  
        }
        
        if (this.signatory().signs() && this.signatory().document().padDelivery() && this.isSignature()) {
            var msg = localization.designview.validation.notPlacedSignature;
            return new Validation({validates : function() {return field.hasPlacements()}, message : msg});
        }
        return new Validation();
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
        return this.isOptionalCheckbox() || this.isObligatoryCheckbox();
    },
    isOptionalCheckbox : function() {
        return this.type() == "checkbox-optional";
    },
    isObligatoryCheckbox : function() {
        return this.type() == "checkbox-obligatory";
    },
    defaultTip : function() {
      if (this.isCheckbox())
        return "left";
      return "right";
    },
    makeCheckboxOptional : function() {
        this.set({"type":"checkbox-optional"}, {silent: true});
    },
    makeCheckboxObligatory : function() {
        this.set({"type":"checkbox-obligatory"}, {silent: true});
    },
    signature : function() {
        return this.get("signature");
    },
    isReady: function(){
      return this.get("fresh") == false;
    },
    makeReady : function() {
      this.set({fresh: false});
      this.trigger("ready");
    },
    remove: function(){
      _.each(this.placements(),function(placement) {
            placement.remove();
        });
      this.signatory().deleteField(this);
      this.trigger("removed");
    },
    draftData : function() {
      return {
                 type : this.type()
               , name : this.name()
               , value : this.value()
               , placements : _.map(this.placements(), function(placement) {return placement.draftData();})
             };
    },
   hasPlacements : function() {
      return this.get("placements").length > 0;
   },
   addPlacement : function(placement) {
      var newplacements = new Array(); //Please don't ask why we rewrite this array
      for(var i=0;i<this.placements().length;i++)
         newplacements.push(this.placements()[i]);
      newplacements.push(placement);
      this.set({placements : newplacements});
    },
   removePlacement : function(placement) {
       var newplacements = new Array();
       for(var i=0;i<this.placements().length;i++)
          if (placement !== this.placements()[i])
             newplacements.push(this.placements()[i]);
       this.set({placements : newplacements});
       if (this.isCheckbox() && newplacements.length == 0)
           this.signatory().deleteField(this);

    },
    removeAllPlacements : function() {
        _.each(this.placements(), function(p) {p.remove();});
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
<<<<<<< HEAD
        if (document.mainfile() != undefined && document.mainfile().view != undefined) {

            draggebleField(icon, field);
=======
        if (document.mainfile() != undefined && document.mainfile().view != undefined)
        {   var fileview = field.signatory().document().mainfile().view;
            icon.draggable({
                    handle: ".ddIcon",
                    appendTo: "body",
                    helper: function(event) {
                        return new FieldPlacementView({model: field, el : $("<div/>")}).el;
                    },
                    start: function(event, ui) {
                        fileview.showCoordinateAxes(ui.helper);
                    },
                    stop: function() {
                        fileview.hideCoordinateAxes();
                    },
                    drag: function(event, ui) {
                        fileview.moveCoordinateAxes(ui.helper);
                    },
                    onDrop: function(page, x,y ){
                          x = page.fixedX(x,y);
                          y = page.fixedY(x,y);
                          field.addPlacement(new FieldPlacement({
                              page: page.number(),
                              field: field,
                              x : x,
                              y : y,
                              tip : "right"
                            }));
                    }
            });
>>>>>>> Rough design view with support for documentless templates
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
          if (field.isClosed())
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
