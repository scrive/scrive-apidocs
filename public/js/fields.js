/* Fields of signatories
 * Placement of fields + support for moving them to files
 * View of field in sign view
 */


(function(window){

window.FieldPlacement = Backbone.Model.extend({
    defaults: {
        x : 0,
        y : 0,
        placed : false

    },
    initialize : function(args){
        var placement = this;
        setTimeout(function() {placement.addToPage();},100);
        args.field.bind('removed', function() {
            placement.trigger("removed")
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
         var fileid = this.get("fileid");
         if ( document.getFile(fileid) != undefined)
         {
             var page = document.getFile(fileid).page(this.get("page"));
             if (page != undefined)
             {
                this.set({placed : true});
                page.addPlacement(placement);
             }

          }
         return setTimeout(function() {placement.addToPage();},100);

    },
    x : function() {
        return this.get("x");
    },
    y : function() {
        return this.get("y");
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
    remove : function() {
       var document = this.field().signatory().document();
       var fileid = this.get("fileid");
       var page = document.getFile(fileid).page(this.get("page"));
       page.removePlacement(this);
       this.field().removePlacement(this);
       this.off();
    },
    draftData : function() {
        var document = this.field().signatory().document();
        var fileid = this.get("fileid");
        var page = document.getFile(fileid).page(this.get("page"));
        return {
            x : parseInt(this.x()),
            y : parseInt(this.y()),
            pagewidth : page.width(),
            pageheight : page.height(),
            page : page.number()
        };
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
    },
    name : function() {
        return this.get("name");
    },
    value : function() {
        return this.get("value");
    },
    setValue : function(value) {
        return this.set({value : value});
    },
    setName : function(name) {
        return this.set({name : name});
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
        var name = this.name();
        return this.value() == "" && this.placements().length == 0 && (name == "fstname" || name == "sndname" || name == "sigco" || name == "sigpersnr" || name == "sigcompnr" || name == "signature");
    },
    hasRestrictedName : function() {
        return this.isStandard() || this.isSignature(); //this checks are name based
    },
    readyForSign : function(){
        return this.value() != "" || this.canBeIgnored();
    },
    nicename : function() {
        var name = this.name();
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
        return name;
    },
    nicetext : function() {
        if (this.value() != "")
          return this.value();
        else
          return this.nicename();
    },
    validation: function() {
        var field = this;
        var name  = this.name();

        if (!this.signatory().author() && (name == "fstname" ||name == "sndname") && !this.signatory().isCsv()) {
            var msg = localization.designview.validation.missingOrWrongNames;
            return new NameValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if (!this.signatory().author() && name == "email" && !this.signatory().isCsv() ){
            var msg = localization.designview.validation.missingOrWrongEmail;
            return new EmailValidation({message: msg}).concat(new NotEmptyValidation({message: msg}));
        }

        if (this.signatory().document().elegAuthorization() && name == "sigpersnr" && this.signatory().signs()  && !this.signatory().isCsv() ) {
            var msg = localization.designview.validation.missingOrWrongPersonalNumber;
            return new NotEmptyValidation({message: msg});
        }

        if (this.isCustom()) {
          var msg1 = localization.designview.validation.notReadyField;
          var msg2 = localization.designview.validation.notPlacedField;
          var validation = new Validation({validates: function() {return field.isReady()}, message: msg1}).concat(new Validation({validates: function() {return field.isPlaced()}, message: msg2}));
          if (this.signatory().author()) {
            return validation.concat(new NotEmptyValidation({message: localization.designview.validation.missingOrWrongCustomFieldValue}));
          } else {
            return validation;
          }
        }
        if (this.signatory().signs() && this.signatory().document().padAuthorization() && this.isSignature()) {
            var msg = localization.designview.validation.notPlacedSignature
            return new Validation({validates : function() {return field.hasPlacements()}, message : msg});
        }
        return new Validation();
    },
    isStandard: function() {
        var name = this.name();
        return  (name == "fstname")
             || (name == "sndname" )
             || (name == "email")
             || (name == "sigco")
             || (name == "sigpersnr" )
             || (name == "sigcompnr");
    },
    isCustom: function() {
        return !this.isStandard() && !this.isSignature();
    },
    isSignature : function() {
        return this.name() == "signature";

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
      return {   name : this.name()
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

window.FieldBasicDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'cleanredborder');
        this.model.view = this;
        this.model.bind('change', this.cleanredborder);
        this.model.signatory().document().bind('change:authorization', this.cleanredborder);
        this.render();
    },
    render: function(){
        var field = this.model;
        $(this.el).empty();
        var signatory = field.signatory();
        $(this.el).addClass("field");
        var input = InfoTextInput.init({
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
            input.attr("readonly","yes");
        }
        $(this.el).append(input);
        return this;
    },
    redborder : function() {
        $(this.el).addClass("redborder");
    },
    cleanredborder : function() {
        $(this.el).removeClass("redborder");
    }
});

window.FieldAdvancedDesignView = FieldBasicDesignView.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'cleanredborder');
        this.model.bind('change', this.cleanredborder);
        this.model.bind('ready', this.render);
        this.model.signatory().document().bind('change:authorization', this.cleanredborder);
        this.model.view = this;
        this.render();
    },
    ddIcon : function() {
        var icon =  $("<div class='ddIcon' />");
        var field = this.model;
        var document = field.signatory().document();
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
                          field.addPlacement(new FieldPlacement({
                              page: page.number(),
                              fileid: page.file().fileid(),
                              field: field,
                              x : x,
                              y : y
                            }));
                    }
            });
        }
        return icon;
    },
    prepIcon : function() {
        return $("<a class='prepIcon' href='#'/>");
    },
    setNameIcon : function() {
        var field = this.model;
        var input = this.input;
        var icon =  $("<a class='setNameIcon' href='#'/>");
        var fn = function(){
          if (!field.hasRestrictedName())
            field.makeReady();
          else  FlashMessages.add({
            color : "red",
            content: localization.designview.validation.restrictedName
          });
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
