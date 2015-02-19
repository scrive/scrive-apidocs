define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor, Backbone) {

window.TextPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear', 'closeTypeSetter', 'updateErrorBackground', 'fixWHRel');
        var view = this;
        var placement = this.model;
        var field =  placement.field();
        var signatory = field?field.signatory():placement.signatory();
        this.arrow = args.arrow;

        this.model.bind('removed', this.clear, this);
        this.model.bind('change:field change:signatory change:step change:withTypeSetter change:fsrel', this.render);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel', this.updatePosition, this);
        this.model.bind('clean', this.closeTypeSetter);
        placement.bind('change', this.updateErrorBackground);

        this.model.view = this;
        this.render();
    },
    fontSize : function() {
        var parent = $(this.el).parent();
        if( parent.length>0 ) return Math.floor(this.model.fsrel() * parent.width());
        return 16;
    },
    updatePosition: function() {
        /*
         * There is a series of these updatePosition functions, all
         * the same.  We need to round position down to nearest
         * integer. We need to ceil size because if we do not then we
         * end up with place not big enough to fit text and it will
         * wrap or be cropped.
         */
        var placement = this.model;
        var place = $(this.el);
        var parent = place.parent();
        if( parent.length>0 ) {
            /*
             * We set size only when we have parent. If drag was just
             * started then drag helper does not have parent and will
             * use default size for itself.
             */
            var parentWidth = parent.width();
            var parentHeight = parent.height();
            place.css({
                left: Math.floor(placement.xrel() * parentWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
                top: Math.floor(placement.yrel() * parentHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset,
                fontSize: Math.floor(placement.fsrel() * parentWidth)
            });
        }
    },
    clear: function() {
        var placement = this.model;
        var field =  placement.field();
        var signatory = field?field.signatory():placement.signatory();
        this.off();
        $(this.el).remove();
        this.model.unbind('removed', this.clear, this);
        this.model.unbind('change:field change:signatory', this.render);
        this.model.unbind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.unbind('clean', this.closeTypeSetter);
        this.stopListening();
    },
    hasTypeSetter : function(){
        return this.model.typeSetter != undefined;
    },
    addTypeSetter : function() {
         var placement = this.model;
         if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
             placement.typeSetter = new TextTypeSetterView({model : placement});
             $('body').append(placement.typeSetter.el);
             setTimeout(function() {
                 placement.typeSetter.place();
             }, 0);
         }
    },
    closeTypeSetter : function() {
         var placement = this.model;
         if (this.hasTypeSetter()) {
             placement.typeSetter.clear();
         }
    },
    startInlineEditing : function() {
        var placement = this.model;
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);
        var view = this;
        var self = this;
        if (self.inlineediting == true) {
          if (self.input != undefined) {
               if ($(window).scrollTop() + $(window).height() > self.input.el().offset().top && $(window).scrollTop() < self.input.el().offset().top) {
                  self.input.focus();
               }

          }
          return false;
        }

        view.inlineediting = true;
        var width = place.width() > 30 ? place.width() : 30;
        var parent = place.parent();
        if( parent.length>0 ) { // Check max width so we don't expand inline editing over page width.
          var maxWidth = (1 - placement.xrel()) * parent.width() - 36;
          if (maxWidth < width) width = maxWidth;
          if (width < 30) width = 30;

        }
        place.addClass('active');
        var accept = function() {
                      view.inlineediting = false;
                      place.removeClass('active');
                      var val = self.input.value();
                      field.setValue(val);
                      field.signatory().trigger('change');
                      view.render();
                      field.trigger('change:inlineedited');
        };

        self.input = new InfoTextInput({
          infotext: field.nicename(),
          value : field.value(),
          cssClass : "text-inline-editing",
          autoGrowth : true,
          style: "font-size:" + this.fontSize() + "px;" +
                 "line-height: 1;" +
                 "height:"+ (this.fontSize() + 4) +"px;" +
                 "border-width: 0px;" +
                 "padding:" + FieldPlacementGlobal.textPlacementSpacingString + ";",
          inputStyle : "font-size:" + this.fontSize() + "px ;" +
                       "line-height: " + (this.fontSize()+ FieldPlacementGlobal.textPlacementExtraLineHeight) + "px;" +
                       "height:"+ (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) +"px;" +
                       "width: " + width +"px;"+
                       "background:transparent;",
          okStyle :  "font-size:" + this.fontSize() + "px ;" +
                     "line-height: " + (this.fontSize()+ FieldPlacementGlobal.textPlacementExtraLineHeight) + "px;" +
                     "height:"+ (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight/2) +"px;",
          onEnter : accept,
          onAutoGrowth : function() {
            if (self.arrow() != undefined)
              self.arrow().updatePosition();
          },
          onTab : accept,
          onBlur : accept,
          onOk : accept
        });
        place.empty().css("z-index","1").append(self.input.el());
        self.input.render(); // Input has autogrowth on it - and it can't determine proper width before element is mounted. Render forces autogrowth recalculation.
        field.trigger('change:inlineedited');
        field.bind('change',function() { view.inlineediting  = false;place.removeClass('active'); view.render();});

        if ($(window).scrollTop() + $(window).height() > self.input.el().offset().top && $(window).scrollTop() < self.input.el().offset().top) {
                   self.input.focus();
        }
        return false;
    },
    updateErrorBackground: function() {
        var placement = this.model;
        var field = placement.field();
        var document = field.signatory().document();
        if(field) {
            field.unbind('change', this.updateErrorBackground);
            field.bind('change', this.updateErrorBackground);
        }
        $(this.el).toggleClass("invalid",field && !field.isValid(true));
    },
    possibleFields: [
        {name: "fstname",
         type: 'standard'},
        {name: "sndname",
         type: 'standard'},
        {name: "email",
         type: 'standard'},
        {name: "sigco",
         type: 'standard'},
        {name: "sigpersnr",
         type: 'standard'},
        {name: "sigcompnr",
         type: 'standard'},
        {name: "mobile",
         type: 'standard'}
    ],
    fieldNames: {
        fstname: localization.fstname,
        sndname: localization.sndname,
        email: localization.email,
        sigcompnr: localization.companyNumber,
        sigpersnr: localization.personalNumber,
        sigco: localization.company,
        mobile: localization.phone
    },
    selector : function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();
        var sig = field.signatory();
        var doc = sig.document();

        var box = $("<div class='subtitle'/>");

        var name = localization.designview.chooseParticipant;

        var options = _.map(doc.signatories(), function(s) {
            return {name: s.nameOrEmail() || s.nameInDocument(),
                    value: s};
        });

        var selector = new Select({
            name: name,
            options: options,
            cssClass: 'text-field-placement-setter-field-selector',
            border : "",
            textWidth: undefined,
            onSelect: function(s) {
                mixpanel.track('Select placement signatory');
                placement.setSignatory(s);
                placement.goToStepField();
                return true;
            }
        });

        box.append(selector.el());

        return box;
    },
    fieldSelector: function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();

        var signatory = placement.signatory();

        var name = localization.designview.selectField;

        // we need to build a list of all of the different field name/type pairs
        // plus the ability to add a custom field

        // clone the array
        var allFieldOptions = view.possibleFields.concat([]);

        _.each(signatory.fields(), function(f) {
                if(f.isCustom() && f.name() !== '')
                    allFieldOptions.push({name: f.name(),
                                          type: f.type()});
        });

        var options = [];

        if(!field || field.name() !== '')
            options.push({name: localization.designview.customField,
                          value: {name: '--custom',
                                  type: '--custom'}});

        _.each(allFieldOptions, function(o) {
            options.push({name: view.fieldNames[o.name] || o.name,
                          value: o});
        });

        var selector = new Select({
            name: name,
            options: options,
            cssClass: ('text-field-placement-setter-field-field-selector ' + FieldPlacementGlobal.signatoryCSSClass(this.model.signatory())),
            border : "",
            textWidth : undefined,
            onSelect: function(o) {
                var f = signatory.field(o.name, o.type);

                if(o.name === '--custom') {
                    mixpanel.track('Choose placement type', {
                        Type: 'custom'
                    });

                    f = new Field({signatory: signatory,
                                   type: 'custom',
                                   name: '',
                                   obligatory: true,
                                   shouldbefilledbysender: signatory.author()});
                    placement.setField(f);
                    f.addPlacement(placement);

                    signatory.addField(f);
                    f.addedByMe = true;
                } else if(f) {
                    mixpanel.track('Choose placement type', {
                        Type: o.type,
                        Name: o.name
                    });
                    placement.setField(f);
                    f.addPlacement(placement);
                } else {
                    mixpanel.track('Choose placement type', {
                        Type: o.type,
                        Name: o.name
                    });
                    f = new Field({signatory: signatory,
                                   type: o.type,
                                   name: o.name,
                                   obligatory: true,
                                   shouldbefilledbysender: signatory.author()});
                    placement.setField(f);
                    f.addPlacement(placement);

                    signatory.addField(f);
                    f.addedByMe = true;
                }

                placement.goToStepEdit();
                view.addTypeSetter();
                return true;
            }
        });

        view.myFieldSelector = selector;

        return selector.el();
    },
    fieldNamer: function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();

        var signatory = placement.signatory();

        var div = $('<div />');
        div.addClass('text-field-placement-setter-field-name');
        div.addClass(FieldPlacementGlobal.signatoryCSSClass(signatory));

        function setName() {
            var value = input.value();
            if(value) {

                var samenameexists = _.any(signatory.customFields(), function(c) { return value == c.name();});
                if (samenameexists) {
                  new FlashMessage({type: 'error', content : localization.designview.fieldWithSameNameExists});
                  return;
                }
                field.setName(value);
                if (view.place != undefined)
                  view.place();

                mixpanel.track('Set placement field name');
                placement.trigger('change:field');
                signatory.trigger('change:fields');
                view.addTypeSetter();
            }
        }

        var input = new InfoTextInput({
            infotext: localization.designview.fieldName,
            value: field.name() || field.nameTMP(),
            cssClass: "name",
            onChange : function(value) {
                field.setNameTMP(value);
                if (view.place != undefined)
                  view.place();
            },
            onEnter: setName,
            suppressSpace: (field.name()=="fstname")
        });

        var button = new Button({
            size: 'tiny',
            text: localization.ok,
            width: 64,
            onClick: setName
        });

        div.append(input.el());
        div.append(button.el());
        return div;
    },
    editor: function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();

        var input = new InfoTextInput({
            cssClass: 'text-field-placement-setter-field-editor ' + FieldPlacementGlobal.signatoryCSSClass(this.model.signatory()),
            infotext: field.nicename(),
            style: "font-size:" + this.fontSize() + "px ;" +
                   "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) +  "px;" +
                   "height:"+ (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) +"px; " +
                   "padding:" + FieldPlacementGlobal.textPlacementSpacingString + ";",
            inputStyle : "font-size:" + this.fontSize() + "px ; " +
                         "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px; " +
                         "height:"+ (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) +"px; " +
                         "vertical-align: top;",
            value: field.value(),
            suppressSpace: (field.name()=="fstname"),
            onChange: function(val) {
                field.setValue(val.trim());
            },
            onEnter : function(val) {
                  view.closeTypeSetter();
                  view.model.cleanTypeSetter();
                  view.render();
            }
        });

        return input;
    },
    fixWHRel : function() {
        // This will silently update wrel and hrel. Should be called after all changes to w/h are applied. Not 100% reliable
        if ($(this.el).width() != undefined && $(this.el).width() > 0 && $(this.el).height() != undefined && $(this.el).height() > 0)
          this.model.fixWHRel($(this.el).width(),$(this.el).height());
    },
    render: function() {
        var self = this;
        var placement = this.model;
        var field =  placement.field();
        var signatory = placement.signatory()||field.signatory();
        var document = signatory.document();
        var place = $(this.el);
        var placewrapper = $("<div class='placedfield-placement-wrapper'>");

        place.addClass('placedfield');
        this.updateErrorBackground();

        if ((signatory == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation())
              place.css('cursor','pointer');

        this.updatePosition();

        place.empty();

        if(placement.step() === 'signatory') {
            place.append(this.selector());
        } else if(placement.step() === 'field') {
            place.append(this.fieldSelector());
        } else if(field.noName()) {
            place.append(this.fieldNamer());
            place.find('input').focus();
        } else if(self.hasTypeSetter() && !field.isCsvField() && !field.isAuthorUnchangeableField()) {
            var editor = this.editor();
            place.append(editor.el());
            editor.focus(); // We need to focus when element is appended;
        } else {
            place.append(placewrapper.append(new TextPlacementView({model: field, fontSize: this.fontSize()}).el));
        }

        place.unbind('click');
        if (document.allowsDD()) {
            draggebleField(place, placement, undefined , undefined, false, this.fontSize());
            place.click(function(){
                if (!self.hasTypeSetter()) {
                    self.addTypeSetter();
                    placement.trigger('change:step');
                }
                //else
                //    self.closeTypeSetter();
                return false;
            });
        }
        if (field && signatory.canSign() && !field.isClosed() && field.signatory().current() && self.inlineediting != true && !document.readOnlyView()) {
         place.addClass("to-fill-now");
         if (field.obligatory())
            place.addClass("obligatory");

         place.toggleClass("empty-text-field",field.value() == "");

          place.click(function() {
                return self.startInlineEditing();
          });
        }
        else if (!document.preparation() && (field.value() == undefined || field.value()=="") ) {

          // We hide placements of text fields if user can't interact with them, and they are not filled.
          place.css("display","none");
        }



        if (placement.withTypeSetter()) {
          this.addTypeSetter();
        }
        this.stopListening(undefined,undefined,self.fixWHRel);
        this.listenTo(placement.field(), 'change', self.fixWHRel );
        this.fixWHRel();
        return this;
    }
});

});
