define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor) {

/* Borders for all placements */
var placementBorder = 2;

/* Margins for text placements. Such placements have some internal margin and we need to adjust it*/
var textPlacementHorSpace = 7;
var textPlacementExtraLineHeight = 4;
var textPlacementVerSpace = 5;
var textPlacementSpacingString = textPlacementVerSpace + 'px ' + textPlacementHorSpace + 'px ' + textPlacementVerSpace + 'px ' + textPlacementHorSpace + 'px';
var textPlacementXOffset = placementBorder + textPlacementHorSpace;
var textPlacementYOffset = placementBorder + textPlacementVerSpace + textPlacementExtraLineHeight;

/* Font sizes for text placements */
var fontSizeSmall  = 12;
var fontSizeNormal = 16;
var fontSizeLarge  = 20;
var fontSizeHuge   = 24;

/* Margins for checkbox placement. */
var checkboxSprite = 10;
var checkboxSpriteBorder = 1;
var checkboxPlacementMargin = placementBorder + 1;

/* Margins for signature placement. */
var signaturePlacementTopMargin = placementBorder + 1;
var signaturePlacementLeftMargin = placementBorder + 1;

/* Offsets for type setter placements */
var textTypeSetterArrowOffset = 18;
var checkboxTypeSetterHorizontalOffset = 32;
var checkboxTypeSetterVerticalOffset = -22;
var signatureTypeSetterHorizontalOffset = 18;
var signatureTypeSetterVerticalOffset = -19;

// !!! Not placed views model field, not placement
window.createFieldPlacementView = function (args) {
    if (args.model.isSignature())
        return new SignaturePlacementViewWithoutPlacement(args);
    else if (args.model.isCheckbox())
        return new CheckboxPlacementView(args);
    else return new TextPlacementView(args);
};

window.createFieldPlacementPlacedView = function (args) {
    if (args.model.field().isSignature())
        return new SignaturePlacementPlacedView(args);
    else if (args.model.field().isCheckbox())
        return new CheckboxPlacementPlacedView(args);
    else return new TextPlacementPlacedView(args);
};

window.draggebleField = function(dragHandler, fieldOrPlacementFN, widthFunction, heightFunction, cursorNormalize, fontSize, onFieldAdded, onDragStart)
{
    var droppedInside = false;
    var helper;
    var field;
    var placement;
    var verticaloffset = 0;
    var initFP = function() {
      if(typeof fieldOrPlacementFN === 'function') {
              fieldOrPlacement = fieldOrPlacementFN();
      } else {
              fieldOrPlacement = fieldOrPlacementFN;
      }

      if( fieldOrPlacement.field !=undefined ) {
              placement = fieldOrPlacement;
              field = placement.field();
      }
      else {
              placement = undefined;
              field = fieldOrPlacement;
      }
    };
    initFP();
    if(field.isFake())
            verticaloffset = -1;
    else if (field.isText())
            verticaloffset = -1;
    else if (field.isSignature())
            verticaloffset = 1;

    if (onDragStart !== undefined) {
      dragHandler.mousedown(function() {
        if (onDragStart(field)) {
          dragHandler.draggable('enable');
          return true;
        } else {
          dragHandler.draggable('disable');
          return false;
        }
      });
    }

    dragHandler.draggable({
        appendTo : ".design-view-frame",
        scroll: false,
        cursorAt : cursorNormalize ? { top :7 , left :7} : undefined,
        helper: function(event) {
            initFP();
            helper = createFieldPlacementView({
                          model: field,
                          height : heightFunction != undefined ? heightFunction() : undefined,
                          width: widthFunction != undefined ? widthFunction() : undefined,
                          fontSize: fontSize
            }).el;
            return helper;
        },
        start: function(event, ui) {
            if ($('html')[0].scrollWidth <= $(window).width()) {
              // there's no horizontal scrollbar, so dragging away should not create one
              $('html').css('overflow-x', 'hidden');
            }
            initFP();
            if( placement!=undefined ) {
                if (placement.typeSetter != undefined) {
                    placement.typeSetter.clear();
                    placement.typeSetter = undefined;
                }
            }

            if( dragHandler.hasClass("placedfield")) {
                dragHandler.hide();
            }
            if (field.signatory().document().mainfile() != undefined) {
                var xAxisOffset = 0;
                var yAxisOffset = 0;
                if (field.isText() || field.isFake()) {
                  xAxisOffset = textPlacementXOffset;
                  yAxisOffset = textPlacementYOffset;
                }
                field.signatory().document().mainfile().view.showCoordinateAxes(ui.helper,verticaloffset, xAxisOffset, yAxisOffset);
            }
        },
        stop: function() {
            $('html').css('overflow-x', 'auto');
            if( placement!=undefined && !droppedInside ) {
                placement.remove();
                var f = placement.field();
                var s = f.signatory();
                if(f &&
                   f.addedByMe &&
                   f.value() === '' &&
                   f.placements().length <= 1 &&
                   !f.isLastIdentificationField()) {
                    s.deleteField(field);
                    placement.setField(undefined);
                    f.removePlacement(placement);

                }

            }
            else if( dragHandler.hasClass("placedfield")) {
                dragHandler.show();
            }
            if (field.signatory().document().mainfile() != undefined)
                field.signatory().document().mainfile().view.hideCoordinateAxes();
            droppedInside = false;
            initFP();
        },
        drag: function(event, ui) {
            if (field.signatory().document().mainfile() != undefined) {
                var xAxisOffset = 0;
                var yAxisOffset = 0;
                if (field.isText() || field.isFake()) {
                  xAxisOffset = textPlacementXOffset;
                  yAxisOffset = textPlacementYOffset;
                }
                field.signatory().document().mainfile().view.showCoordinateAxes(ui.helper, verticaloffset, xAxisOffset, yAxisOffset);
                field.signatory().document().mainfile().view.moveCoordinateAxes(ui.helper, verticaloffset, xAxisOffset, yAxisOffset);
            }
        },
        onDrop: function(page, x, y, w, h) {
            if (field.isText() || field.isFake() ) {
              x += textPlacementXOffset;
              y += textPlacementYOffset;
            } else if (field.isCheckbox()) {
              x += checkboxPlacementMargin;
              y += checkboxPlacementMargin;
            } else if (field.isSignature()) {
              x += signaturePlacementLeftMargin;
              y += signaturePlacementTopMargin;
            }
            droppedInside = true;
            var signatory = field.signatory();
            if( !_.find(signatory.fields(), function(f) { return f==field; })) {
                if (onFieldAdded != undefined) onFieldAdded(field);
                signatory.addField(field);
            }

            field.setSignatory(signatory);

            var fontSizeText = $(helper).css("font-size");
            var fontSize = parseFloat(fontSizeText) || 16;

            if( placement!=undefined ) {
                if( placement.page()==page.number() ) {
                    placement.set({ xrel: x/w,
                                    yrel: y/h,
                                    wrel: $(helper).width()/w,
                                    hrel: $(helper).height()/h
                                  });
                }
                else {
                    /*
                     * Placement has been moved from page to another
                     * page. For now we just remove and re-add
                     * placement. Refactor this later to in place
                     * update.
                     */
                    mixpanel.track('Drag field to new page', {fieldname:field.name(),
                                                              signatory:field.signatory().signIndex(),
                                                              documentid:field.signatory().document().documentid()});
                    placement.remove();
                    var newPlacement = new FieldPlacement({
                        page: page.number(),
                        fileid: page.file().fileid(),
                        field: field,
                        xrel : x/w,
                        yrel : y/h,
                        wrel: $(helper).width() / w,
                        hrel: $(helper).height() / h,
                        fsrel: fontSize/w,
                        tip: placement.tip(),
                        step : placement.step()
                    });
                    field.addPlacement(newPlacement);
                }
            }
            else {
              _.each(field.signatory().document().signatories(),function(s) {
                _.each(s.fields(), function(f) {
                  _.each(f.placements(), function(p) {
                      if (p.typeSetter != undefined && p.withTypeSetter())
                          p.typeSetter.clear();
                 });
               });
             });
                mixpanel.track('Drag field', {
                    documentid:field.signatory().document().documentid()
                });
                var newPlacement = new FieldPlacement({
                    page: page.number(),
                    fileid: page.file().fileid(),
                    field: field,
                    xrel : x/w,
                    yrel : y/h,
                    wrel: $(helper).width() / w,
                    hrel: $(helper).height() / h,
                    fsrel: fontSize/w,
                    withTypeSetter : true,
                    step : (field.isFake() ? 'signatory' : 'edit')
                });
                field.addPlacement(newPlacement);
                signatory.trigger('drag:checkbox');
            }
        }
    });
};

    /**
       model is field
     **/
    var FieldOptionsView = Backbone.View.extend({
        initialize: function(args) {
            var view = this;
            view.options = args.options;
            view.extraClass = args.extraClass;
            var field = view.model;
            _.bindAll(view);
            view.render();
            if(field) {
                this.listenTo(field,'change:obligatory change:shouldbefilledbysender', view.render);
                this.listenTo(field.signatory(),'change:delivery change:confirmationdelivery change:authentication', view.render);
            }
        },
        render: function() {
            var view = this;
            var field = view.model;
            var selected,selectedName;
            if(!field) {
                selected = 'optional';
                selectedName = localization.designview.optionalField;
            } else if(field.isOptional()) {
                selected = 'optional';
                selectedName = localization.designview.optionalField;
            } else if(field.shouldbefilledbysender()) {
                selected = 'sender';
                selectedName = localization.designview.mandatoryForSender;
            } else {
                selected = 'signatory';
                selectedName = localization.designview.mandatoryForRecipient;
            }

            var options = [];
            if (selected != 'sender')
              options.push({ name : localization.designview.mandatoryForSender,
                             value : 'sender'
                            });
            if (selected != 'signatory' && field.canBeSetByRecipent())
              options.push({ name : localization.designview.mandatoryForRecipient,
                             value : 'signatory'
                            });
            if (selected != 'optional' && field.canBeOptional())
              options.push({ name : localization.designview.optionalField,
                             value : 'optional'
                            });


            var select = new Select({
                options: options,
                inactive: (options.length == 0),
                name: selectedName,
                cssClass : (view.extraClass || ""),
                style: 'font-size: 16px;',
                textWidth: 191,
                optionsWidth: "218px",
                onSelect: function(v) {
                    if(field) {
                        mixpanel.track('Choose obligation', {
                            Subcontext: 'inline'
                        });
                        if(v === 'optional') {
                            if (field.isLastNonOptionalIdentificationField())
                            {
                               new FlashMessage({
                                  color: "red",
                                  content : localization.designview.cantMakeAllIdentificationFieldsOptionalText
                                });
                               return true;
                            }
                            field.makeOptional();
                            field.setShouldBeFilledBySender(false);
                            field.authorObligatory = 'optional';
                        } else if(v === 'signatory') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(false);
                            field.authorObligatory = 'signatory';
                        } else if(v === 'sender') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(true);
                            field.authorObligatory = 'sender';
                        }
                        field.addedByMe = false;
                    }
                    return true;
                }
            });
            $(view.el).empty().append((select.el()));
            return view;
        }
    });

var TextTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        var self = this;
        _.bindAll(this);
        self.listenTo(self.model,'removed',self.clear);
        self.listenTo(self.model,'change:field change:signatory change:step change:fsrel',self.render);

        self.listenTo(self.model.field().signatory(),'change:fields',self.render);
        self.listenTo(self.model.field().signatory().document(),"change:signatories",self.render);
        self.listenTo(self.model,'change:field',function() {
          self.listenTo(self.model.field(),'change:value',self.updatePosition);
        });
        self.listenTo(self.model.field(),'change:value',self.updatePosition);

        _.each(self.model.field().signatory().document().signatories(), function(s) {
          _.each(s.fields(), function(f) {
            _.each(f.placements(), function(p) {
              if(self.model != p && p.typeSetter) {
                p.typeSetter.clear();
              }
            });
          });
        });

        self.fixPlaceFunction = function(){
            self.place();
        };
        $(window).scroll(self.fixPlaceFunction); // To deal with resize;
        $(window).resize(self.fixPlaceFunction);
        self.render();
    },
    updatePosition : function() {
        var self = this;
        setTimeout(function() {self.place();},1);
    },
    clear: function() {
        this.off();
        this.stopListening();

        $(this.el).remove();

        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);

        this.model.typeSetter = undefined;
    },
    obligatoryOption : function() {
        var view = this;
        var field = this.model.field();
        var sig = field?field.signatory():view.model.signatory();

        var optionOptions = ['optional', 'signatory', 'sender'];

        if(sig.author())
            optionOptions = _.without(optionOptions, 'signatory');

        if(name === 'email')
            optionOptions = _.without(optionOptions, 'optional');

        if(name === 'email' && sig.needsEmail())
            optionOptions = ['sender'];

        if(name === 'mobile' && sig.needsMobile())
            optionOptions = ['sender'];

        if(name === 'sigpersnr' && sig.needsPersonalNumber())
            optionOptions = _.without(optionOptions, 'optional');

        return $("<div style='display:block;margin-bottom:5px;'/>").append(
          new FieldOptionsView({
              model: this.model.field(),
              extraClass: 'typesetter-obligatory-option',
              options: optionOptions
          }).el);
    },
    placementOptions : function() {
        var page =  this.model.field().signatory().document().mainfile().page(this.model.get("page"));
        if (page == undefined || page.width() == undefined) return $("<div class='empty'>");

        var placement = this.model;
        var fontSizeName = localization.fontSize.custom;
        var currSize = placement.fsrel() * page.width();
        if (Math.abs(currSize - fontSizeSmall) < 1)
          fontSizeName = localization.fontSize.small;
        if (Math.abs(currSize - fontSizeNormal) < 1)
          fontSizeName = localization.fontSize.normal;
        if (Math.abs(currSize - fontSizeLarge) < 1)
          fontSizeName = localization.fontSize.big;
        if (Math.abs(currSize - fontSizeHuge) < 1)
          fontSizeName = localization.fontSize.large;
        return new Select({name : localization.fontSize.name + ": " + fontSizeName,
                           textWidth: 191,
                           optionsWidth: "218px",
                           style: "font-size: 16px;",
                           options: [
                              { name : localization.fontSize.small,
                                style: "font-size: " + fontSizeSmall + "px",
                                onSelect: function() {placement.setFSRel(fontSizeSmall/page.width()); return true;}},
                              { name : localization.fontSize.normal,
                                style: "font-size: " + fontSizeNormal + "px",
                                onSelect: function() {placement.setFSRel(fontSizeNormal/page.width()); return true;}},
                              { name : localization.fontSize.big,
                                style: "font-size: " + fontSizeLarge + "px",
                                onSelect: function() {placement.setFSRel(fontSizeLarge/page.width()); return true;}},
                              { name : localization.fontSize.large,
                                style: "font-size: " + fontSizeHuge + "px",
                                onSelect: function() {placement.setFSRel(fontSizeHuge/page.width()); return true;}}
                           ] }).el;
    },
    title : function() {
        var view = this;
        var placement = view.model;
        var field = placement.field();
        var fname = field.nicename();
        var signatory = placement.signatory();
        var sname = signatory.nameOrEmail() || signatory.nameInDocument();

        var copy = $("<div class='title'>" + localization.designview.requestFieldFrom + "</div>");
        $('.put-field-name',copy).text(fname);
        $('.put-person-name',copy).text(sname);
        return copy;
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return new Button({color:"black",
                            size: "tiny",
                            text: localization.designview.textFields.done,
                            cssClass : "fieldTypeSetter-button",
                            onClick : function() {
                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done) {
                                    mixpanel.track('Click save inline field');
                                    field.makeReady();
                                    view.clear();
                                    view.model.cleanTypeSetter();
                                    view.model.trigger('change:step');
                                } else {
                                    if (view.nameinput != undefined) view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
                           }).el();
    },
    place : function() {
        var placement = this.model;
        var offset = $(placement.view.el).offset();
        $(this.el).css("left",offset.left + Math.max($(placement.view.el).width() + textTypeSetterArrowOffset));
        $(this.el).css("top",offset.top - 19);
    },
    render: function() {
        var view = this;
        var container = $(this.el);

        var placement = view.model;
        var field = placement.field();
        if(placement.step() === 'edit' && field.name()) {

            container.addClass("fieldTypeSetter-container");
            container.css("position", "absolute");
            var body = $("<div class='fieldTypeSetter-body'/>");
            var arrow = $("<div class='fieldTypeSetter-arrow'/>");


            body.append(this.title());

            if (field.isAuthorUnchangeableField()) {
              var text = "";
              if (field.isEmail())
                text = localization.designview.emailCanBeChangedInAccountSection;
              else if (field.isFstName() || field.isSndName())
                text = localization.designview.nameCanBeChangedInAccountSection;
              body.append($("<div class='microcopy'/>").text(text));
            }

            if (!field.isAuthorUnchangeableField())
              body.append(this.obligatoryOption());
            body.append(this.placementOptions());


            body.append(this.doneOption());
            container.html('');
            container.append(arrow);
            container.append(body);

            this.place();
        }
        return this;
    }
});

var TextPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        var view = this;
        this.fontSize = args.fontSize;
        if(this.model) {
            this.model.bind('removed', this.clear);
        }
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        if(this.model) {
            this.model.unbind('removed', this.clear);
        }

    },
    updateColor : function() {
        var field = this.model;
        var signatory = field.signatory();
        var color = signatory.color();
        if(field.placements().length > 0) {
          color = field.placements()[0].signatory().color();
        }
        $(this.el).css('border', placementBorder + 'px solid ' + color);
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedfieldvalue value');
            box.css("padding", textPlacementSpacingString);
        if(field) {
            box.text(field.nicetext());
            field.bind('change', function() {
                box.text(field.nicetext());
            });
        } else {
            box.text('unset field');
        }
        if (this.fontSize != undefined) {
            box.css("font-size"  ,this.fontSize + "px");
            box.css("line-height",this.fontSize + textPlacementExtraLineHeight + "px");
        }

        this.updateColor();
    }
});

var TextPlacementPlacedView = Backbone.View.extend({
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
                left: Math.floor(placement.xrel() * parentWidth + 1.5) - textPlacementXOffset,
                top: Math.floor(placement.yrel() * parentHeight + 1.5) - textPlacementYOffset,
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
                 "padding:" + textPlacementSpacingString + ";",
          inputStyle : "font-size:" + this.fontSize() + "px ;" +
                       "line-height: " + (this.fontSize()+ textPlacementExtraLineHeight) + "px;" +
                       "height:"+ (this.fontSize() + textPlacementExtraLineHeight) +"px;" +
                       "width: " + width +"px;"+
                       "background:transparent;",
          okStyle :  "font-size:" + this.fontSize() + "px ;" +
                     "line-height: " + (this.fontSize()+ textPlacementExtraLineHeight) + "px;" +
                     "height:"+ (this.fontSize() + textPlacementExtraLineHeight/2) +"px;",
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
            border : placementBorder + "px solid " + (sig.color() || "#f33"),
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
            cssClass: 'text-field-placement-setter-field-field-selector',
            border : placementBorder + "px solid " + (signatory.color() || "#f33"),
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

        function setName() {
            var value = input.value();
            if(value) {

                var samenameexists = _.any(signatory.customFields(), function(c) { return value == c.name();});
                if (samenameexists) {
                  new FlashMessage({color: "red", content : localization.designview.fieldWithSameNameExists});
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
            style : (field && field.signatory() && field.signatory().color()) ? ('border-color : ' + field.signatory().color()) : "",
            suppressSpace: (field.name()=="fstname")
        });

        var button = new Button({
            color: 'black',
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
            cssClass: 'text-field-placement-setter-field-editor',
            infotext: field.nicename(),
            style: "font-size:" + this.fontSize() + "px ;" +
                   "line-height: " + (this.fontSize() + textPlacementExtraLineHeight) +  "px;" +
                   "height:"+ (this.fontSize() + textPlacementExtraLineHeight) +"px; " +
                    ((field && field.signatory() && field.signatory().color()) ? "border-color : "  + field.signatory().color() + ";": "") +
                   "border-width:" + placementBorder + "px; " +
                   "padding:" + textPlacementSpacingString + ";",
            inputStyle : "font-size:" + this.fontSize() + "px ; " +
                         "line-height: " + (this.fontSize() + textPlacementExtraLineHeight) + "px; " +
                         "height:"+ (this.fontSize() + textPlacementExtraLineHeight) +"px; " +
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

var CheckboxPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear', 'updateColor');
        this.model.bind('removed', this.clear);
        this.model.signatory().document().bind('change:signatories',this.updateColor);

        this.render();
    },
    clear: function() {
        this.off();
        this.model.unbind('removed', this.clear);
        this.model.signatory().document().unbind('change:signatories',this.updateColor);
        $(this.el).remove();
    },
    updateColor : function() {
      if(this.model.signatory().color())
                $(this.el).css({'border': placementBorder + 'px solid ' + this.model.signatory().color(),
                         'background-position': '-' + checkboxSpriteBorder + 'px -' + checkboxSpriteBorder + 'px',
                         'width': checkboxSprite,
                         'height': checkboxSprite});
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedcheckbox');
            this.updateColor();
            box.toggleClass('checked', field.value() != "");
            box.toggleClass('needs-sender-action', field.needsSenderAction());

            field.bind('change', function() {
                box.toggleClass('checked', field.value() != undefined && field.value() != "");
                box.toggleClass('needs-sender-action', field.needsSenderAction());
            });
    }
});

var CheckboxTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        var self = this;
        self.model.bind('removed', self.clear);
        self.model.field().bind('change', self.render);
        self.model.field().signatory().bind("change:fields",self.render);
        self.model.field().signatory().document().bind("change:signatories",self.render);

        _.each(self.model.field().signatory().document().signatories(), function(s) {
          _.each(s.fields(), function(f) {
            _.each(f.placements(), function(p) {
              if(self.model != p && p.typeSetter) {
                p.typeSetter.clear();
              }
            });
          });
        });

        var view = self;
        self.fixPlaceFunction = function(){
            view.place();
        };
        $(window).scroll(view.fixPlaceFunction); // To deal with resize;
        $(window).resize(view.fixPlaceFunction);
        self.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        this.model.unbind('removed', this.clear);
        this.model.field().unbind('change', this.render);
        this.model.field().signatory().unbind("change:fields",this.render);
        this.model.field().signatory().document().unbind("change:signatories",this.render);
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
    },
    selector : function() {
        var view = this;
        var box = $("<div class='subtitle'/>");
        var model = view.model;
        var field = model.field();
        var sig = field.signatory();
        var doc = sig.document();

        var signame = sig.nameOrEmail() || sig.nameInDocument();

        var options = [];

        _.each(doc.signatories(), function(s) {
            if(s !== sig)
                options.push({name: s.nameOrEmail() || s.nameInDocument(),
                              value: s});
        });

        var selector = new Select({
            name: signame,
            options: options,
            cssClass: 'signature-field-placement-setter-field-selector',
            textWidth: 191,
            optionsWidth: "218px",
            onSelect: function(s) {
                mixpanel.track('Choose checkbox signatory');
                field.signatory().deleteField(field);
                field.setSignatory(s);
                s.addField(field);
                if (s.author())
                  field.setValue("checked");
                else
                  field.setValue("");
                return true;

            }
        });

        box.text(localization.designview.textFields.forThis + " ");
        box.append($("<div style='margin-top:5px;margin-bottom:10px;'>").append(selector.el()));

        return box;
    },
    obligatoryOption : function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.obligatoryCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.checkboxes.obligatory);
        var field = self.model.field();
        option.append(self.obligatoryCheckbox).append(label);
        if (field.isObligatory()) {
            self.obligatoryCheckbox.addClass("checked");
        }
        self.obligatoryCheckbox.click(function() {
          self.changeObligatoryOption();
        });
        label.click(function() {
          self.changeObligatoryOption();
        });

        return option;
    },
    changeObligatoryOption : function() {
        var field = this.model.field();
        if (field.isObligatory()) {
            mixpanel.track('Choose checkbox obligation', {
                Value: 'optional'
            });
            field.makeOptional();
        } else {
            mixpanel.track('Choose checkbox obligation', {
                Value: 'obligatory'
            });
            field.makeObligatory();
        }
        this.obligatoryCheckbox.toggleClass("checked", field.isObligatory());
    },
    precheckedOption: function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.precheckedCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.checkboxes.prechecked);
        var field = self.model.field();
        option.append(self.precheckedCheckbox).append(label);
        if (field.value() != undefined && field.value()  != "") {
            self.precheckedCheckbox.addClass("checked");
        }
        self.precheckedCheckbox.click(function() {
          self.changePrecheckedOption();
        });
        label.click(function() {
          self.changePrecheckedOption();
        });

        return option;
    },
    changePrecheckedOption : function() {
        var field = this.model.field();
        var checkbox = this.precheckedCheckbox;
        if (field.value() != undefined && field.value()  != "") {
            mixpanel.track('Choose prechecked', {
                Value: 'unchecked'
            });
            checkbox.removeClass("checked");
            field.setValue("");
        } else {
            mixpanel.track('Choose prechecked', {
                Value: 'prechecked'
            });
            checkbox.addClass("checked");
            field.setValue("checked");
        }
        field.trigger("change");
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return new Button({color:"black",
                            size: "tiny",
                            text: localization.designview.checkboxes.done,
                            cssClass : "fieldTypeSetter-button",
                            onClick : function() {

                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done){
                                     field.makeReady();
                                     view.clear();
                                } else {
                                    if (view.nameinput != undefined)  view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
            }).el();
    },
    title: function() {
        return $("<div class='title'/>").text(localization.designview.checkboxes.checkbox).attr("title",this.model.field().name());
    },
    subtitle : function() {
        var box = $("<div class='subtitle'/>");
        var name = this.model.field().signatory().nameInDocument();
        if (this.model.field().signatory().nameOrEmail() != "")
            name = this.model.field().signatory().nameOrEmail();
        var text = localization.designview.checkboxes.forThis + " " + name;
        box.text(text);
        return box;
    },
    place : function() {
        var placement = this.model;
        var offset = $(placement.view.el).offset();
        $(this.el).css("left",offset.left + checkboxTypeSetterHorizontalOffset);
        $(this.el).css("top",offset.top + checkboxTypeSetterVerticalOffset);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.empty();
           container.addClass("fieldTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='fieldTypeSetter-body'/>");
           var arrow = $("<div class='fieldTypeSetter-arrow'/>");
           container.append(arrow);
           container.append(body);

           body.append(this.title());
           body.append(this.selector());
           body.append(this.precheckedOption());
           body.append(this.obligatoryOption());
           body.append(this.doneOption());
           this.place();
           return this;
    }
});


var CheckboxPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this);
        this.model.bind('removed', this.clear);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.field().bind('change', this.render);
        this.model.view = this;

        var view = this;
        this.model.bind('change:withTypeSetter', this.closeTypeSetterIfNeeded);
        this.render();
    },
    closeTypeSetterIfNeeded : function() {
       if(!this.model.withTypeSetter())
                this.closeTypeSetter();
    },
    updatePosition: function() {
        var placement = this.model;
        var place = $(this.el);
        var parent = place.parent();
        if( parent.length>0 ) {
            var parentWidth = parent.width();
            var parentHeight = parent.height();
            place.css({
                left: Math.round(placement.xrel() * parentWidth) - placementBorder,
                top: Math.round(placement.yrel() * parentHeight) - placementBorder,
                width: Math.round(placement.wrel() * parentWidth),
                height: Math.round(placement.hrel() * parentHeight),
                fontSize: placement.fsrel() * parentWidth
            });
        }
    },
    clear: function() {
        this.off();
        this.model.unbind('removed', this.clear);
        this.model.unbind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.field().unbind('change', this.render);
        this.model.unbind('change:withTypeSetter', this.closeTypeSetterIfNeeded);


        $(this.el).remove();
    },
    hasTypeSetter : function(){
        return this.model.typeSetter != undefined;
    },
    addTypeSetter : function() {
         var placement = this.model;
         if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
             placement.typeSetter = new CheckboxTypeSetterView({model : placement});
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
    render: function() {
        var self = this;
        var placement = this.model;
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);

        place.addClass('placedfield');
        if ((field.signatory() == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation()) {
              place.css("cursor", "pointer");
              place.addClass("to-fill-now");
              if (field.obligatory())
                place.addClass("obligatory");
        }
        this.updatePosition();

        place.empty();
        var innerPlace = $(new CheckboxPlacementView({model: placement.field(), el: $("<div/>")}).el);
        place.append(innerPlace);

        if (document.allowsDD()) {

            draggebleField(place, placement);
            innerPlace.click(function(){
                if (!self.hasTypeSetter())
                    self.addTypeSetter();
                else
                    self.closeTypeSetter();
                return false;
            });
        }
        if (field.signatory().canSign() && !field.isClosed() &&
            field.signatory().current() && self.inlineediting != true &&
            !document.readOnlyView()) {
            innerPlace.click(function() {
                if (field.value() == "")
                    field.setValue("CHECKED");
                else
                    field.setValue("");
                return false;
            });
        }
        if (placement.withTypeSetter()) {
          this.addTypeSetter();
        }
        return this;
    }

});










/* This thing can work with either field as a model */

window.SignaturePlacementViewForDrawing = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change', this.render);
        this.height = args.height;
        this.width = args.width;
        this.arrow = args.arrow;
        this.signview = args.signview;

        this.render();
    },
    clear: function() {
        this.off();
        this.model.unbind('removed', this.clear);
        this.model.unbind('change', this.render);
        $(this.el).remove();
    },
    updateSize: function(width, height) {
      this.width = width;
      this.height = height;
      var box = $(this.el);
      box.width(width);
      box.height(height);
      box.css("line-height",Math.floor(height) + "px");
    },
    render: function() {
            var self = this;
            var field = this.model;
            var box = $(this.el);
            var width =  this.width;
            var height = this.height;
            var arrow = this.arrow;
            var signview = this.signview;
            var image = field.value();
            box.empty();
            box.unbind("click");
            box.attr("style","");
            box.addClass('signatureBox').addClass('forDrawing');
            if (image == "")
            {
                box.removeClass('withImage').addClass('signaturePlaceholder');
                if (field.obligatory())
                  box.addClass("obligatory");
                else
                  box.addClass("optional");

                box.width(width);
                box.height(height);
                box.css("line-height",Math.floor(height) + "px");
                box.text(localization.signature.clickToDraw);
            }
            else {
                console.log("Place for drawing - rendering with value");
                box.removeClass("signaturePlaceholder").addClass('withImage');
                var img = $("<img alt=''/>");
                box.css("width",width);
                box.css("height",height);
                if(BrowserInfo.isIE7orLower() && image.substring(0, 'data:'.length) == 'data:') {
                  // IE 7 cannot handle data uris
                  img.attr('src', '/img/img-signature-saved-' + localization.code + '.png');
                } else {

                  img.css("width",width);
                  img.attr("width",width);
                  img.css("height",height);
                  img.attr("height",height);
                  img.attr('src', image);
                }
                box.append(img);
            }

            box.click(function() {
              new SignatureDrawOrTypeModal({
                field: field,
                width: self.width,
                height: self.height,
                arrow: arrow,
                signview: signview
              });
            });
            return this;
    }
});


var SignaturePlacementViewWithoutPlacement = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.width = args.width;
        this.height = args.height;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    ddIcon : function() {
        return $("<div class='signatureDDIcon'/>");
    },
    header : function() {
        var field = this.model;
        var signatory = this.model.signatory();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  localization.process.signatoryname + " " + signatory.signIndex();
        }
        var text = $("<span>" + localization.signature.placeFor + "</span>");
        text.find(".put-name-here").text(sname);
        box.append(text);
        return box;
    },
    render: function() {
            var box = $(this.el);
            box.empty();
            var width =  this.width != undefined ? this.width : 260;
            var height = this.height != undefined ? this.height : 102;
            box.addClass('signatureBox');
            box.css('border', placementBorder + 'px solid ' + (this.model.value() == "" ? (this.model.signatory().color() || '#999') : "transparent" ));
            box.append(this.ddIcon());
            box.append(this.header());
            box.width(width);
            box.height(height);
            return this;
    }
});

var SignatureTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this);
        var self = this;
        self.model.bind('removed', self.clear);
        self.model.field().bind('change:signatory', self.render);
        self.model.field().signatory().bind("change:fields",self.render);
        self.model.field().signatory().document().bind("change:signatories",self.render);

        _.each(self.model.field().signatory().document().signatories(), function(s) {
          _.each(s.fields(), function(f) {
            _.each(f.placements(), function(p) {
              if(self.model != p && p.typeSetter) {
                p.typeSetter.clear();
              }
            });
          });
        });

        var view = self;
        self.fixPlaceFunction = function(){
            view.place();
        };
        $(window).scroll(view.fixPlaceFunction); // To deal with resize;
        $(window).resize(view.fixPlaceFunction);
        self.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        this.model.unbind('removed', this.clear);
        this.model.field().unbind('change:signatory', this.render);
        this.model.field().signatory().unbind("change:fields",this.render);
        this.model.field().signatory().document().unbind("change:signatories",this.render);
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
    },
    obligatoryOption : function() {
        var self = this;
        var option = $("<div class='fieldTypeSetter-option checkbox-box'/>");
        self.obligatoryCheckbox = $("<div class='checkbox'><div class='checkmark'/></div>");
        var label = $("<label/>").text(localization.designview.textFields.obligatory);
        var field = self.model.field();
        option.append(self.obligatoryCheckbox).append(label);
        if (field.isObligatory()) {
            self.obligatoryCheckbox.addClass("checked");
        }
        self.obligatoryCheckbox.click(function() {
          self.changeObligatoryOption();
        });
        label.click(function() {
          self.changeObligatoryOption();
        });

        return option;
    },
    changeObligatoryOption : function() {
        var field = this.model.field();
        var checkbox = this.obligatoryCheckbox;
        if (field.isObligatory()) {
            field.makeOptional();
        } else {
            field.makeObligatory();
        }
        checkbox.toggleClass("checked", field.isObligatory());
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return new Button({color:"black",
                            size: "tiny",
                            text: localization.designview.textFields.done,
                            cssClass : "fieldTypeSetter-button",
                            onClick : function() {
                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done) {
                                    field.makeReady();
                                    view.clear();
                                } else {
                                    if (view.nameinput != undefined) view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
                           }).el();
    },
    title : function() {
        return $("<div class='title'/>").text(localization.designview.signatureBoxSettings);
    },
    selector : function() {
        var view = this;
        var box = $("<div class='subtitle'/>");
        var model = view.model;
        var field = model.field();
        var sig = field.signatory();
        var doc = sig.document();

        var signame = sig.nameOrEmail() || sig.nameInDocument();

        var options = [];

        _.each(doc.signatories(), function(s) {
            if(s !== sig)
                options.push({name: s.nameOrEmail() || s.nameInDocument(),
                              value: s});
        });

        var selector = new Select({
            name: signame,
            options: options,
            textWidth: 191,
            optionsWidth: "218px",
            cssClass: 'signature-field-placement-setter-field-selector',
            onSelect: function(s) {
                mixpanel.track('Choose signature signatory');
                field.signatory().deleteField(field);
                field.setSignatory(s);
                s.addField(field);
                return true;
            }
        });

        box.text(localization.designview.textFields.forThis + " ");
        box.append($("<div class='fieldTypeSetter-subtitle-select'>").append(selector.el()));

        return box;
    },
    place : function() {
        var placement = this.model;
        var el = $(placement.view.el);
        var offset = el.offset();
        $(this.el).css("left", offset.left + el.width() + signatureTypeSetterHorizontalOffset);
        $(this.el).css("top", offset.top + signatureTypeSetterVerticalOffset);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.addClass("fieldTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='fieldTypeSetter-body'/>");
           var arrow = $("<div class='fieldTypeSetter-arrow'/>");

           body.append(this.title());
           body.append(this.selector());
           body.append(this.obligatoryOption());

           body.append(this.doneOption());
        container.html('');
           container.append(arrow);
           container.append(body);

           this.place();
           return this;
    }
});

var SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear', 'updateColor');
        this.model.bind('removed', this.clear);
        if (this.model.field().signatory().fstnameField() != undefined)
          this.model.field().signatory().fstnameField().bind('change', this.render);
        if (this.model.field().signatory().sndnameField() != undefined)
            this.model.field().signatory().sndnameField().bind('change', this.render);
        this.model.bind('change', this.render);
        this.model.field().bind('change:signatory', this.render);
        this.model.field().signatory().document().bind('change:signatories', this.updateColor);
        this.resizable = args.resizable;
        this.render();
    },
    clear: function() {
        this.off();
        this.model.field().unbind('change:signatory', this.render);
        this.model.unbind('change', this.render);
        this.model.unbind('removed', this.clear);
        this.model.field().signatory().document().unbind('change:signatories', this.updateColor);

        if (this.model.field().signatory().fstnameField() != undefined)
          this.model.field().signatory().fstnameField().unbind('change', this.render);
        if (this.model.field().signatory().sndnameField() != undefined)
            this.model.field().signatory().sndnameField().unbind('change', this.render);
        $(this.el).remove();
    },
    ddIcon : function() {
        return $("<div class='signatureDDIcon'/>");
    },
    header : function() {
        var placement = this.model;
        var signatory = this.model.field().signatory();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  localization.process.signatoryname + " " + signatory.signIndex();
        }

        if (placement.field().value() == "") {
            var text = $("<span>" + localization.signature.placeFor + "</span>");
            text.find(".put-name-here").text(sname);
            box.append(text);
        }

        return box;
    },
    updateColor : function() {
        $(this.el).css('border', placementBorder + 'px solid ' + (this.model.field().value() == "" ? (this.model.field().signatory().color() || '#999') : "transparent" ));
    },
    updateSize: function(width, height) {
      var box = $(this.el);
      box.width(width);
      box.height(height);
    },
    render: function() {
            var placement = this.model;
            var view = this;
            var signatory = this.model.field().signatory();
            var box = $(this.el);
            box.empty();
            var width =  placement.wrel() * box.parent().parent().width();
            var height = placement.hrel() * box.parent().parent().height();
            if (placement.field().value() == "")
            {
                box.addClass('signatureBox');
                box.append(this.ddIcon());
                box.append(this.header());
                signatory.bind('change', function() {
                    $(".signatureHeader",box).replaceWith(view.header());
                });
                box.width(width);
                box.height(height);
            }
            else {
                box.removeClass('signatureBox');
                var img = $("<img alt=''/>");
                box.css("width",width);
                box.css("height",height);
                img.attr('src',placement.field().value());
                img.css("width",width);
                img.attr("width",width);
                img.css("height",height);
                img.attr("height",height);
                box.append(img);
            }
        this.updateColor();
            if (this.resizable) {
                if (box.hasClass("ui-resizable")) box.resizable("destroy");
                box.resizable({
                    stop: function(e, ui) {
                        _.each(placement.field().placements(), function(p) {
                            p.fixWHRel(Math.floor(box.width()),Math.floor(box.height()));
                            if(p.typeSetter)
                                p.typeSetter.place();
                        });
                    },
                    resize: function(e, ui) {

                        if (ui.size.width < 44) {
                          ui.size.width = 44;
                          box.width(44);
                        }
                        if (ui.size.height < 22) {
                          ui.size.height = 22;
                          box.height(22);
                        }

                        if (ui.size.height >  ui.size.width) {
                            box.height(ui.size.width);
                        } else if (3 * ui.size.height <  ui.size.width ) {
                            box.height(ui.size.width/3);
                        }

                        if(placement.typeSetter)
                            placement.typeSetter.place();
                    }

                });
                $(".ui-resizable-se",box).css("z-index","0");
            }
            return this;
    }
});

var SignaturePlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this);
        this.model.bind('removed', this.clear);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.bind('change:withTypeSetter', this.closeTypeSetter);
        this.model.view = this;
        this.signview = args.signview;
        this.arrow = args.arrow;
        this.render();
    },
    updatePosition: function() {
        var placement = this.model;
        var place = $(this.el);
        var parent = place.parent();
        if( parent.length>0 ) {
            var parentWidth = parent.width();
            var parentHeight = parent.height();
            place.css({
                left: Math.round(placement.xrel() * parentWidth),
                top: Math.round(placement.yrel() * parentHeight),
                fontSize: placement.fsrel() * parentWidth
            });
            if (this.signaturePlacement !== undefined) {
              this.signaturePlacement.updateSize(placement.wrel() * parentWidth, placement.hrel() * parentHeight);
            }
        }
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        this.model.unbind('removed', this.clear);
        this.model.unbind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.unbind('change:withTypeSetter', this.closeTypeSetter);
    },
    hasTypeSetter : function(){
        return this.model.typeSetter != undefined;
    },
    addTypeSetter : function() {
         var placement = this.model;
         if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
             placement.typeSetter = new SignatureTypeSetterView({model : placement});
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
    render: function() {
        var view = this;
        var placement = this.model;
        var field = placement.field();
        var signatory = field.signatory();
        var document = signatory.document();
        var place = $(this.el);

        place.empty();

        place.addClass('placedfield');
        place.toggleClass("empty-signature",field.value() == "");

        if ((field.signatory() == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation()) {
              place.css('cursor','pointer');
              place.css('z-index','1');
        }
        this.updatePosition();

        if (document.signingInProcess() && signatory.document().currentSignatoryCanSign() && signatory.current() && !signatory.document().readOnlyView()) {
            this.signaturePlacement = new SignaturePlacementViewForDrawing({
                                                                model: placement.field(),
                                                                width : placement.wrel() * place.parent().width(),
                                                                height : placement.hrel() * place.parent().height(),
                                                                signview: this.signview,
                                                                arrow: this.arrow
                                                              });
            place.append(this.signaturePlacement.el);
        }
        else if (document.preparation()) {
            this.signaturePlacement = new SignaturePlacementView({model: placement, resizable : true});
            place.append(this.signaturePlacement.el);
        }
        else {
            place.append(new SignaturePlacementView({model: placement}).el);

            // We hide placements of signatures if user can't interact with them, and they are not filled.
            if (field.value() == undefined || field.value()=="")
              place.css("display","none");
        }

        if (document.allowsDD()) {
            var parentWidth = place.parent().width();
            var parentHeight = place.parent().height();
            draggebleField(place, placement, function() {return placement.wrel() * parentWidth;}, function() {return placement.hrel() * parentHeight;});

            place.click(function(){
                if (!view.hasTypeSetter())
                    view.addTypeSetter();
                else
                    view.closeTypeSetter();
                return false;
            });
        }
        if (placement.withTypeSetter()) {
          this.addTypeSetter();
        }


        return this;
    }
});

});
