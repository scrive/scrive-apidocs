

(function(window){


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

window.draggebleField = function(dragHandler, fieldOrPlacement, widthFunction, heightFunction)
{
    var droppedInside = false;
    var helper;
    var field;
    var placement;

    if( fieldOrPlacement.field !=undefined ) {
        placement = fieldOrPlacement;
        field = placement.field();
    }
    else {
        placement = undefined;
        field = fieldOrPlacement;
    }
    var verticaloffset = 0;
    if (field.isText())
       verticaloffset = -2;
    else if (field.isSignature())
       verticaloffset = 1;
    dragHandler.draggable({
        appendTo: ".mainContainer",
        helper: function(event) {
            helper = createFieldPlacementView({model: field, height : heightFunction != undefined ? heightFunction() : undefined, width: widthFunction != undefined ? widthFunction() : undefined}).el;
            return helper;
        },
        start: function(event, ui) {
            if( placement!=undefined ) {
                if (placement.typeSetter != undefined) {
                    placement.typeSetter.clear();
                    placement.typeSetter = undefined;
                }
            }

            if( dragHandler.hasClass("placedfield")) {
                /*
                 * We are draggin an existing placed field. We need to
                 * copy all important attributes, so it looks exactly
                 * the same.
                 */
                $(helper).css({fontSize: dragHandler.css("fontSize")});
                dragHandler.hide();
            }
            if (field.signatory().document().mainfile() != undefined)
                field.signatory().document().mainfile().view.showCoordinateAxes(ui.helper,verticaloffset);
        },
        stop: function() {
            console.log('stop');
            if( placement!=undefined && !droppedInside ) {
                placement.remove();
            }
            else if( dragHandler.hasClass("placedfield")) {
                dragHandler.show();
            }
            if (field.signatory().document().mainfile() != undefined)
                field.signatory().document().mainfile().view.hideCoordinateAxes();
            droppedInside = false;
        },
        drag: function(event, ui) {
            if (field.signatory().document().mainfile() != undefined)
                field.signatory().document().mainfile().view.moveCoordinateAxes(ui.helper, verticaloffset);
        },
        onDrop: function(page, x, y, w, h) {
            console.log('drop');
            droppedInside = true;
            var signatory = field.signatory();
            if( !_.find(signatory.fields(), function(f) { return f==field; })) {
                signatory.addField(field);
            }
            var fontSizeText = $(helper).css("font-size");
            var fontSize = parseFloat(fontSizeText) || 16;

            if( placement!=undefined ) {
                if( placement.page()==page.number() ) {
                    placement.set({ page: page.number(),
                                    xrel: x/w,
                                    yrel: y/h,
                                    wrel: $(helper).width()/w,
                                    hrel: $(helper).height()/h,
                                    fsrel: fontSize/w
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
                        tip: placement.tip()
                    });
                    field.addPlacement(newPlacement);
                }
            }
            else {
                mixpanel.track('Drag field', {fieldname:field.name(),
                                              signatory:field.signatory().signIndex(),
                                              documentid:field.signatory().document().documentid()});
                var newPlacement = new FieldPlacement({
                    page: page.number(),
                    fileid: page.file().fileid(),
                    field: field,
                    xrel : x/w,
                    yrel : y/h,
                    wrel: $(helper).width() / w,
                    hrel: $(helper).height() / h,
                    fsrel: fontSize/w,
                    withTypeSetter : true
                });
                field.addPlacement(newPlacement);
            }
        }
    });
}

var TextTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change:field', this.render);
        var view = this;
        this.fixPlaceFunction = function(){
            view.place();
        }
        $(window).scroll(view.fixPlaceFunction); // To deal with resize;
        $(window).resize(view.fixPlaceFunction);
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
    },
    obligatoryOption : function() {
        var option = $("<div class='checkboxTypeSetter-option checkbox-box'/>");
        var checkbox = $("<div class='checkbox'>");
        var label = $("<label/>").text(localization.designview.textFields.obligatory);
        var field = this.model.field();
        option.append(checkbox).append(label);
        if (field.isObligatory())
            checkbox.addClass("checked");
        checkbox.click(function(){
            if (field.isObligatory()) {
                    checkbox.removeClass("checked");
                    field.makeOptional();
            } else {
                    checkbox.addClass("checked");
                    field.makeObligatory();
            }
        });

        return option;
    },
    help : function() {
        return $("<div class='help'/>").text(localization.designview.textFields.help);
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return Button.init({color:"green",
                            size: "tiny",
                            text: localization.designview.textFields.done,
                            style: "position: relative;  z-index: 107;margin-top: 4px;",
                            onClick : function() {
                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done) {
                                    field.makeReady();
                                    view.clear();
                                } else {
                                    view.nameinput.addClass('redborder');
                                }
                                return false;
                            }
                           }).input();
    },
    title : function() {
        return $("<div class='title'/>").text(localization.designview.textFields.textField);
    },
    subtitle : function() {
        var view = this;
        var box = $("<div class='subtitle'/>");
        var model = view.model;
        var field = model.field();
        var sig = field.signatory();
        var doc = sig.document();
        
        var signame = sig.nameOrEmail() || sig.nameInDocument();
        var fldname = field.nicename();

        var name = signame + ': ' + fldname;

        var options = [];

        _.each(doc.signatories(), function(s) {
            options.push({name: s.nameOrEmail() || sig.nameInDocument(),
                          value: null});
            _.each(s.fields(), function(f) {
                if(f !== field)
                    options.push({name: '   ' + f.nicename(),
                                  value: f,
                                 leftMargin: 16});
            });
        });

        var selector = new Select({
            name: name,
            options: options,
            onSelect: function(f) {
                if(f) {
                    view.model.setField(f);
                }
            }
        });
        
        var text = localization.designview.textFields.forThis + " ";
        box.text(text);
        box.append(selector.input());
        
        return box;
    },
    place : function() {
        var placement = this.model;
        var offset = $(placement.view.el).offset();
        $(this.el).css("left",offset.left + Math.max($(placement.view.el).width()+18));
        $(this.el).css("top",offset.top - 19);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.addClass("checkboxTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='checkboxTypeSetter-body'/>");
           var arrow = $("<div class='checkboxTypeSetter-arrow'/>");

           body.append(this.title());
           body.append(this.subtitle());
           body.append(this.obligatoryOption());

           body.append(this.doneOption());
           body.append(this.help());
        container.html('');
           container.append(arrow);
           container.append(body);

           this.place();
           return this;
    }
});

var TextPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedfieldvalue value');
            box.text(field.nicetext());
            field.bind('change', function() {
                box.text(field.nicetext());

            });

    }
});

var TextPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        this.model.bind('removed', this.clear, this);
        this.model.bind('change:field', this.render);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.view = this;
        this.render();
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
                left: Math.floor(placement.xrel() * parentWidth + 0.5),
                top: Math.floor(placement.yrel() * parentHeight + 0.5),
                fontSize: placement.fsrel() * parentWidth
            });
        }
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    hasTypeSetter : function(){
        return this.model.typeSetter != undefined;
    },
    addTypeSetter : function() {
         var placement = this.model;
         if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
             placement.typeSetter = new TextTypeSetterView({model : placement});
             $('body').append(placement.typeSetter.el);
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
               if ($(window).scrollTop() + $(window).height() > this.input.offset().top && $(window).scrollTop() < this.input.offset().top) {
                  self.input.focus();
               }

          }
          return false;
        }
        view.inlineediting = true;
        var width = place.width() > 100 ? place.width() : 100;
        var parent = place.parent();
        if( parent.length>0 ) { // Check max width so we don't expand inline editing over page width.
          var maxWidth = (1 - placement.xrel()) * parent.width() - 36;
          if (maxWidth < width) width = maxWidth;
          if (width < 30) width = 30;

        }

        place.empty();
        var box = $("<div class='inlineEditing'/>").width(width+24);
        this.input = $("<input type='text'/>").val(field.value()).width(width+5).attr("placeholder",field.nicetext());
        if (field.value() == "" && $.browser.msie) {
          this.input.val(field.nicetext());
          this.input.addClass("grayed");
        }
        var acceptIcon = $("<span class='acceptIcon'/>");
        place.append(box.append(this.input).append(acceptIcon));
        field.bind('change',function() { view.inlineediting  = false; view.render();});
        var accept =  function() {
                      view.inlineediting = false;
                      var val = view.input.val();
                      field.setValue(val);
                      SessionStorage.set(field.signatory().document().documentid(),field.name(),val);
                      field.trigger('change:inlineedited');
                      field.signatory().trigger('change');
                      view.render();
        }
        acceptIcon.click(function() {
                      accept();
                      return false;
        });
        this.input.keydown(function(event) {
                    if(event.which === 13 || event.which === 9)
                    {   accept();
                        return false;
                    }
        });
        this.input.blur(function() {
                  if (view.input.val() != "") {
                      accept();
                      return false;
                  }
        });
        if (this.input.hasClass("grayed") && $.browser.msie) {
                      this.input.val("");
                      this.input.removeClass("grayed");
        }

        if ($(window).scrollTop() + $(window).height() > this.input.offset().top && $(window).scrollTop() < this.input.offset().top && self.input != undefined) {
                   self.input.focus();
        }
        return false;
    },
    render: function() {
        var view = this;
        var placement = this.model;
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);

        place.addClass('placedfield');
        if ((field.signatory() == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation())
              place.css('cursor','pointer');

        this.updatePosition();

        place.empty();
        place.append(new TextPlacementView({model: placement.field()}).el);
        place.unbind('click');
        if (document.allowsDD()) {
            draggebleField(place, placement);
            place.click(function(){
                if (!view.hasTypeSetter())
                    view.addTypeSetter();
                else
                    view.closeTypeSetter();
                return false;
            });
        }
        if (field.signatory().canSign() && !field.isClosed() && field.signatory().current() && view.inlineediting != true && !document.readOnlyView()) {
            place.click(function() {
                return view.startInlineEditing();
            });
        }

        if (placement.withTypeSetter()) {
          this.addTypeSetter();
          placement.cleanTypeSetter();
        }

        return this;
    }
});






var CheckboxPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedcheckbox');
            if (field.value() != "")
                box.addClass("checked");
            else
                box.removeClass("checked");

            field.bind('change', function() {
                if (field.value() != undefined && field.value()  != "")
                    box.addClass("checked");
                else
                    box.removeClass("checked");
            });
    }
});

var CheckboxTypeSetterView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        this.model.bind('removed', this.clear);
        var view = this;
        this.fixPlaceFunction = function(){
            view.place();
        }
        $(window).scroll(view.fixPlaceFunction); // To deal with resize;
        $(window).resize(view.fixPlaceFunction);
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
        $(window).unbind('scroll',this.fixPlaceFunction);
        $(window).unbind('resize',this.fixPlaceFunction);
        this.model.typeSetter = undefined;
    },

    nameEditor: function() {
      var view = this;
      var field = this.model.field();
      this.nameinput = InfoTextInput.init({
                                 infotext: localization.designview.checkboxes.name,
                                 value: field.name(),
                                 cssClass: "name",
                                 onChange : function(value) {
                                     view.nameinput.removeClass('redborder');
                                     field.setName(value);
                                 }
                            }).input();
     return this.nameinput;
    },
    obligatoryOption : function() {

        var option = $("<div class='checkboxTypeSetter-option checkbox-box'/>");
        var checkbox = $("<div class='checkbox'>");
        var label = $("<label/>").text(localization.designview.checkboxes.obligatory);
        var field = this.model.field();
        option.append(checkbox).append(label);
        if (field.isObligatory())
            checkbox.addClass("checked");
        checkbox.click(function(){
            if (field.isObligatory()) {
                    checkbox.removeClass("checked");
                    field.makeOptional();
            } else {
              checkbox.addClass("checked");
              field.makeObligatory();
            }
        });

        return option;
    },
    precheckedOption: function() {
        var option = $("<div class='checkboxTypeSetter-option checkbox-box'/>");
        var checkbox = $("<div class='checkbox'>");
        var label = $("<label/>").text(localization.designview.checkboxes.prechecked);
        var field = this.model.field();
        option.append(checkbox).append(label);
        if (field.value() != undefined && field.value()  != "")
            checkbox.addClass("checked");
        checkbox.click(function(){
            if (field.value() != undefined && field.value()  != "") {
                    checkbox.removeClass("checked");
                    field.setValue("");
            }  else {
                    checkbox.addClass("checked");
                    field.setValue("checked");
            }
            field.trigger("change");
        });
        return option;
    },
    help : function() {
        return $("<div class='help'/>").text(localization.designview.checkboxes.help);
    },
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return Button.init({color:"green",
                            size: "tiny",
                            text: localization.designview.checkboxes.done,
                            style: "position: relative;  z-index: 107;margin-top: 4px;",
                            onClick : function() {

                                var done = field.name() != undefined && field.name() != "";
                                done = done && _.all(field.signatory().fields(), function(f) {
                                    return f.name() != field.name() || f.type() != field.type() || f == field;
                                });
                                if (done){
                                     field.makeReady();
                                     view.clear();
                                    }
                                else
                                    view.nameinput.addClass('redborder');
                                    return false;
                                }
            }).input();
    },
    title : function() {
        return $("<div class='title'/>").text(localization.designview.checkboxes.checkbox);
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
        $(this.el).css("left",offset.left + 32);
        $(this.el).css("top",offset.top - 22);
    },
    render: function() {
           var view = this;
           var container = $(this.el);
           container.addClass("checkboxTypeSetter-container");
           container.css("position", "absolute");
           var body = $("<div class='checkboxTypeSetter-body'/>");
           var arrow = $("<div class='checkboxTypeSetter-arrow'/>");
           container.append(arrow);
           container.append(body);

           body.append(this.title());
           body.append(this.subtitle());
           body.append(this.nameEditor());
           body.append(this.precheckedOption());
           body.append(this.obligatoryOption());

           body.append(this.doneOption());
           body.append(this.help());
           this.place();
           return this;
    }
});


var CheckboxPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear', 'addTypeSetter');
        this.model.bind('removed', this.clear);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.view = this;
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
                left: Math.floor(placement.xrel() * parentWidth + 0.5),
                top: Math.floor(placement.yrel() * parentHeight + 0.5),
                width: Math.ceil(placement.wrel() * parentWidth),
                height: Math.ceil(placement.hrel() * parentHeight),
                fontSize: placement.fsrel() * parentWidth
            });
        }
    },
    clear: function() {
        this.off();
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
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);

        place.addClass('placedfield');
        if ((field.signatory() == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation())
              place.css('cursor','pointer');
        this.updatePosition();

        place.empty();
        var innerPlace = $(new CheckboxPlacementView({model: placement.field(), el: $("<div/>")}).el);
        place.append(innerPlace);

        if (document.allowsDD()) {

            draggebleField(place, placement);
            innerPlace.click(function(){
                if (!view.hasTypeSetter())
                    view.addTypeSetter();
                else
                    view.closeTypeSetter();
                return false;
            });
        }
        if (field.signatory().canSign() && !field.isClosed() &&
            field.signatory().current() && view.inlineediting != true &&
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
          placement.cleanTypeSetter();
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
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var view = this;
            var field = this.model;
            var box = $(this.el);
            var width =  this.width;
            var height = this.height;
            var image = field.value();
            box.empty();
            box.unbind("click");
            box.attr("style","");
            box.addClass('signatureBox').addClass('forDrawing');
            if (image == "")
            {
                console.log("Place for drawing - rendering no value");
                box.removeClass('withImage');
                var bwidth = 253;
                var bheight = 48;
                // Lukas wanted the width and height to be set directly,
                // without a minimum, to be able to fit into small form
                // fields. -- Eric
                //box.width(Math.max(this.signature.width(),bwidth));
                //box.height(Math.max(this.signature.height(),bheight));
                box.width(width);
                box.height(height);

                var textholder = $("<span class='text'/>");

                var button = $("<div class='placesignaturebutton'/>");
                var document = field.signatory().document();

                button.append(textholder.text(localization.signature.placeYour));

                if (width > bwidth) {
                    button.css("margin-left", Math.floor((width - bwidth) / 2) + "px");
                };
                if (height >bheight) {

                    button.css("margin-top", Math.floor((height - bheight) / 2) + "px");
                };
                box.append(button);
            }
            else {
                console.log("Place for drawing - rendering with value");
                box.addClass('withImage');
                var img = $("<img alt=''/>");
                img.css("width",width);
                img.attr("width",width);
                img.css("height",height);
                img.attr("height",height);
                box.css("width",width);
                box.css("height",height);
                img.attr('src',image);
                box.append(img);
            }
            box.click(function() {new SignatureDrawerPopup({field: field, width: width, height: height})});
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
    header : function() {
        var field = this.model;
        var signatory = this.model.signatory();
        var process = signatory.document().process();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  process.processLocalization().signatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
        }
            box.text(localization.signature.placeFor(sname));
        return box;
    },
    render: function() {
            var box = $(this.el);
            box.empty();
            var width =  this.width != undefined ? this.width : 260;
            var height = this.height != undefined ? this.height : 102;
            box.addClass('signatureBox');
            box.append(this.header());
            box.width(width);
            box.height(height);
            return this;
    }
});


var SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        if (this.model.field().signatory().fstnameField() != undefined)
          this.model.field().signatory().fstnameField().bind('change', this.render);
        if (this.model.field().signatory().sndnameField() != undefined)
        this.model.field().signatory().sndnameField().bind('change', this.render);
        this.model.bind('change', this.render);
        this.resizable = args.resizable;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    header : function() {
        var placement = this.model;
        var signatory = this.model.field().signatory();
        var process = signatory.document().process();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  process.processLocalization().signatoryname + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
        }
        if (placement.field().value() == "")
            box.text(localization.signature.placeFor(sname));
        return box;
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
            if (this.resizable) {
                if (box.hasClass("ui-resizable")) box.resizable("destroy");
                box.resizable({stop : function(e, ui) {
                                _.each(placement.field().placements(), function(p) {
                                p.fixWHRel(Math.floor(ui.size.width),Math.floor(ui.size.height));
                               })}});
                $(".ui-resizable-se",box).css("z-index","0");
            }
            return this;
    }
});


var SignaturePlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.view = this;
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
                left: Math.floor(placement.xrel() * parentWidth + 0.5),
                top: Math.floor(placement.yrel() * parentHeight + 0.5),
                fontSize: placement.fsrel() * parentWidth
            });
        }
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
        console.log("Rendering placement");
        var placement = this.model;
        var field = placement.field();
        var signatory = field.signatory();
        var document = signatory.document();
        var place = $(this.el);
        place.addClass('placedfield');
        if ((field.signatory() == document.currentSignatory() && document.currentSignatoryCanSign()) || document.preparation())
              place.css('cursor','pointer');
        this.updatePosition();

        if (document.signingInProcess() && signatory.document().currentSignatoryCanSign() && signatory.current() && !signatory.document().readOnlyView()) {
            place.append(new SignaturePlacementViewForDrawing({
                                                                model: placement.field(),
                                                                width : placement.wrel() * place.parent().width(),
                                                                height : placement.hrel() * place.parent().height()
                                                              }).el);
        }
        else if (document.preparation()) {
            var placementView = $(new SignaturePlacementView({model: placement, resizable : true}).el);
            place.append(placementView);
        }
        else {
            place.append(new SignaturePlacementView({model: placement}).el);
        }
        if (document.allowsDD()) {
            var parentWidth = place.parent().width();
            var parentHeight = place.parent().height();
            draggebleField(place, placement, function() {return placement.wrel() * parentWidth;}, function() {return placement.hrel() * parentHeight;});
        }
        return this;
    }
});

})(window);
