

(function(window){


// !!! Not placed views model field, not placement
window.createFieldPlacementView = function (args) {
    if (args.model.isSignature())
        return new SignaturePlacementView(args);
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

window.draggebleField = function(dragHandler, fieldOrPlacement)
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
    var fileview = field.signatory().document().mainfile().view;

    dragHandler.draggable({
        appendTo: "body",
        helper: function(event) {
            if( helper==undefined ) {
                helper = createFieldPlacementView({model: field}).el;
            }
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
            fileview.showCoordinateAxes(ui.helper);
        },
        stop: function() {
            if( placement!=undefined && !droppedInside ) {
                placement.remove();
            }
            else if( dragHandler.hasClass("placedfield")) {
                dragHandler.show();
            }
            fileview.hideCoordinateAxes();
            droppedInside = false;
        },
        drag: function(event, ui) {
            fileview.moveCoordinateAxes(ui.helper);
        },
        onDrop: function(page, x, y, w, h) {
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
                var newPlacement = new FieldPlacement({
                    page: page.number(),
                    fileid: page.file().fileid(),
                    field: field,
                    xrel : x/w,
                    yrel : y/h,
                    wrel: $(helper).width() / w,
                    hrel: $(helper).height() / h,
                    fsrel: fontSize/w
                });
                field.addPlacement(newPlacement);
            }
        }
    });
}

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
    startinlineediting : function() {
        var placement = this.model;
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);
        var view = this;
        if (view.inlineediting == true) {
          if (this.input != undefined) this.input.focus();
          return false;
        }
        view.inlineediting = true;
        var width = place.width() > 80 ? place.width() : 80;
        place.empty();
        var box = $("<div class='inlineEditing'/>").width(width+24);
        this.input = $("<input type='text' autofocus='autofocus'/>").val(field.value()).width(width+5);
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
        this.input.focus();
        return false;
    },
    render: function() {
        var view = this;
        var placement = this.model;
        var field =  placement.field();
        var document = field.signatory().document();
        var place = $(this.el);

        place.addClass('placedfield');

        this.updatePosition();

        place.empty();
        place.append(new TextPlacementView({model: placement.field()}).el);

        if (document.allowsDD()) {
            draggebleField(place, placement);
        }
        if (field.signatory().canSign() && !field.isClosed() && field.signatory().current() && view.inlineediting != true) {
            place.click(function() {
                return view.startinlineediting();
            });
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
    tagname : "div",
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
        var option = $("<div class='checkboxTypeSetter-option'/>");
        var checkbox = $("<input type='checkbox'>");
        var label = $("<span/>").text(localization.designview.checkboxes.obligatory);
        var field = this.model.field();
        option.append(checkbox).append(label);
        if (field.isObligatoryCheckbox())
            checkbox.attr("checked","Yes");
        checkbox.click(function(){
            if (field.isObligatoryCheckbox())
                    field.makeCheckboxOptional();
                else
                    field.makeCheckboxObligatory();
        });

        return option;
    },
    precheckedOption: function() {
        var option = $("<div class='checkboxTypeSetter-option'/>");
        var checkbox = $("<input type='checkbox'>");
        var label = $("<span/>").text(localization.designview.checkboxes.prechecked);
        var field = this.model.field();
        option.append(checkbox).append(label);
        if (field.value() != undefined && field.value()  != "")
            checkbox.attr("checked","Yes");
        checkbox.click(function(){
            if (field.value() != undefined && field.value()  != "")
                    field.setValue("");
                else
                    field.setValue("checked");
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
                            style: "position: relative;  z-index: 107;",
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
        var name = this.model.field().signatory().view.name();
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
        $(this.el).css("top",offset.top - 20);
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
        this.updatePosition();

        place.empty();
        var innerPlace = $(new CheckboxPlacementView({model: placement.field(), el: $("<div/>")}).el);
        place.append(innerPlace);

        if (document.allowsDD()) {

            draggebleField(place, placement);


            if (!field.isReady())
            {
                setTimeout(function(){
                    view.addTypeSetter();
                } ,50);
            }
            innerPlace.dblclick(function(){
                if (!view.hasTypeSetter())
                    view.addTypeSetter();
                else
                    view.closeTypeSetter();
                return false;
            });
        }
        if (field.signatory().canSign() && !field.isClosed() &&
            field.signatory().current() && view.inlineediting != true ) {
            innerPlace.click(function() {
                if (field.value() == "")
                    field.setValue("CHECKED");
                else
                    field.setValue("");
                return false;
            });
        }
        return this;
    }

});












var SignaturePlacementViewForDrawing = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change', this.render);
        this.signature = this.model.signature();
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var view = this;
            var signatory = this.model.signatory();
            var box = $(this.el);
            box.empty();
            box.attr("style","");
            box.addClass('signatureBox');
            if (!this.signature.hasImage())
            {
                box.css("border-width","0px");
                var bwidth = 253;
                var bheight = 48;
                // Lukas wanted the width and height to be set directly,
                // without a minimum, to be able to fit into small form
                // fields. -- Eric
                //box.width(Math.max(this.signature.width(),bwidth));
                //box.height(Math.max(this.signature.height(),bheight));
                box.width(this.signature.width());
                box.height(this.signature.height());
                var button = $("<div/>");
                if (!signatory.canPadSignQuickSign()){
                    button.addClass('placesignaturebutton');
                    button.append($("<span class='text'/>").text(localization.signature.placeYour));
                }
                else {
                    button.addClass('placesignatureandsignbutton');
                    button.append($("<span class='text'/>").text(signatory.document().process().signbuttontext()));
                }
                if (this.signature.width() > bwidth) {
                    button.css("margin-left", Math.floor((this.signature.width() - bwidth) / 2) + "px");
                };
                if (this.signature.height() >bheight) {

                    button.css("margin-top", Math.floor((this.signature.height() - bheight) / 2) + "px");
                };
                box.append(button);
            }
            else {
                box.css("border-width","0px");
                var img = $("<img alt=''/>");
                img.css("width",view.signature.width());
                img.attr("width",view.signature.width());
                img.css("height",view.signature.height());
                img.attr("height",view.signature.height());
                box.css("width",view.signature.width());
                box.css("height",view.signature.height());
                img.attr('src',this.signature.image());
                box.append(img);
            }
            box.click(function() {SignatureDrawerPopup.popup({signature: view.signature})});
            return this;
    }
});


var SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change', this.render);
        this.signature = this.model.signature();
        this.resizable = args.resizable;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    header : function() {
        var signatory = this.model.signatory();
        var process = signatory.document().process();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title;
            else
             sname =  process.signatoryname() + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
        }
        if (!this.signature.hasImage())
            box.text(localization.signature.placeFor(sname));
        return box;
    },
    render: function() {
            var view = this;
            var signatory = this.model.signatory();
            var box = $(this.el);
            box.empty();
            if (!this.signature.hasImage())
            {
                box.addClass('signatureBox');
                box.append(this.header());
                signatory.bind('change', function() {
                    $(".signatureHeader",box).replaceWith(view.header());
                });
                box.width(this.signature.width());
                box.height(this.signature.height());
                this.signature.bind('change', function() {
                    box.width(view.signature.width());
                    box.height(view.signature.height());
                });
            }
            else {
                box.removeClass('signatureBox');
                var img = $("<img alt=''/>");
                box.css("width",view.signature.width());
                box.css("height",view.signature.height());
                img.attr('src',this.signature.image());
                img.css("width",view.signature.width());
                img.attr("width",view.signature.width());
                img.css("height",view.signature.height());
                img.attr("height",view.signature.height());
                box.append(img);
            }
            box.resizable("destroy");
            if (this.resizable)
                box.resizable({resize : function(e, ui) {
                           view.signature.setSize(ui.size.width,ui.size.height);
                        }});
            return this;
    }
});


var SignaturePlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear);
        this.model.bind('change:xrel change:yrel change:wrel change:hrel change:fsrel', this.updatePosition, this);
        this.model.view = this;
        this.signature = this.model.field().signature();
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
    render: function() {
        var signature = this.signature;
        var placement = this.model;
        var field = placement.field();
        var signatory = field.signatory();
        var document = signatory.document();
        var place = $(this.el);
        var fileview = field.signatory().document().mainfile().view;
        place.addClass('placedfield');
        this.updatePosition();

        if (document.signingInProcess() && signatory.document().currentSignatoryCanSign() && signatory.current()) {
            place.append(new SignaturePlacementViewForDrawing({model: placement.field()}).el);
        }
        else if (document.preparation()) {
            var placementView = $(new SignaturePlacementView({model: placement.field(), resizable : true}).el);
            place.append(placementView);
        }
        else {
            place.append(new SignaturePlacementView({model: placement.field()}).el);
        }
        if (document.allowsDD()) {
            draggebleField(place, placement);
        }
        return this;
    }
});

})(window);
