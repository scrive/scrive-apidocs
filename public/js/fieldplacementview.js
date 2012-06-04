

(function(window){


// !!! Not placed views model field, not placement
window.FieldPlacementView = Backbone.View.extend({
    initialize: function (args) {
        if (args.model.isSignature())
            return new SignaturePlacementView(args);
        else if (args.model.isCheckbox())
            return new CheckboxPlacementView(args);
        else return new TextPlacementView(args);
    }
});

window.FieldPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        if (args.model.field().isSignature())
            return new SignaturePlacementPlacedView(args);
        else if (args.model.field().isCheckbox())
            return new CheckboxPlacementPlacedView(args);
        else return new TextPlacementPlacedView(args);
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
        this.model.bind('removed', this.clear);
        this.model.view = this;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var view = this;
            var placement = this.model;
            var field =  placement.field();
            var document = field.signatory().document();
            var place = $(this.el);
            if (this.placed != true)
            {   this.placed = true;
                place.addClass('placedfield').css('position','absolute');
                place.offset({
                    left: placement.x(),
                    top: placement.y()
                });
            }
            place.empty();
            var fileview = field.signatory().document().mainfile().view;
            place.append(new TextPlacementView({model: placement.field(), el: $("<div/>")}).el);

            if (document.allowsDD())
              place.draggable({
                    appendTo: "body",
                    helper: function(event) {
                        return new TextPlacementView({model: placement.field(), el: $("<div/>")}).el;
                    },
                    start: function(event, ui) {
                        place.hide();
                        fileview.showCoordinateAxes(ui.helper);
                    },
                    stop: function() {
                        placement.remove();
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
            if (field.signatory().canSign() && !field.isClosed() && field.signatory().current() && view.inlineediting != true)
            place.click(function() {
                  if (view.inlineediting == true) return false;
                  view.inlineediting = true;
                  console.log('Input was clicked');
                  var width = place.width() > 80 ? place.width() : 80;
                  place.empty();
                  var box = $("<div class='inlineEditing'/>").width(width+24);
                  var iti = $("<input type='text'/>").val(field.value()).width(width+5);
                  var acceptIcon = $("<span class='acceptIcon'/>");
                  place.append(box.append(iti).append(acceptIcon));
                  iti.focus();
                  //clear the value underneath - while they're editing it is undetermined
                  field.setValue("");
                  field.bind('change',function() { view.inlineediting  = false; view.render();});
                  var accept =  function() {
                      view.inlineediting = false;
                      var val = iti.val();
                      field.setValue(val);
                      SessionStorage.set(field.signatory().document().documentid(),field.name(),val);
                      field.trigger('change:inlineedited');
                      view.render();
                  }
                  acceptIcon.click(function() {
                      accept();
                      return false;
                  });
                  iti.keydown(function(event) {
                    if(event.which === 13 || event.which === 9)
                    {   accept();
                        return false;
                    }
                  });
                  return false;
            });
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
        this.render();
    },
    tagname : "div",
    clear: function() {
        this.off();
        $(this.el).remove();
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
    doneOption : function() {
        var view = this;
        var field = this.model.field();
        return Button.init({color:"green",
                            size: "tiny",
                            text: "Done",
                            style: "position: relative;  z-index: 107;",
                            onClick : function() {
                                if (field.name() != undefined && field.name() != ""){
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
        var text = localization.designview.checkboxes.for + " " + name;
        box.text(text);
        return box;
    },
    
    render: function() {
           var view = this;
           var placement = this.model;
           var container = $(this.el);
           container.addClass("checkboxTypeSetter-container");
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
           var offset = $(placement.view.el).offset();
           offset.left = offset.left + 32;
           offset.top = offset.top - 20;
           container.offset(offset);
           return this;
    }
});

var CheckboxPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear', 'addTypeSetter');
        this.model.bind('removed', this.clear);
        this.model.view = this;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    addTypeSetter : function() {
         var placement = this.model;
         if (placement.typeSetter == undefined)
                {
                  placement.typeSetter = new CheckboxTypeSetterView({model : placement});
                  $('body').append(placement.typeSetter.el);
                }
    },
    render: function() {
            var view = this;
            var placement = this.model;
            var field =  placement.field();
            var document = field.signatory().document();
            var place = $(this.el);
            if (this.placed != true)
            {   this.placed = true;
                place.addClass('placedfield').css('position','absolute');
                place.offset({
                    left: placement.x(),
                    top: placement.y()
                });
            }
            place.empty();
            var fileview = field.signatory().document().mainfile().view;
            var innerPlace = $(new CheckboxPlacementView({model: placement.field(), el: $("<div/>")}).el);
            place.append(innerPlace);

            if (document.allowsDD()) {
              innerPlace.draggable({
                    appendTo: "body",
                    helper: function(event) {
                        return new CheckboxPlacementView({model: placement.field(), el: $("<div/>")}).el;
                    },
                    start: function(event, ui) {
                        if (placement.typeSetter != undefined)
                        {   placement.typeSetter.clear();
                            placement.typeSetter = undefined;
                        };
                        place.hide();
                        fileview.showCoordinateAxes(ui.helper);
                    },
                    stop: function() {
                        placement.remove();
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
            if (!field.isReady())
            {
                setTimeout(function(){
                    view.addTypeSetter();
                    } ,50);
            }    
            innerPlace.dblclick(function(){
                view.addTypeSetter();
                return false;
            });  
            }    
            if (field.signatory().canSign() && !field.isClosed() && field.signatory().current() && view.inlineediting != true)
            {  innerPlace.click(function() {
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
    tagname: 'div',
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
                box.width(Math.max(this.signature.width(),bwidth));
                box.height(Math.max(this.signature.height(),bheight));
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
    tagname: 'div',
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
        this.model.view = this;
        this.signature = this.model.field().signature();
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove();
    },
    render: function() {
            var signature = this.signature;
            var placement = this.model;
            var field =  placement.field();
            var signatory =  field.signatory();
            var document = signatory.document();
            var place = $(this.el);
            var fileview = field.signatory().document().mainfile().view;
            place.addClass('placedfield').css('position','absolute');
            place.offset({
                left: placement.x(),
                top: placement.y()
            });
            if (document.signingInProcess() && signatory.document().currentSignatoryCanSign() && signatory.current())
                place.append(new SignaturePlacementViewForDrawing({model: placement.field()}).el);
            else  if (document.preparation()) {
                    var placementView = $(new SignaturePlacementView({model: placement.field(), resizable : true}).el);
                    place.append(placementView);
                }    
            else {
                place.append(new SignaturePlacementView({model: placement.field()}).el);
            }
            if (document.allowsDD())
              place.draggable({
                    appendTo: "body",
                    helper: function(event) {
                        return new SignaturePlacementView({model: placement.field()}).el;
                    },
                    start: function(event, ui) {
                        place.hide();
                    },
                    stop: function() {
                        placement.remove();
                    },
                    drag: function(event, ui) {
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
            return this;
    }
});

})(window);
