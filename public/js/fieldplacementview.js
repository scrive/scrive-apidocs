

(function(window){


// !!! Not placed views model field, not placement
window.FieldPlacementView = Backbone.View.extend({
    initialize: function (args) {
        if (args.model.isSignature())
            return new SignaturePlacementView(args);
        else return new StandardPlacementView(args);
    }
});

window.FieldPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        if (args.model.field().isSignature())
            return new SignaturePlacementPlacedView(args);
        else return new StandardPlacementPlacedView(args);
    }
});



var StandardPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear)
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove()
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

var StandardPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render' , 'clear');
        this.model.bind('removed', this.clear)
        this.model.view = this;
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove()
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
            place.append(new StandardPlacementView({model: placement.field(), el: $("<div/>")}).el);
            
            if (document.allowsDD())
              place.draggable({
                    appendTo: "body",
                    helper: function(event) {
                        return new StandardPlacementView({model: placement.field(), el: $("<div/>")}).el;
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
                            }))
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
                  iti.keypress(function(event) {
                    if(event.which === 13)
                    {   accept();
                        return false;
                    }   
                  });
                  return false;
            })
            return this;
    }
});

var SignaturePlacementViewForDrawing = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear)
        this.signature = new Signature({field : this.model})
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove()
    },
    render: function() {
            var view = this;
            var signatory = this.model.signatory();
            var box = $(this.el);
            box.css('border', "1px solid black");
            box.width(this.signature.width());
            box.height(this.signature.height());
            box.click(function() {SignatureDrawerPopup.popup({signature: view.signature})})
            return this;
    }
});

var SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear)
        this.signature = new Signature({field : this.model})
        this.render();
    },
    clear: function() {
        this.off();
        $(this.el).remove()
    },
    header : function() {
        var signatory = this.model.signatory();
        var process = signatory.document().process();
        var box = $("<div class='signatureHeader'>");
        var sname = signatory.nameOrEmail();
        if (sname == "")
        {
            if (signatory.isCsv())
             sname =  localization.csv.title
            else
             sname =  process.signatoryname() + (process.numberedsignatories() ? " " + signatory.signIndex() : "");
        }
        box.text(localization.signature.placeFor(sname));
        return box;
    },
    render: function() {
            var view = this;
            var signatory = this.model.signatory();
            var box = $(this.el);
            box.empty();
            box.addClass('signatureBoxNotDrawing');
            box.append(this.header());
            signatory.bind('change', function() {
                    $(".signatureHeader",box).replaceWith(view.header());
            });
            if (!this.signature.hasImage())
            {
                box.width(this.signature.width());
                box.height(this.signature.height());
                this.signature.bind('change', function() {
                    box.width(view.signature.width());
                    box.height(view.signature.height());
                })
            }
            else {
                var img = $("<img alt='signature'  width='250' height='100'/>");
                img.attr('src',this.model.value());
                box.append(img);
            }

            return this;
    }
});

var SignaturePlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear');
        this.model.bind('removed', this.clear)
        this.model.view = this;
        this.signature = new Signature({field : this.model.field()})
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
            if (document.signingInProcess() && signatory.canSign() && signatory.current())
                place.append(new SignaturePlacementViewForDrawing({model: placement.field(), el: $("<div/>")}).el);
            else  if (document.preparation()) {
                    var placementView = $(new SignaturePlacementView({model: placement.field(), el: $("<div/>")}).el);
                    placementView.resizable({resize : function(e, ui) {
                           signature.setSize(ui.size.width,ui.size.height);
                        }});
                    place.append(placementView);
                }    
            else {
                
            }

            if (document.allowsDD())
              place.draggable({
                    appendTo: "body",
                    helper: function(event) {
                        return new SignaturePlacementView({model: placement.field(), el: $("<div/>")}).el;
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
                            }))
                    }
            });
            return this;
    }
});

})(window);
