

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
        _.bindAll(this, 'render');
        this.render();
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
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
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
        _.bindAll(this, 'render');
        this.render();
    },
    render: function() {
            var view = this;
            var signatory = this.model.signatory();
            var box = $(this.el);
            box.append($(SignatureDrawer.init({signaturefield : this.model}).view.el));
            return this;
    }
});

var SignaturePlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
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
            if (this.model.value() == undefined || this.model.value() == "")
            {
                var img = $("<div class='signatureDummy'>")
                box.append(img);

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
        _.bindAll(this, 'render');
        this.model.view = this;
        this.render();
    },
    render: function() {
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
            else    
                place.append(new SignaturePlacementView({model: placement.field(), el: $("<div/>")}).el);

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
