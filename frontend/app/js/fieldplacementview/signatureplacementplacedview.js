define(['Backbone', 'legacy_code'], function(Backbone) {

window.SignaturePlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'updatePosition', 'clear', 'hasTypeSetter', 'addTypeSetter', 'closeTypeSetter', 'render');
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
