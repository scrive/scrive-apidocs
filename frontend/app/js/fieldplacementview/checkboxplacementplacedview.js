define(['Backbone', 'legacy_code'], function(Backbone) {

window.CheckboxPlacementPlacedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'closeTypeSetterIfNeeded', 'updatePosition', 'clear', 'hasTypeSetter', 'addTypeSetter', 'closeTypeSetter', 'render');
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
                left: Math.round(placement.xrel() * parentWidth) - FieldPlacementGlobal.placementBorder,
                top: Math.round(placement.yrel() * parentHeight) - FieldPlacementGlobal.placementBorder,
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

});
