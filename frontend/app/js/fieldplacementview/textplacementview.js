define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor, Backbone) {

window.TextPlacementView = Backbone.View.extend({
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
    updateSignatoryCSSClass : function() {
      FieldPlacementGlobal.updateSignatoryCSSClass($(this.el), this.model.isFake() ? undefined : this.model.signatory());
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedfieldvalue value');
            box.css("padding", FieldPlacementGlobal.textPlacementSpacingString);
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
            box.css("line-height",this.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px");
        }

        this.updateSignatoryCSSClass();
    }
});

});
