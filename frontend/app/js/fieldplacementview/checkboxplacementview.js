define(['Backbone', 'legacy_code'], function(Backbone) {

window.CheckboxPlacementView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'clear', 'updateSignatoryCSSClass');
        this.model.bind('removed', this.clear);
        this.model.signatory().document().bind('change:signatories',this.updateSignatoryCSSClass);

        this.render();
    },
    clear: function() {
        this.off();
        this.model.unbind('removed', this.clear);
        this.model.signatory().document().unbind('change:signatories',this.updateSignatoryCSSClass);
        $(this.el).remove();
    },
    updateSignatoryCSSClass : function() {
      FieldPlacementGlobal.updateSignatoryCSSClass($(this.el),this.model.signatory());
    },
    render: function() {
            var field =   this.model;
            var box = $(this.el);
            box.addClass('placedcheckbox');
            this.updateSignatoryCSSClass();
            box.toggleClass('checked', field.value() != "");
            box.toggleClass('needs-sender-action', field.needsSenderAction());

            field.bind('change', function() {
                box.toggleClass('checked', field.value() != undefined && field.value() != "");
                box.toggleClass('needs-sender-action', field.needsSenderAction());
            });
    }
});

});
