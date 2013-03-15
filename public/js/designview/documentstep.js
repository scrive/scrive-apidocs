
(function(window) {

    var DocumentStepModel = Backbone.Model.extend({
        defaults: {

        },
        initalize: function(args) {

        },
        document: function() {
            return this.documentdesignview().document();
        },
        documentdesignview: function() {
            return this.get('documentdesignview');
        }
    });

    var DocumentStepView = Backbone.View.extend({
        initialize: function(args) {
            _.bindAll(this);
            this.render();
        },
        messageBox: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');
            div.addClass('document-step-message');
            if(model.document().mainfile())
                div.text('Click the document to add checkboxes, signature requests, and text fields.');
            else
                div.text('Add a document by clicking one of the options below.');
            return div;
        },
        render: function() {
            $(this.el).html(this.messageBox());
            return this;
        }
        
    });

    window.DocumentStep = function(args) {
        var model = new DocumentStepModel(args);
        var view = new DocumentStepView({model: model});
        this.el = function() {
            return $(view.el);
        };
    };

}(window));
