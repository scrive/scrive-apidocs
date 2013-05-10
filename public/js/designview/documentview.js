/**

   Design Document View

   Shows upload buttons when there is no document and pages when there is.

**/

(function(window){

    // expected model: document
    // expected viewmodel: DesignViewModel
    var DesignViewDocumentView = Backbone.View.extend({
        className: 'design-view-document-container',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.viewmodel = args.viewmodel;

            view.render();
            view.model.bind('change:file', view.render);
            view.model.bind('change:flux', view.render);
            view.viewmodel.bind('change:showProblems', view.showProblems);
            $(window).resize(function() {
                var myTop = view.$el.offset().top;
                var winHeight = $(window).height();
                view.$el.css('min-height', Math.max(winHeight - myTop, 300));
            });
            view.viewmodel.bind('change:step', function() {
                $(window).resize();
            });
            $(window).resize();
        },
        render: function() {
            var view = this;
            var model = view.viewmodel;
            var document = view.model;
            view.$el.children().detach();
            if(document.flux()) {
                view.$el.html(view.loading());
            } else if(document.mainfile()) {
                view.$el.html(view.renderDocument());
            } else {
                view.$el.html(view.uploadButtons());
            }
            view.showProblems();
            return view;
        },
        showProblems: function() {
            var view = this;
            var model = view.viewmodel;
            var document = view.model;
            
            if(model.showProblems() && !document.mainfile())
                view.$el.addClass('redborder');
            else
                view.$el.removeClass('redborder');
        },
        loading: function() {
            var div = $('<div />');
            div.addClass('design-view-document-loading');
            var inner = $('<div />');
            inner.addClass('design-view-document-loading-inner');
            div.html(inner);
            var spinner = new Spinner({
	        lines  : 10,     // The number of lines to draw
	        length : 19,     // The length of each line
	        width  : 10,     // The line thickness
	        radius : 30,     // The radius of the inner circle
	        color  : '#000', // #rbg or #rrggbb
	        speed  : 1.5,    // Rounds per second
	        trail  : 74,     // Afterglow percentage
	        shadow : false   // Whether to render a shadow
            }).spin(inner.get(0));
            return div;
        },
        renderDocument: function() {
            var view = this;
            var document = view.model;
            var div = $('<div />');
            div.addClass('design-view-document-pages');
            div.append(KontraFile.init({file: document.mainfile()}).view.el);
            return div;
        },
        uploadButtons: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-buttons');

            var inner = $('<div />');
            inner.addClass('design-view-document-buttons-inner');
            div.append(inner);

            inner.append(view.title());
            inner.append(view.buttons());

            return div;
        },
        title: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-buttons-title');

            div.text(localization.designview.chooseDocument);

            return div;
        },
        buttons: function() {
            var view = this;
            var div = $('<div />');

            div.addClass('design-view-document-buttons-buttons');

            div.append(view.uploadFile());

            return div;
        },
        uploadFile : function() {
            var view = this;
            var document = view.model;

            var url = "/api/frontend/changemainfile/" + document.documentid();

            return Button.init({
                color: 'green',
                size: 'big',
                text: localization.designview.openPDF,
                onClick: function() {
                    document.save();
                    FileUpload.upload({
                        action: url,
                        name: 'file',
                        mimetype: 'application/pdf',
                        beforeUpload: function() {
                            document.setFlux();
                        },
                        success: function(d) {
                            document.save();
                            document.afterSave(function() {
                                document.recall();
                            });
                        },
                        error: function(d, a){
                            console.log(d);
                            if(a === 'parsererror') { // file too large
                                new FlashMessage({content: localization.fileTooLarge, color: "red"});
                                document.unsetFlux();
                                mixpanel.track('Error',
                                               {Message: 'main file too large'});
                                
                            } else {
                                new FlashMessage({content: localization.couldNotUpload, color: "red"});
                                document.unsetFlux();
                                mixpanel.track('Error',
                                               {Message: 'could not upload main file'});
                            }
                            document.trigger('change');
                        }
                    });
                }
            }).input();
        },
        afterInsert: function() {
            $(window).resize();
        }

    });


    window.DesignViewDocumentView = DesignViewDocumentView;

}(window));
