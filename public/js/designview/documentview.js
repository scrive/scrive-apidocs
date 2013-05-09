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
            div.append(view.removeDocumentButtons());
            div.append(KontraFile.init({file: document.mainfile()}).view.el);
            return div;
        },
        removeDocumentButtons: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-remove-button-container');
            div.append(view.removeDocumentButton());
            return div;
        },
        removeDocumentButton: function() {
            var view = this;
            var doc = view.model;
            var div = $('<div />');
            div.addClass('design-view-document-remove-button-wrapper');
            div.append(view.removeDocumentButtonLabel());
            div.append(view.removeDocumentButtonIcon());
            div.click(function() {
                doc.setFlux();
                doc.removePlacements();
                doc.save();
                doc.afterSave( function() {
                    new Submit({
                        method : "POST",
                        url :  "/api/frontend/changemainfile/" + doc.documentid(),
                        ajax: true,
                        onSend: function() {
                            
                        },
                        ajaxerror: function(d,a){
                            doc.recall();
                        },
                        ajaxsuccess: function() {
                            doc.recall();

                        }}).send();
                });

            });
            return div;
        },
        removeDocumentButtonIcon: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-remove-button-icon');
            var img = $('<img />');
            img.addClass('design-view-document-remove-button-icon-img');
            img.attr('src', '/img/trash.png');
            div.append(img);
            
            return div;
        },
        removeDocumentButtonLabel: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-remove-button-label');
            var txt = $('<div />');
            txt.addClass('design-view-document-remove-button-label-text');
            txt.text(localization.designview.removeThisDocument);
            div.append(txt);
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
            //div.append(view.orText());
            //div.append(view.avtalButton());

            return div;
        },
        uploadFile : function() {
            var view = this;
            var document = view.model;
            var div = $('<div />');
            div.addClass('design-view-document-buttons-upload-button');
            
            var circle = $('<img />');
            circle.addClass('design-view-document-buttons-upload-button-circle');
            circle.attr('src', '/img/circle.png');

            var label = $('<div />');
            label.addClass('design-view-document-buttons-upload-button-label');
            
            var line1 = $('<div />');
            line1.addClass('design-view-document-buttons-upload-button-line1');

            label.append(line1);

            line1.text(localization.designview.openPDF);
   
            div.append(circle);
            div.append(label);

            var url = "/api/frontend/changemainfile/" + document.documentid();

            div.click(function() {
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
                            //mixpanel.track('Error',
                            //               {Message: 'main file too large'});
                            
                        } else {
                            new FlashMessage({content: localization.couldNotUpload, color: "red"});
                            document.unsetFlux();
                            //mixpanel.track('Error',
                            //               {Message: 'could not upload main file'});
                        }
                        document.trigger('change');
                    }
                });
            });

            return div;
        },
        orText: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-buttons-or-text');

            div.text('- or -');

            return div;
        },
        avtalButton: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-buttons-avtal-button');
            var circle = $('<img />');
            circle.addClass('design-view-document-buttons-avtal-button-circle');
            circle.attr('src', '/img/circle.png');

            var label = $('<div />');
            label.addClass('design-view-document-buttons-avtal-button-label');
            
            var line1 = $('<div />');
            line1.addClass('design-view-document-buttons-avtal-button-line1');

            var line2 = $('<div />');
            line2.addClass('design-view-document-buttons-avtal-button-line2');

            label.append(line1);
            label.append(line2);

            line1.text(localization.avtal24.buy);
            line2.text('(' + localization.designview.offsite + ')');
   
            div.append(circle);
            div.append(label);

            div.click(Avtal24Popup);

            return div;
        },
        afterInsert: function() {
            $(window).resize();
        }

    });


    window.DesignViewDocumentView = DesignViewDocumentView;

}(window));
