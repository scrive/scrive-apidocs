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
        },
        render: function() {
            var view = this;
            var document = view.model;
            view.$el.children().detach();
            if(document.flux()) {
                view.$el.html(view.loading());
            } else if(document.mainfile()) {
                view.$el.html(view.renderDocument());
            } else {
                view.$el.html(view.uploadButtons());
            }
            return view;
        },
        loading: function() {
            var div = $('<div />');
            div.addClass('design-view-document-loading');
            var inner = $('<div />');
            inner.addClass('design-view-document-loading-inner');
            div.html(inner);
            var spinner = new Spinner({
	        lines: 10, // The number of lines to draw
	        length: 19, // The length of each line
	        width: 10, // The line thickness
	        radius: 30, // The radius of the inner circle
	        color: '#000', // #rbg or #rrggbb
	        speed: 1.5, // Rounds per second
	        trail: 74, // Afterglow percentage
	        shadow: false // Whether to render a shadow
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
                doc.save();
                doc.afterSave( function() {
                    new Submit({
                        method : "POST",
                        url :  "/api/frontend/mainfile/" + doc.documentid(),
                        ajax: true,
                        onSend: function() {
                            
                        },
                        ajaxerror: function(d,a){
                            doc.fetch();
                            doc.unsetFlux();
                        },
                        ajaxsuccess: function() {
                            doc.fetch({
                                success: function() {
                                    doc.unsetFlux();
                                },
                                error: function() {
                                    doc.unsetFlux();
                                }

                            });

                        }}).send();
                });

            });
            return div;
        },
        removeDocumentButtonIcon: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-document-remove-button-icon');
            var img = $('<div />');
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
            txt.text('Remove this document');
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

            div.text('Choose a document');

            return div;
        },
        buttons: function() {
            var view = this;
            var div = $('<div />');

            div.addClass('design-view-document-buttons-buttons');

            div.append(view.uploadFile());
            div.append(view.orText());
            div.append(view.avtalButton());

            return div;
        },
        uploadFile : function() {
            var view = this;
            var document = view.model;
            var div = $('<div />');
            
            var circle = $('<img />');
            circle.addClass('design-view-document-buttons-upload-button-circle');
            circle.attr('src', '/img/circle.png');

            var label = $('<div />');
            label.addClass('design-view-document-buttons-upload-button-label');
            
            var line1 = $('<div />');
            line1.addClass('design-view-document-buttons-upload-button-line1');

            label.append(line1);

            line1.text('Open a PDF file');
   
            div.append(circle);
            div.append(label);

            var url = "/api/frontend/mainfile/" + document.documentid();
            var upbutton = UploadButton.init({
                button: div,
                name: "file",
                color : "black",
                shape : "rounded",
                size : "small",
                width: 110,
                height: 170,
                text: localization.uploadButton,
                submitOnUpload: true,
                onClick : function () {
                    console.log('click');
                },
                onError: function() {
                    document.trigger('change');
                },
                onAppend: function(input) {
                    document.setFlux();
                    document.save();

                    document.afterSave( function() {
                        new Submit({
                            method : "POST",
                            url : url,
                            ajax: true,
                            beforeSend: function() {

                            },
                            onSend: function() {
                                //LoadingDialog.open();
                            },
                            ajaxtimeout : 40000,
                            ajaxerror: function(d,a){
                                //LoadingDialog.close();
                                if(a === 'parsererror') { // file too large
                                    new FlashMessage({content: localization.fileTooLarge, color: "red"});
                                    //mixpanel.track('Error',
                                    //               {Message: 'main file too large'});

                                }
                                else {
                                    new FlashMessage({content: localization.couldNotUpload, color: "red"});
                                    //mixpanel.track('Error',
                                    //               {Message: 'could not upload main file'});
                                }
                                //LoadingDialog.close();
                                document.trigger('change');
                            },
                            ajaxsuccess: function() {
                                document.save();
                                document.afterSave(function() {
                                    document.fetch({
                                        success: function() {
                                            document.unsetFlux();
                                        },
                                        error : function() {
                                            document.unsetFlux();
                                        }
                                    });
                                });
                                //trackTimeout('Upload main file', {}, function() {
                                //LoadingDialog.close();
                                //window.location.reload();
                                //});
                            }
                        }).addInputs(input).send();
                    });
                }
            });

            var btn = upbutton.input();
            btn.addClass('design-view-document-buttons-upload-button');
            return btn;
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

            line1.text('Create from Avtal24');
            line2.text('(offsite)');
   
            div.append(circle);
            div.append(label);

            return div;
        }

    });


    window.DesignViewDocumentView = DesignViewDocumentView;

}(window));
