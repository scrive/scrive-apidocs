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
            $(window).resize(function() {
                var myTop = view.$el.offset().top;
                var winHeight = $(window).height();
                view.$el.css('min-height', Math.max(winHeight - myTop, 300));
                view.refreshMargins();
            });
            view.viewmodel.bind('change:step', function() {
                $(window).resize();
            });
            $(window).resize();
        },
        render: function() {
            var view = this;
            if(view.file) {
                view.file.destroy();
                view.file = undefined;
            }
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
            return view;
        },
        loading: function() {
            this.wrapperDiv = $("<div class='design-view-document-buttons-wrapper'/>");
            var div = $("<div class='design-view-document-loading'/>");
            this.innerDiv = $("<div class='design-view-document-loading-inner''/>");
            div.html(this.innerDiv);
            var spinner = new Spinner({
	        lines  : 10,     // The number of lines to draw
	        length : 19,     // The length of each line
	        width  : 10,     // The line thickness
	        radius : 30,     // The radius of the inner circle
	        color  : '#000', // #rbg or #rrggbb
	        speed  : 1.5,    // Rounds per second
	        trail  : 74,     // Afterglow percentage
	        shadow : false   // Whether to render a shadow
            }).spin(this.innerDiv.get(0));
            this.wrapperDiv.append(div);
            this.refreshMargins();
            return this.wrapperDiv;
        },
        renderDocument: function() {
            var view = this;
            var document = view.model;
            var div = $('<div />');
            div.addClass('design-view-document-pages');
            if (!this.file)
              this.file = KontraFile.init({file: document.mainfile()})
            div.append(this.file.view.el);
            return div;
        },
        refreshMargins : function() {
          if (this.wrapperDiv != undefined)
            this.wrapperDiv.css("height", ($(window).height() - 306) + "px");
          if (this.wrapperDiv != undefined)
            this.innerDiv.css("margin-top", (Math.floor($(window).height() - 306)/2) - 60) + "px";
        },
        uploadButtons: function() {
            var view = this;
            this.wrapperDiv = $("<div class='design-view-document-buttons-wrapper'/>");
            var div = $("<div class='design-view-document-buttons'/>");

            this.innerDiv = $("<div class='design-view-document-buttons-inner'/>");
            div.append(this.innerDiv);

            this.innerDiv.append(view.title()).append(view.buttons());
            this.wrapperDiv.append(div);

            this.refreshMargins();

            return this.wrapperDiv;
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
            var submit = new Submit({
                        method : "POST",
                        url : "/api/frontend/changemainfile/" + document.documentid(),
                        ajaxsuccess: function(d) {
                            document.killAllPlacements();
                            document.recall();
                        },
                        ajaxerror: function(d, a){
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
            var input = UploadButton.init({    color: 'green',
                                     size: 'big',
                                     text: localization.uploadButton,
                                     width: 220,
                                     name: 'file',
                                     maxlength: 2,
                                     onAppend: function(input, title, multifile) {
                                       document.setFlux();
                                       submit.addInputs(input);

                                       document.save();
                                       document.afterSave(function() {
                                           submit.sendAjax();
                                           view.viewmodel.setStep(1);
                                        });
                                     }
                       }).input();
            input.addClass('design-view-document-buttons-upload-button');
            return input;
        },
        afterInsert: function() {
            $(window).resize();
        }

    });


    window.DesignViewDocumentView = DesignViewDocumentView;

}(window));
