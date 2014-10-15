/**

   Design Document View

   Shows upload buttons when there is no document and pages when there is.

**/

define(['Spinjs', 'Backbone', 'legacy_code'], function(Spinner) {

    // expected model: document
    // expected viewmodel: DesignViewModel
    var DesignViewDocumentView = Backbone.View.extend({
        className: 'design-view-document-container',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.viewmodel = args.viewmodel;

            view.render();
            view.model.bind('change:ready', view.render);
            view.model.bind('change:file', view.render);
            $(window).resize(function() {
                var myTop = view.$el.offset().top;
                var winHeight = $(window).height();
                view.refreshMargins();
            });
            view.viewmodel.bind('change:step', function() {
                $(window).resize();
            });
            $(window).resize();
        },
        render: function() {
            var view = this;
            if(view.file && (view.file.model != view.model.mainfile())) {
                view.file.destroy();
                view.file = undefined;
            }
            var model = view.viewmodel;
            var document = view.model;
            view.$el.children().detach();
            if(!document.ready()) {
                view.$el.html(view.loading());
            } else if(document.mainfile()) {
                view.$el.html(view.renderDocument());
            } else {
                view.$el.html(view.uploadButtons());
            }
            return view;
        },
        loading: function() {
            this.wrapperDiv = $("<div class='design-view-document-buttons-wrapper-outer'/>");
            var innerWrapper = $("<div class='design-view-document-buttons-wrapper'/>");
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
	        shadow : false,  // Whether to render a shadow
                zIndex: 100      // Don't be extreme and put spinner over error dialog
            }).spin(this.innerDiv.get(0));
            this.wrapperDiv.append(innerWrapper.append(div));
            this.refreshMargins();
            return this.wrapperDiv;
        },
        renderDocument: function() {
            var view = this;
            var document = view.model;
            var div = $('<div />');
            div.addClass('design-view-document-pages');
            if (!this.file)
              this.file = new KontraFile({file: document.mainfile()});
            div.append(this.file.view.el);
            return div;
        },
        // this is called when designview window resizes and fixes
        // the size of the empty document space (before document upload)
        refreshMargins : function() {
          var designViewFrameTopBar = $('.design-view-frame-top-bar');
          var designViewButtonBar = $('.design-view-button-bar');
          if (designViewFrameTopBar.length == 0 || designViewButtonBar.length == 0) {
            // content not displayed yet, skip margin fixing
            return;
          }

          var sizeOfEverythingAboveEmptyDocSpace = designViewFrameTopBar.height() + designViewFrameTopBar.offset().top;
          var sizeOfFooter = $(window).height() - designViewButtonBar.offset().top; // size of everything that is below the empty document space
          var paddingSize = 36; // 2 * documentview.less: .design-view-document-buttons-wrapper-outer[padding-(top|bottom)]
          var docHeight = Math.floor($(window).height() - sizeOfEverythingAboveEmptyDocSpace - sizeOfFooter - paddingSize);

          if (this.wrapperDiv != undefined) {
            this.wrapperDiv.css('height', docHeight + 'px');
          }

          if (this.innerDiv != undefined) {
            // shift upload button to the middle of empty space (if there's enough of it)
            var uploadButtonDivHeight = $('.design-view-document-buttons').height();
            if (docHeight >= uploadButtonDivHeight) {
              var docMinHeight = 220; // documentview.less: .design-view-document-buttons-wrapper-outer[min-height]
              var realDocSize = Math.max(docHeight, docMinHeight);
              this.innerDiv.css('margin-top', (Math.floor((realDocSize - uploadButtonDivHeight) / 2)) + 'px');
            } else {
              this.innerDiv.css('margin-top', '');
            }
          }
        },
        uploadButtons: function() {
            var view = this;
            this.wrapperDiv = $("<div class='design-view-document-buttons-wrapper-outer'/>");
            this.wrapperDiv.addClass('white-background');
            var innerWrapper = $("<div class='design-view-document-buttons-wrapper'/>");
            var div = $("<div class='design-view-document-buttons'/>");

            this.innerDiv = $("<div class='design-view-document-buttons-inner'/>");
            div.append(this.innerDiv);

            this.innerDiv.append(view.buttons());
            innerWrapper.append(div);
            this.wrapperDiv.append(innerWrapper);

            this.refreshMargins();

            return this.wrapperDiv;
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
            var viewmodel = view.viewmodel;
            var document = view.model;

            var url = "/api/frontend/changemainfile/" + document.documentid();
            var submit = new Submit({
                        method : "POST",
                        url : "/api/frontend/changemainfile/" + document.documentid(),
                        ajaxsuccess: function(d) {
                            mixpanel.track('Upload main file');
                            document.killAllPlacements();
                            document.recall();
                        },
                        ajaxerror: function(d, a){
                            console.log(d);
                            if(a === 'parsererror') { // file too large
                                new FlashMessage({content: localization.fileTooLarge, color: "red"});
                                document.markAsNotReady();
                                mixpanel.track('Error',
                                               {Message: 'main file too large'});

                            } else {
                                new FlashMessage({content: localization.couldNotUpload, color: "red"});
                                document.markAsNotReady();
                                mixpanel.track('Error',
                                               {Message: 'could not upload main file'});
                            }
                            document.recall();
                        }
            });
            return new UploadButton({    color: 'green',
                                     size: 'big',
                                     text: localization.uploadButton,
                                     width: 250,
                                     name: 'file',
                                     maxlength: 2,
                                     cssClass : 'design-view-document-buttons-upload-button',
                                     onAppend: function(input, title, multifile) {
                                       document.markAsNotReady();
                                       submit.addInputs(input);
                                       viewmodel.saveMaybeFlashMessage();
                                       document.afterSave(function() {
                                           submit.sendAjax();
                                       });
                                     }
                       }).el();
        },
        afterInsert: function() {
            $(window).resize();
        }

    });


    window.DesignViewDocumentView = DesignViewDocumentView;

});
