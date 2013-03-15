/* 
 * New Design view
 *
 * Eric Normand
 */


(function(window){

var DesignViewModel = Backbone.Model.extend({
    defaults : {
        step : 1      
    },
    initialize: function (args) {
        var model = this;
        _.bindAll(model);
    },
    document : function() {
        return this.get("document");
    },
    ready : function() {
        return this.document().ready();
    },
    step: function() {
        return this.get('step');
    },
    setStep: function(s) {
        this.set({step:s});
        console.log(s);
        return this;
    },
    participantDetail: function() {
        return this.get('participantDetail');
    },
    setParticipantDetail: function(s) {
        this.set({participantDetail : s});
        console.log(s);
        return this;
    },
    newSingle: function() {
        return this.get('newSingle');
    },
    setNewSingle: function(sig) {
        this.set({newSingle:sig});
        return sig;
    }
});

// expected model: DesignViewModel
var DesignViewTabsView = Backbone.View.extend({
    className: 'design-view-tab-container',
    initialize: function(args) {
        var view = this;
        _.bindAll(view);
        // probably just need to change a class
        view.model.bind('change:step', view.render);
        view.render();
    },
    render: function() {
        var view = this;
        var model = view.model;

        var div = $('<div />');
        div.append(view.tab1());
        div.append(view.tab2());
        div.append(view.tab3());

        view.$el.html(div.children());

        return view;
    },
    tab1: function () {
        var view = this;
        var model = view.model;

        var div = $('<div />')
            .addClass('design-view-tab1')
            .append($('<div />')
                    .addClass('design-view-tab1-text')
                    .text('Edit participants'));
        div.click(function() {
            model.setStep(1);
        });
        return div;
    },
    tab2: function () {
        var view = this;
        var model = view.model;

        var div = $('<div />')
            .addClass('design-view-tab2')
            .append($('<div />')
                    .addClass('design-view-tab2-text')
                    .text('Edit document ')
                    .append($('<span />')
                            .addClass('design-view-tab2-text-optional')
                            .text('(optional)')));
        div.click(function() {
            model.setStep(2);
        });
        return div;
    },
    tab3: function () {
        var view = this;
        var model = view.model;

        var div = $('<div />')
            .addClass('design-view-tab3')
            .append($('<div />')
                    .addClass('design-view-tab3-text')
                    .text('Edit signing process ')
                    .append($('<span />')
                            .addClass('design-view-tab3-text-optional')
                            .text('(optional)')));
        div.click(function() {
            model.setStep(3);
        });
        return div;
    }
});

// expected model: DesignViewModel
var DesignViewActionsView = Backbone.View.extend({
    className: 'design-view-action-container',
    initialize: function(args) {
        var view = this;
        _.bindAll(view);
        view.participantsView = window.DesignViewParticipantsView({ model : view.model});
        view.draggablesView   = new DesignViewDraggablesView({ model : view.model});
        view.model.bind('change:step', view.render);
        view.render();
    },
    render: function() {
        var view = this;
        var model = view.model;

        if(model.step() === 1) {
            // detach to keep the handlers around
            view.$el.children().detach();
            view.$el.html(view.participantsView.el);
        } else if(model.step() === 2) {
            view.$el.children().detach();
            view.$el.html(view.draggablesView.el);
        } else if(model.step() === 3) {
            // add in the edit process view
            view.$el.children().detach();
            view.$el.html('');
        } else {
            view.$el.children().detach();
            view.$el.html('');
        }

        return view;
    }
});

// expected model: DesignViewModel
var DesignViewButtonBarView = Backbone.View.extend({
    className: 'design-view-button-bar',
    initialize: function(args) {
        var view = this;
        _.bindAll(view);
        view.render();
        view.model.document().bind('change:template', view.render);
    },
    render: function() {
        var view = this;
        var model = view.model;
        
        view.$el.html(view.inner());

        return view;
    },
    inner: function() {
        var view = this;
        var model = view.model;
        
        var div = $('<div />');
        div.addClass('design-view-button-bar-inner');
        div.append(view.saveAsDraft());
        div.append(view.saveAsTemplate());
        div.append(view.send());

        return div;
    },

    saveAsDraft: function() {
        var view = this;
        var viewmodel = view.model;
        
        var div = $('<div />');
        div.addClass('design-view-button1');
        div.append($('<div />')
                   .addClass('design-view-button1-text')
                   .append($('<img />')
                           .addClass('design-view-button1-icon')
                           .attr('src', '/img/save.png'))
                   .append('Save as draft'));

        div.click(function() {
            viewmodel.document().save();
        });

        return div;
    },
    saveAsTemplate: function() {
        var view = this;
        var viewmodel = view.model;

        var div = $('<div />');
        div.addClass('design-view-button2');
        div.append($('<div />')
                   .addClass('design-view-button2-text')
                   .append($('<img />')
                           .addClass('design-view-button2-icon')
                           .attr('src', '/img/template.png'))
                   .append('Save as template'));

        div.click(function() {
            viewmodel.document().makeTemplate();
            viewmodel.document().save();
        });

        return div;
    },
    send: function() {
        var view = this;
        
        var div = $('<div />');
        div.addClass('design-view-button3');
        div.append($('<div />')
                   .addClass('design-view-button3-text')
                   .append('Start signing'));
        return div;
    },
    affix: function() {
        var view = this;
        var $el = view.$el;
        if(!$el.hasClass('design-view-button-bar-fixed'))
            $el.addClass('design-view-button-bar-fixed');
        return view;
    },
    defix: function() {
        var view = this;
        var $el = view.$el;
        $el.removeClass('design-view-button-bar-fixed');
        return view;
    },
    setupScrolling: function(container) {
        var view = this;
        var win = $(window);
        win.scroll(function(){view.updateFixed(container);});
        return view;
    },
    updateFixed: function(container) {
        var view = this;
        var win = $(window);
        var bottomOfWindow = win.scrollTop() + win.height();
        var bottomOfDocView = container.position().top 
            + container.outerHeight();
        if(bottomOfWindow > bottomOfDocView)
            view.defix();
        else
            view.affix();
    }
});

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
    },
    render: function() {
        var view = this;
        var document = view.model;
        view.$el.children().detach();
        if(document.mainfile()) {
            view.$el.html(view.renderDocument());
        } else {
            view.$el.html(view.uploadButtons());
        }
        return view;
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
        var div = $('<div />');
        div.addClass('design-view-document-remove-button-wrapper');
        div.append(view.removeDocumentButtonLabel());
        div.append(view.removeDocumentButtonIcon());
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
        
        div.append(view.uploadFile());

        return div;
    },
    uploadFile : function() {
        var view = this;
        var document = view.model;
        var url = "/api/frontend/mainfile/" + document.documentid();
        var upbutton = UploadButton.init({
            name: "file",
            color : "black",
            shape : "rounded",
            size : "small",
            width: 300,
            text: localization.uploadButton,
            submitOnUpload: true,
            onClick : function () {
            },
            onError: function() {
                document.trigger('change');
            },
            onAppend: function(input) {
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
                            document.fetch();
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
        return upbutton.input();
    },

});



var DesignViewView = Backbone.View.extend({
    className: 'design-view-frame',
    initialize: function (args) {
        var view = this;
        _.bindAll(view);
        view.tabsView    = new DesignViewTabsView({model : view.model});
        view.actionsView = new DesignViewActionsView({model : view.model});
        view.buttonBar   = new DesignViewButtonBarView({model : view.model});
        view.documentView = new DesignViewDocumentView({model : view.model.document(),
                                                        viewmodel : view.model});
        view.render();
        view.buttonBar.setupScrolling(view.$el);
    },
    frame: function() {
        var view = this;
        var div = $('<div/>');
        div.append(view.tabsView.el);
        div.append(view.actionsView.el);
        div.append(view.documentView.el);
        div.append(view.buttonBar.el);
        return div.children();
    },
    render: function() {
        var view = this;
        view.$el.html(view.frame());
        return view;
    },
    // call this after insertion into the dom
    finish: function() {
        var view = this;
        view.buttonBar.updateFixed(view.$el);
        return view;
    }
});

window.DesignView = function(args) {
    var document = new Document({
        id : args.id
    });
    var model = new DesignViewModel({
        document : document
    });
    var view = new DesignViewView({
        model: model
    });
    document.fetch({ processData:  true, cache : false
                   });
    this.el = function() {return $(view.el);};
    this.finish = function() {
        view.finish();
        return this.el();
    };
}

})(window);
