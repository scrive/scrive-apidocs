/** 

    Setting options on the document.
    
    This is the third tab in the design view.

**/

(function(window) {
    /** 
        model is DocViewModel
    **/
    var DesignViewProcessView = Backbone.View.extend({
        className: 'design-view-action-process',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.mail = view.model.document().inviteMail();
            view.render();
            view.mail.bind('change', view.changeMail);
            view.model.document().bind('change', view.render);
        },
        render: function() {
            var view = this;
            
            var div = $('<div />');

            div.append(view.leftColumn());
            div.append(view.rightColumn());

            view.$el.html(div.children());

            return view;
        },
        leftColumn: function() {
            var view = this;

            var div =  $('<div />');
            div.addClass('design-view-action-process-left-column');

            div.append(view.documentName());
            div.append(view.documentType());
            div.append(view.language());
            div.append(view.deadline());

            return div;
        },
        rightColumn: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-process-right-column');
            view.rightColumnDiv = div;
            div.append(view.invitationBox());

            return div;
        },
        documentName: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var labelText = 'Document name';

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-document-name');


            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-document-name-label');
            label.text(labelText + ':');

            var field = InfoTextInput.init({
                infotext: labelText,
                value: doc.title(),
                onChange: function(v) {
                    doc.setTitle(v);
                }
            });

            field.input().addClass('design-view-action-process-left-column-document-name-field');

            doc.bind('change:title', function() {
                field.setValue(doc.title());
            });

            div.append(label);
            div.append(field.input());

            return div;
        },
        documentType: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var labelText = 'Document type';

            var process = doc.process();
            var processName = 'Contract';
            if(process)
                processName = process.process();

            var processText = {
                Contract : { name : 'Contract' , value : 'Contract' },
                Offer    : { name : 'Offer'    , value : 'Offer'    },
                Order    : { name : 'Order'    , value : 'Order'    }
            };

            var processTypes = ['Contract', 'Offer', 'Order'];

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-document-type');

            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-document-type-label');
            label.text(labelText + ':');

            var field = new Select({
                options: _.map(processTypes, function(e) {
                    return processText[e];
                }),
                name: processText[processName].name,
                onSelect: function(v) {
                    doc.process().setProcess(v);
                    return true;
                }
            });

            field.input().addClass('design-view-action-process-left-column-document-type-field');
            doc.bind('change:process', function() {
                    field.model().setName(processText[doc.process().process()].name);
                });

            div.append(label);
            div.append(field.input());

            return div;
        },
        language: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var language = doc.lang();

            var lang = 'sv';
            if(language)
                lang = language.simpleCode();

            var labelText = 'Recipient\'s language';

            var languageText = {
                en : {name: 'English', value: 'en'},
                sv : {name: 'Swedish', value: 'sv'}
            };

            var languages = ['sv', 'en'];

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-language');


            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-language-label');
            label.text(labelText + ':');

            var field = new Select({
                options: _.map(languages, function(e) {
                    return languageText[e];
                }),
                name: languageText[lang].name,
                onSelect: function(v) {
                    doc.lang().setLanguage(v);
                    return true;
                }
            });

            field.input().addClass('design-view-action-process-left-column-language-field');
            doc.bind('change:lang', function() {
                field.model().setName(languageText[doc.lang().simpleCode()].name);
            });

            div.append(label);
            div.append(field.input());

            return div;
        },
        deadline: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var labelText = 'Signing deadline';

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-deadline');


            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-deadline-label');
            label.text(labelText + ':');
            console.log(doc.daystosign());
            var field = InfoTextInput.init({
                infotext: doc.daystosign(),
                value: doc.daystosign(),
                onChange: function(v) {
                    // TODO: this does not seem to connect to the saved document
                    v = parseInt(v);
                    doc.setDaystosign(v);
                }
            });
            field.input().addClass('design-view-action-process-left-column-deadline-field');

            var tag = $('<div />');
            tag.addClass('design-view-action-process-left-column-deadline-tag');
            tag.text('day(s)');

            div.append(label);
            div.append(field.input());
            div.append(tag);

            return div;
        },
        invitationBox: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');

            var label = $('<div />');
            label.addClass('design-view-action-process-right-column-invitation-label');
            label.text('Edit invitation:');
            view.editInvitationLabel = label;

            var wrapper = $('<div />');
            wrapper.addClass('design-view-action-process-right-column-invitation-wrapper');
            
            var textarea = $('<textarea />');
            textarea.addClass('design-view-action-process-right-column-invitation-editor');
            if(view.mail.ready()) {
                var editableContent = view.mail.content().find(".editable").html();
                textarea.html(editableContent);
            }
            wrapper.html(textarea);

            view.invitationEditor = textarea;

            var previewButton = Button.init({
                color: 'blue',
                text: 'Preview invitation',
                size: 'tiny',
                onClick: function() {
                    doc.save();
                    doc.afterSave(function() {
                        ConfirmationWithEmail.popup({
                            editText: '',
                            title: localization.editInviteDialogHead,
                            mail: doc.inviteMail()
                        });
                    });
                }
            });

            var buttonWrapper = $('<div />');
            buttonWrapper.addClass('design-view-action-process-right-column-invitation-button-wrapper');
            buttonWrapper.append(previewButton.input());
            view.invitationPreviewButton = buttonWrapper;
            
            div.append(label);
            div.append(wrapper);
            div.append(buttonWrapper);

            return div.children();;
        },
        changeMail: function() {
            var view = this;
            if(view.mail.ready()) {
                var editableContent = view.mail.content().find(".editable").html();
                view.invitationEditor.html(editableContent);
            }
            view.afterInsertion();
        },
        // this needs to be called AFTER it gets put into the document
        afterInsertion: function() {
            this.setupTinyMCE();
        },
        setupTinyMCE: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var lwidth = view.editInvitationLabel.outerWidth();
            var cwidth = view.rightColumnDiv.width();

            view.invitationEditor.tinymce({
                script_url: '/tiny_mce/tiny_mce.js',
                theme: "advanced",
                theme_advanced_toolbar_location: "top",
                theme_advanced_buttons1: "bold,italic,underline,separator,strikethrough,bullist,numlist,separator,undo,redo,separator,cut,copy,paste",
                theme_advanced_buttons2: "",
                convert_urls: false,
                theme_advanced_toolbar_align: "middle",
                plugins: "noneditable,paste",
                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul",
                width: cwidth-lwidth-8, // automatically adjust for different swed/eng text
                oninit : function(ed) {
                    $(ed.getDoc()).blur(function(e) {
                        doc.setInvitationMessage(ed.getBody().innerHTML);
                    });
                },
                onchange_callback  : function (inst) {
                    doc.setInvitationMessage(inst.getBody().innerHTML);
                }
            });

            return view;
        }
    });

    window.DesignViewProcessView = function(args) {
        return new DesignViewProcessView(args);
    };

}(window));
