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
            view.render();
            view.model.document().bind('change', view.render);
        },
        render: function() {
            var view = this;
            console.log("Rendering process");
            var div = $('<div />');

            div.append(view.leftColumn());
            div.append(view.middleColumn());
            div.append(view.rightColumn());

            view.$el.html(div.children());
            view.setupTinyMCE();
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
            //div.append(view.attachments());

            return div;
        },
        middleColumn: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-process-middle-column');
            view.middleColumnDiv = div;
            div.append(view.invitationBox());

            return div;
        },
        rightColumn: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-process-right-column');

            div.append(view.attachments());

            return div;
        },
        documentName: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var labelText = localization.designview.documentName;

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

            var labelText = localization.designview.documentType;

            var process = doc.process();
            var processName = 'Contract';
            if(process)
                processName = process.process();

            var processText = {
                Contract : { name : localization.process.contract.name,
                             value : 'Contract' },
                Offer    : { name : localization.process.offer.name,
                             value : 'Offer'    },
                Order    : { name : localization.process.order.name,
                             value : 'Order'    }
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

            var labelText = localization.designview.recipientsLanguage;

            var languageText = {
                en : {name: localization.languages.en, value: 'en'},
                sv : {name: localization.languages.sv, value: 'sv'}
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

            var labelText = localization.designview.signingDeadline;

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-deadline');


            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-deadline-label');
            label.text(labelText + ':');
            console.log(doc.daystosign());
            var calendarbutton = $("<div class='calendarbutton'/>");
            var calendar = new Calendar({on : calendarbutton,
                                         days : doc.daystosign(),
                                         change: function(days) {
                                            if (days != doc.daystosign()) {
                                              doc.setDaystosign(days);
                                              if (view.daysinputfield != undefined)
                                                  view.daysinputfield.setValue(days)
                                            }
                                          }
                        });

            view.daysinputfield = InfoTextInput.init({
                infotext: doc.daystosign(),
                value: doc.daystosign(),
                onChange: function(v) {
                    v = parseInt(v);
                    if (v != undefined && !isNaN(v) && v != doc.daystosign()) {
                      doc.setDaystosign(v);
                      calendar.setDays(v);
                    }
                }
            });
            view.daysinputfield.input().addClass('design-view-action-process-left-column-deadline-field');

            var tag = $('<div />');
            tag.addClass('design-view-action-process-left-column-deadline-tag');
            tag.text(localization.designview.days);

            div.append(label);
            div.append(view.daysinputfield.input());
            div.append(tag);
            div.append(calendarbutton);


            return div;
        },
        attachments: function() {
            var view = this;
            var viewmodel = view.model;
            var document = viewmodel.document();

            var div = $('<div />');
            div.addClass('design-view-action-process-left-column-attachments');

            var labelText = localization.attachments;

            var label = $('<div />');
            label.addClass('design-view-action-process-left-column-attachments-label');
            label.text(labelText + ':');

            var authorAttachmentButton = Button.init({
                color: 'blue',
                size: 'tiny',
                text: localization.designview.addRemove,
                cssClass: 'design-view-action-process-left-column-attachments-author-button',
                onClick: function() {
                    document.save();
                    DesignAuthorAttachmentsPopup.popup({document: document});
                }
            });

            var sigAttachmentButton = Button.init({
                color: 'blue',
                size: 'tiny',
                text: localization.designview.request,
                cssClass: 'design-view-action-process-left-column-attachments-signatory-button',
                onClick: function() {
                    document.save();
                    DesignSignatoryAttachmentsPopup.popup({document: document});
                }
            });

            //div.append(label);
            div.append(authorAttachmentButton.input());
            div.append(sigAttachmentButton.input());
            if (this.attachmentList != undefined)
            {
              this.attachmentList.destroy();
              this.attachmentList = undefined;
            }
            this.attachmentList = new DesignAttachmentsList({viewmodel : viewmodel});

            div.append(this.attachmentList.el());

            return div;
        },
        invitationBox: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');

            var topLine = $('<div />');
            topLine.addClass('design-view-action-process-right-column-invitation-topline');

            var label = $('<div />');
            label.addClass('design-view-action-process-right-column-invitation-label');
            label.text(localization.designview.editInvitation);
            view.editInvitationLabel = label;

            var wrapper = $('<div />');
            wrapper.addClass('design-view-action-process-right-column-invitation-wrapper');

            var textarea = $('<textarea />');
            textarea.addClass('design-view-action-process-right-column-invitation-editor');
            textarea.hide();
            wrapper.append(textarea);

            view.invitationEditor = textarea;

            var previewLink = $('<a />');
            previewLink.addClass('design-view-action-process-right-column-invitation-link');
            previewLink.text(localization.designview.previewInvitation);
            previewLink.click(function() {
                doc.save();
                doc.afterSave(function() {
                    var popup = ConfirmationWithEmail.popup({
                        editText: '',
                        title: localization.editInviteDialogHead,
                        mail: doc.inviteMail(),
                        onAccept: function() {
                            popup.close();
                        }
                    });
                });
            });

            topLine.append(label);
            topLine.append(previewLink);

            div.append(topLine);
            div.append(wrapper);

            return div.children();
        },
        setupTinyMCE: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var cwidth = view.middleColumnDiv.width();
            view.invitationEditor.html(doc.invitationmessage());
            view.invitationEditor.show();

            view.invitationEditor.tinymce({
                script_url: '/tiny_mce/tiny_mce.js',
                theme: "advanced",
                theme_advanced_toolbar_location: "external",
                theme_advanced_buttons1: "",
                //theme_advanced_buttons2: "",
                convert_urls: false,
                theme_advanced_toolbar_align: "middle",
                plugins: "noneditable,paste",
                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul",
                width: cwidth, // automatically adjust for different swed/eng text
                oninit : function(ed) {
                    $('.mceExternalToolbar').css('z-index','-1000');
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
