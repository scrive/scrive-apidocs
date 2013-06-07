/**

    Setting options on the document.

    This is the third tab in the design view.

**/

(function(window) {
    /**
        model is DocViewModel
    **/
    window.DesignViewProcessView = Backbone.View.extend({
        className: 'design-view-action-process',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
            view.model.document().bind('change', view.render);
	    view.emailDeliveryUsedTogglerWorker();
	    view.model.document().bind('change:signatories', view.emailDeliveryUsedToggler);
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
	rerenderMiddleColumn: function() {
	    var view = this;
	    view.middleColumnDiv.html('').append(view.invitationBox());
	    view.setupTinyMCE();
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

            var field = new InfoTextInput({
                infotext: labelText,
                value: doc.title(),
                onChange: function(v) {
                    doc.setTitle(v);
                }
            });

            field.el().addClass('design-view-action-process-left-column-document-name-field');

            doc.bind('change:title', function() {
                field.setValue(doc.title());
            });

            div.append(label);
            div.append(field.el());

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

            var select = new Select({
                options: _.map(processTypes, function(e) {
                    return processText[e];
                }),
                name: processText[processName].name,
                textWidth: "130px",
                cssClass : 'design-view-action-process-left-column-document-type-field',
                onSelect: function(v) {
                    mixpanel.track('Select document type', {
                        'Document type' : v
                    });
                    doc.process().setProcess(v);
                    return true;
                }
            });
            doc.bind('change:process', function() {
                    select.setName(processText[doc.process().process()].name);
                });

            return div.append(label).append(select.el());

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

            var div = $("<div class='design-view-action-process-left-column-language'/>");


            var label = $("<div class='design-view-action-process-left-column-language-label''/>").text(labelText + ':');

            var select = new Select({
                options: _.map(languages, function(e) {
                    return languageText[e];
                }),
                textWidth: "130px",
                name: languageText[lang].name,
                cssClass : 'design-view-action-process-left-column-language-field',
                onSelect: function(v) {
                    mixpanel.track('Select language',
                                   {'New Language': v});
                    doc.lang().setLanguage(v);
                    return true;
                }
            });

            doc.bind('change:lang', function() {
                select.setName(languageText[doc.lang().simpleCode()].name);
            });

            return div.append(label).append(select.el());
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
                                                  view.daysinputfield.setValue(days);
                                            }
                                          }
                        });

            view.daysinputfield = new InfoTextInput({
                infotext: doc.daystosign(),
                value: doc.daystosign(),
                cssClass : 'design-view-action-process-left-column-deadline-field',
                onChange: function(v) {
                    v = parseInt(v);
                    if (v != undefined && !isNaN(v) && v != doc.daystosign()) {
                      doc.setDaystosign(v);
                      calendar.setDays(v);
                    }
                }
            });

            div.append(label)
               .append(view.daysinputfield.el())
               .append($("<div class='design-view-action-process-left-column-deadline-tag'/>").text(localization.designview.days))
               .append(calendarbutton);


            return div;
        },
        attachments: function() {
            var view = this;
            var viewmodel = view.model;
            var document = viewmodel.document();

            var div = $("<div class='design-view-action-process-left-column-attachments'/>");
            var label = $("<div class='design-view-action-process-left-column-attachments-label'/>").text(localization.attachments + ':');

            var authorAttachmentButton = Button.init({
                color: 'blue',
                size: 'tiny',
                text: localization.designview.addRemove,
                cssClass: 'design-view-action-process-left-column-attachments-author-button',
                onClick: function() {
                    mixpanel.track('Open author attachments');
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
                    mixpanel.track('Open sig attachments');
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
	emailDeliveryUsedToggler: function() {
	    var view = this;
	    _.each(this.model.document().signatories(), function(signatory) {
		signatory.unbind('change:delivery', view.emailDeliveryUsedTogglerWorker);
		signatory.bind('change:delivery', view.emailDeliveryUsedTogglerWorker);
            });
	    view.emailDeliveryUsedTogglerWorker();
	},
	emailDeliveryUsedTogglerWorker: function() {
	    this.emaildeliveryused = _.some(this.model.document().signatories(), function(signatory) {
		return signatory.delivery() == 'email' || signatory.delivery() == 'email_mobile';
            });
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
	    if (!view.emaildeliveryused) {
		label.css('color','#AAAAAA');
	    }
            label.text(localization.designview.editInvitation);
            view.editInvitationLabel = label;

            var wrapper = $('<div />');
            wrapper.addClass('design-view-action-process-right-column-invitation-wrapper');
	    if (!view.emaildeliveryused) {
	      wrapper.addClass('disabled');
	    }

            var textarea = $('<textarea id="design-view-action-process-right-column-invitation-editor"/>');
            textarea.addClass('design-view-action-process-right-column-invitation-editor');
            textarea.hide();

            wrapper.append(textarea);

            view.invitationEditor = textarea;

            var previewLink = $('<a />');
            previewLink.addClass('design-view-action-process-right-column-invitation-link');
            previewLink.text(localization.designview.previewInvitation);
            previewLink.click(function() {
                mixpanel.track('Open invitation preview');
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
	    if (view.emaildeliveryused) {
		topLine.append(previewLink);
	    }

            div.append(topLine);
            div.append(wrapper);

            return div.children();
        },
        setupTinyMCE: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();
            var cwidth = view.middleColumnDiv.width();
            if (!doc.ready()) return;
            view.invitationEditor.html(doc.invitationmessage());
            view.invitationEditor.show();
	    if (!view.emaildeliveryused) {
		view.invitationEditor.attr('disabled', '').val('<i>' + localization.designview.editMessagePlaceholder + '</i>');
	    }
            tinymce.init({
	        selector: '#design-view-action-process-right-column-invitation-editor',
                width: 300,
                menubar: false,
                plugins: "noneditable,paste",
                external_plugins: {
                  hide_toolbar: '/js/tinymce_plugins/hide_toolbar.js'
                },
		readonly: !view.emaildeliveryused,
                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul,i[style<_color: #AAAAAA;]",
                width: cwidth, // automatically adjust for different swed/eng text
                setup: function(editor) {
                  editor.on('init', function() {
                    $(editor.getDoc()).blur(function() {
		      if (view.emaildeliveryused) {
                        doc.setInvitationMessage(editor.getBody().innerHTML);
		      }
                    });
		    if (!view.emaildeliveryused) {
         	      editor.getWin().document.body.style.color = '#AAAAAA';
		    }
                  });
                  editor.on('change', function () {
                    doc.setInvitationMessage(editor.getBody().innerHTML);
                  });
                }
            });

            return view;
        }
    });

}(window));
