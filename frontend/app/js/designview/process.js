/**

    Setting options on the document.

    This is the third tab in the design view.

**/

define(['Backbone', 'tinyMCE', 'tinyMCE_theme', 'tinyMCE_noneeditable', 'tinyMCE_paste', 'legacy_code'], function(Backbone, tinyMCE) {
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
        view.model.document().bind('change:daystosign', view.updateDaysToSign);
        view.model.document().bind('change:daystoremind', view.updateDaysToRemind);
        },
        render: function() {
            var view = this;
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
            div.append(view.language());
            div.append(view.deadline());
            div.append(view.autoreminder());

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

            div.append(view.signviewsettings());

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
        updateDaysToSign : function() {
           var self = this;
           var doc = self.model.document();
           self.daystosigndaysinput.setValue((doc.daystosign() == 1 && self.daystosigndaysinput.value() == "") ? "" :  doc.daystosign() );
           self.daystosigncalendar.setDays(doc.daystosign());
           self.daystoremindcalendar.setMax(doc.daystosign());
        },
        deadline: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();


            var div = $("<div class='design-view-action-process-left-column-deadline'/>");
            var label = $("<div class='design-view-action-process-left-column-deadline-label'/>").text(localization.designview.signingDeadline + ':');
            var calendarbutton = $("<div class='calendarbutton'/>");
            view.daystosigncalendar = new Calendar({on : calendarbutton,
                                         days : doc.daystosign(),
                                         change: function(days) {
                                            if (days != doc.daystosign()) {
                                              doc.setDaystosign(days);
                                            }
                                          }
                        });

            view.daystosigndaysinput = new InfoTextInput({
                infotext: "1",
                value: doc.daystosign(),
                cssClass : 'design-view-action-process-left-column-deadline-field',
                onBlur: function() {
                  view.daystosigndaysinput.setValue(doc.daystosign());
                },
                onChange: function(v) {
                    days = parseInt(v);
                    if (days != undefined && !isNaN(days) && (days + "" == v) && days != doc.daystosign()) {
                      doc.setDaystosign(days);
                    } else if (v == "" && doc.daystosign() != 1) {
                      doc.setDaystosign(1);
                    } else if (v != "" && doc.daystosign() + "" != v) {
                      view.daystosigndaysinput.setValue(doc.daystosign());
                    }
                }
            });

            div.append(label)
               .append(view.daystosigndaysinput.el())
               .append($("<div class='design-view-action-process-left-column-deadline-tag'/>").text(localization.designview.days))
               .append(calendarbutton);


            return div;
        },
        updateDaysToRemind : function() {
           var self = this;
           var doc = self.model.document();
           self.daystoreminddaysinput.setValue(doc.daystoremind() != undefined ? doc.daystoremind() : "");
           self.daystoremindcalendar.setDays(doc.daystoremind());
        },
        autoreminder: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $("<div class='design-view-action-process-left-column-remindline'/>");
            var label = $("<div class='design-view-action-process-left-column-remindline-label'/>").text(localization.autoreminders.sendReminderIn + ':');
            var calendarbutton = $("<div class='calendarbutton'/>");
            view.daystoremindcalendar = new Calendar({on : calendarbutton,
                                         days : doc.daystoremind(),
                                         maxValue : doc.daystosign(),
                                         change: function(days) {
                                            if (days != doc.daystoremind()) {
                                              doc.setDaystoremind(days);
                                            }
                                          }
                        });

            view.daystoreminddaysinput = new InfoTextInput({
                infotext: "-",
                value: doc.daystoremind(),
                cssClass : 'design-view-action-process-left-column-remindline-field',
                onChange: function(v) {
                    days = parseInt(v);
                    if (isNaN(days))
                      days = undefined;
                    if (days != doc.daystoremind()) {
                      doc.setDaystoremind(days);
                    } else if (days == undefined && v != "") {
                      view.daystoreminddaysinput.setValue("");
                    } else if (days + "" != v && v != "") {
                      view.daystoreminddaysinput.setValue(days + "");
                    }


                }
            });

            div.append(label)
               .append(view.daystoreminddaysinput.el())
               .append($("<div class='design-view-action-process-left-column-remindline-tag'/>").text(localization.autoreminders.days))
               .append(calendarbutton);


            return div;
        },
        signviewsettings: function() {
            var view = this;
            var viewmodel = view.model;
            var document = viewmodel.document();

            var div = $("<div class='design-view-action-process-signview-settings'/>");

            var editSignviewSettingsButton = new Button({
                color: 'black',
                text: localization.designview.signviewsettings.button,
                cssClass: 'design-view-action-process-signview-settings-button',
                onClick: function() {
                  mixpanel.track('Open signview settings');
                  new SignviewSettingsPopup({document: document});
                }
            });

            div.append(editSignviewSettingsButton.el());

            return div;
        },
        attachments: function() {
            var view = this;
            var viewmodel = view.model;
            var document = viewmodel.document();

            var div = $("<div class='design-view-action-process-left-column-attachments'/>");
            var label = $("<div class='design-view-action-process-left-column-attachments-label'/>").text(localization.attachments + ':');

            var authorAttachmentButton = new Button({
                color: 'black',
                text: localization.designview.addRemove,
                cssClass: 'design-view-action-process-left-column-attachments-author-button',
                onClick: function() {
                    mixpanel.track('Open author attachments');
                    document.save();
                    new DesignAuthorAttachmentsPopup({document: document});
                }
            });

            var sigAttachmentButton = new Button({
                color: 'black',
                text: localization.designview.request,
                cssClass: 'design-view-action-process-left-column-attachments-signatory-button',
                onClick: function() {
                    mixpanel.track('Open sig attachments');
                    document.save();
                    new DesignSignatoryAttachmentsPopup({document: document});
                }
            });

            //div.append(label);
            div.append(authorAttachmentButton.el());
            div.append(sigAttachmentButton.el());
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


            var wrapper = $('<div />');
            wrapper.addClass('design-view-action-process-right-column-invitation-wrapper');
	    if (!view.emaildeliveryused) {
	      wrapper.addClass('disabled');
	    }

            var textarea = $('<textarea id="design-view-action-process-right-column-invitation-editor" placeholder="' + localization.designview.editInvitation + '"></textarea>');
            textarea.addClass('design-view-action-process-right-column-invitation-editor');
            textarea.hide();

            wrapper.append(textarea);

            view.invitationEditor = textarea;

            var previewLink = new Button({
                color: 'black',
                text: localization.designview.previewInvitation,
                cssClass: 'design-view-action-process-right-column-invitation-link',
                onClick: function() {
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
                }
            });

            div.append(topLine);
            div.append(wrapper);

	    if (view.emaildeliveryused) {
		div.append(previewLink.el());
	    }


            return div.children();
        },
        setupTinyMCE: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();
            var cwidth = view.middleColumnDiv.width();
            if (!doc.ready()) return;
            view.invitationEditor.val(doc.invitationmessage());
            view.invitationEditor.show();
	    if (!view.emaildeliveryused) {
		view.invitationEditor.attr('disabled', '').val('<i>' + localization.designview.editMessagePlaceholder + '</i>');
	    }

            tinyMCE.baseURL = '/libs/tiny_mce';
            tinyMCE.init({
	        selector: '#design-view-action-process-right-column-invitation-editor',
                width: 275, // TODO this is overwritten by cwidth (?)
                height: 138,
                menubar: false,
                plugins: "noneditable,paste",
                readonly: !view.emaildeliveryused,
                valid_elements: "br,em,li,ol,p,span[style<_text-decoration: underline;_text-decoration: line-through;],strong,ul,i[style<_color: #AAAAAA;]",
                width: cwidth, // automatically adjust for different swed/eng text
                font_formats : "Verdana=Source Sans Pro, Helvetica Neue;", // fake using source sans pro / helvetica neue
                setup: function(editor) {

                    editor.on('init', function() {
			$(editor.getDoc()).blur(function() {
			    if (view.emaildeliveryused) {
				doc.setInvitationMessage(editor.getContent());
			    }
			});
			if (!view.emaildeliveryused) {
         		    editor.getWin().document.body.style.color = '#AAAAAA';
			}
                    });

		    editor.on('PreInit', function() {
		        $(editor.getContainer()).find('div[role=toolbar]').hide();
		        $(editor.getContainer()).find('.mce-path').parents('.mce-panel').first().hide();
		    });

                    editor.on('change', function () {
			doc.setInvitationMessage(editor.getContent());
                    });


		    /* Imitate a HTML5 placeholder on the TinyMCE textarea */
		    var placeholder = $('#' + editor.id).attr('placeholder');
		    if (typeof placeholder !== 'undefined' && placeholder !== false) {
			var is_default = false;
			editor.on('init', function() {
			    // get the current content
			    var cont = editor.getContent();

			    // If its empty and we have a placeholder set the value
			    if (cont.length === 0) {
				editor.setContent(placeholder);
			    }

  			    var $message = $(editor.getWin().document).find("p");
			    // change placeholder text color, if it's the 'placeholder text'
			    if($message.text() == localization.designview.editInvitation) {
				$message.css("color", "#999999");
			    }
			}).on('focus', function() {
			      // replace the default content on focus if the
			      // same as original placeholder
			      var $message = $(editor.getWin().document).find("p");
				if ($message.text() == localization.designview.editInvitation) {
				    editor.setContent('');
				}
			}).on('blur', function(ed, e) {
			    // if the input field is empty when leaving it, set default
		  	    // placeholder message
			    var message = editor.getContent();
			    if(message == '') {
			      editor.setContent(placeholder);
			      var message = $(editor.getWin().document).find("p");
			      $(message[0]).css("color", "#999999");
			    }
			});
		    }
		  /* END Imitate placeholder */
                }
            });
            return view;
        }
    });

});
