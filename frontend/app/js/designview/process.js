/**

    Setting options on the document.

    This is the third tab in the design view.

**/

define(['Backbone', 'React', 'common/customtexteditor',  'tinyMCE', 'tinyMCE_theme', 'tinyMCE_noneeditable', 'tinyMCE_paste', 'legacy_code'], function(Backbone, React, CustomTextEditor, tinyMCE) {
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
            return view;
        },
        hideAllCalendars: function() {
           var self = this;
           if (self.daystosigncalendar != undefined) self.daystosigncalendar.close();
           if (self.daystoremindcalendar != undefined) self.daystoremindcalendar.close();
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
            div.append(view.invitationAndConfirmationBox());

            return div;
        },
        rerenderMiddleColumn: function() {
          var view = this;
          view.middleColumnDiv.html('').append(view.invitationAndConfirmationBox());
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
            label.text(labelText);

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


            var languages = [
                {name: localization.languages.en, value: "en"}
              , {name: localization.languages.sv, value: "sv"}
              , {name: localization.languages.de, value: "de"}
              , {name: localization.languages.fr, value: "fr"}
              , {name: localization.languages.it, value: "it"}
              , {name: localization.languages.es, value: "es"}
              , {name: localization.languages.pt, value: "pt"}
              , {name: localization.languages.nl, value: "nl"}
              , {name: localization.languages.da, value: "da"}
              , {name: localization.languages.no, value: "no"}
              , {name: localization.languages.el, value: "el"}
              , {name: localization.languages.fi, value: "fi", hidden : true}
            ];
            languages = _.sortBy(languages, function(l) {return l.name.toLowerCase();});
            var lname = _.findWhere(languages, {value :lang}).name;


            var div = $("<div class='design-view-action-process-left-column-language'/>");


            var label = $("<div class='design-view-action-process-left-column-language-label''/>").text(labelText);

            var select = new Select({
                options: _.filter(languages, function(l) { return !l.hidden;}),
                textWidth: "130px",
                name: lname,
                cssClass : 'design-view-action-process-left-column-language-field',
                onSelect: function(v) {
                    mixpanel.track('Select language',
                                   {'New Language': v});
                    doc.lang().setLanguage(v);
                    return true;
                }
            });

            doc.bind('change:lang', function() {
              select.setName(_.findWhere(languages, {value : doc.lang().simpleCode()}).name);
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
            var label = $("<div class='design-view-action-process-left-column-deadline-label'/>").text(localization.designview.signingDeadline);
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
                      days = Math.min(90, days);
                      doc.setDaystosign(days);
                      view.daystosigndaysinput.setValue(doc.daystosign());
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
            var label = $("<div class='design-view-action-process-left-column-remindline-label'/>").text(localization.autoreminders.sendReminderIn);
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
                    new DesignAuthorAttachmentsPopup({viewmodel: viewmodel});
                }
            });

            var sigAttachmentButton = new Button({
                color: 'black',
                text: localization.designview.request,
                cssClass: 'design-view-action-process-left-column-attachments-signatory-button',
                onClick: function() {
                  if(document.signatoriesWhoSign().length < 2) {
                    mixpanel.track('Open signatory attachments but not enough participants');
                    new FlashMessage({ color: 'red'
                                     , content: localization.designview.validation.requestAttachmentFlashMessage});
                  } else {
                    mixpanel.track('Open signatory attachments');
                    document.save();
                    new DesignSignatoryAttachmentsPopup({viewmodel: viewmodel});
                  }
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
        invitationAndConfirmationBox: function() {
           var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');

            // If the document is not ready, don't initialize tinyMCE for invitation or confirmation boxes.
            // If we do, it leads to race conditions within tinyMCE as we remove/init 3 times in a normal pageload to design view.
            if (!doc.ready()) { return div; }

            if (view.invitationmessagewrapper != undefined) React.unmountComponentAtNode(view.invitationmessagewrapper[0]);
            view.invitationmessagewrapper = $('<div />');

            var emailInvitationMessageEditable = _.any(doc.signatories(), function(s) { return s.emailDelivery() || s.emailMobileDelivery();});

            React.renderComponent(CustomTextEditor({
              id : 'design-view-action-process-right-column-invitation-editor',
              customtext : doc.invitationmessage(),
              editable : emailInvitationMessageEditable,
              width: view.middleColumnDiv.width(),
              label: localization.designview.customMessage.invitation,
              previewLabel : localization.designview.customMessage.preview,
              onChange: function(c) {doc.setInvitationMessage(c);},
              placeholder :  localization.designview.editInvitation,
              disabledPlaceholder : localization.designview.editMessagePlaceholder,
              onPreview : function() {
                mixpanel.track('Open invitation preview');
                doc.save();
                doc.afterSave(function() {
                var popup = ConfirmationWithEmail.popup({
                              editText: '',
                              title: localization.designview.customMessage.invitation,
                              mail: doc.inviteMail(),
                              onAccept: function() {
                                popup.close();
                              }
                            });
                });
              }
            }), view.invitationmessagewrapper[0]);



            if (view.confirmationmessagewrapper != undefined) React.unmountComponentAtNode(view.confirmationmessagewrapper[0]);
            view.confirmationmessagewrapper = $("<div style='margin-top:15px'/>");

            var emailConfirmationMessageEditable = _.any(doc.signatories(), function(s) { return s.emailConfirmationDelivery() || s.emailMobileConfirmationDelivery();});

            React.renderComponent(CustomTextEditor({
              id : 'design-view-action-process-right-column-confirmation-editor',
              customtext : doc.confirmationmessage(),
              editable : emailConfirmationMessageEditable,
              width: view.middleColumnDiv.width(),
              label: localization.designview.customMessage.confirmation,
              previewLabel : localization.designview.customMessage.preview,
              placeholder :  localization.designview.editConfirmation,
              disabledPlaceholder : localization.designview.editMessagePlaceholder,
              onChange: function(c) {doc.setConfirmationMessage(c);},
              onPreview : function() {
                mixpanel.track('Open confirmation preview');
                doc.save();
                doc.afterSave(function() {
                var popup = ConfirmationWithEmail.popup({
                              editText: '',
                              title: localization.designview.customMessage.confirmation,
                              mail: doc.confirmMail(),
                              onAccept: function() {
                                popup.close();
                              }
                            });
                });
              }
            }), view.confirmationmessagewrapper[0]);



            div.append(view.invitationmessagewrapper);
            div.append(view.confirmationmessagewrapper);

            return div.children();
        }
    });

});
