/*
 * New Design view
 *
 * Eric Normand
 */

define(['Spinjs', 'Backbone', 'legacy_code'], function(Spinner) {

    var DesignViewModel = Backbone.Model.extend({
        initialize: function (args) {
            var self = this;
            var model = this;
            _.bindAll(this, 'document', 'ready', 'participantDetail', 'setParticipantDetail', 'saveDocument', 'saveAndFlashMessageIfAlreadySaved', 'saveFlashMessage');
        },
        document : function() {
            return this.get("document");
        },
        ready : function() {
            return this.document().ready();
        },
        participantDetail: function() {
            return this.get('participantDetail');
        },
        setParticipantDetail: function(s) {
            this.set({participantDetail : s});
            return this;
        },
        saveDocument: function() {
            var viewmodel = this;
            var document = this.document();

            var wasSaved = document.saved();
            document.setSaved();
            document.save(function () {
              viewmodel.saveFlashMessage(wasSaved);
            });
        },
        saveAndFlashMessageIfAlreadySaved: function() {
            var self = this;
            var isSaved = self.document().saved();
            self.document().save(function () {
              if(isSaved) {
                self.saveFlashMessage(true);
              }
            });
        },
        saveFlashMessage: function(wasSaved) {
            var flashMsg;
            if(this.document().isTemplate()) {
              if(wasSaved) {
                flashMsg = localization.designview.saved.saveTemplate;
              } else {
                flashMsg = localization.designview.saved.savedAsTemplate;
              }
            }
            else {
              if(wasSaved) {
                flashMsg = localization.designview.saved.saveDraft;
              } else {
                flashMsg = localization.designview.saved.savedAsDraft;
              }
            }
            new FlashMessage({type: 'success', content: flashMsg});
        }
    });

  // expected model: DesignViewModel
    var DesignViewButtonBarView = Backbone.View.extend({
        className: 'design-view-button-bar',
        initialize: function(args) {
            var view = this;
            _.bindAll(this, 'render', 'inner', 'makeSaveDraftButton', 'saveDraftButtonText', 'makeSaveTemplateButton', 'saveTemplateButtonText', 'updateSaveButtons', 'send', 'updateSendButton', 'removeDocumentButton', 'cantSignModal', 'finalClick', 'spinnerSmall', 'signConfirmation', 'sendConfirmation', 'signWithCSV', 'sendWithCSV');
            view.render();
            view.model.document().bind('change:template change:file', view.render);
            view.model.document().bind('bubble',view.updateSendButton);
            view.model.document().bind('change', view.updateSaveButtons);
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


            if(model.document().isTemplate()) {
                div.append(view.makeSaveTemplateButton().el());
                if(model.document().mainfile())
                    div.append(view.removeDocumentButton());
            } else {
                div.append(view.makeSaveDraftButton().el());
                div.append(view.makeSaveTemplateButton().el());
                if(model.document().mainfile())
                    div.append(view.removeDocumentButton());
                div.append(view.send());
            }

            return div;
        },
        makeSaveDraftButton: function() {
          var view = this;
          this.saveDraftButton = new Button({
            text: view.saveDraftButtonText(),
            color: 'main',
            cssClass: 'button-save-draft',
            onClick: function(e) {
              mixpanel.track('Click save as draft');
              view.model.saveDocument();
              view.updateSaveButtons();
            }
          });
          return this.saveDraftButton;
        },
        saveDraftButtonText : function() {
          if(this.model.document().saved()) {
            return localization.designview.saveDraftButton;
          }
          return localization.designview.saveAsDraftButton;
        },
        makeSaveTemplateButton: function() {
          var view = this;
          this.saveTemplateButton = new Button({
            text: view.saveTemplateButtonText(),
            color: 'main',
            onClick: function(e) {
              mixpanel.track('Click save as template');
              view.model.document().makeTemplate();
              view.model.saveDocument();
              view.updateSaveButtons();
            }
          });
          return this.saveTemplateButton;
        },
        saveTemplateButtonText : function() {
          var document = this.model.document();
          if(document.isTemplate() && document.saved()) {
            return localization.designview.saveTemplateButton;
          }
          return localization.designview.saveAsTemplateButton;
        },
        updateSaveButtons : function() {
          if(this.saveDraftButton != undefined) {
            this.saveDraftButton.setText(this.saveDraftButtonText());
          }
          if(this.saveTemplateButton != undefined) {
            this.saveTemplateButton.setText(this.saveTemplateButtonText());
          }
        },
        send: function() {
            var view = this;

            this.sendButton = new Button({
                text: localization.designview.startSigning,
                cssClass: 'sendButton'
            }).el();
            this.updateSendButton();
            return this.sendButton;
        },
        updateSendButton : function() {
           if (this.sendButton != undefined) {
             if (this.model.document().hasProblems() || !this.model.document().ready()) {
              this.sendButton.addClass('disabled');
              this.sendButton.unbind('click').click(this.cantSignModal);
             } else  {
              this.sendButton.removeClass("disabled");
              this.sendButton.unbind('click').click(this.finalClick);
             }
           }
        },
        removeDocumentButton: function() {
            var view = this;
            var model = view.model;
            var doc = model.document();

            var removeDocumentButton = new Button({
                text: localization.designview.removeThisDocument,
                onClick: function() {
                    mixpanel.track('Click remove file');
                    doc.markAsNotReady();
                    doc.removeTypeSetters();
                    model.saveAndFlashMessageIfAlreadySaved();
                    doc.afterSave(function() {
                        new Submit({
                            method : "POST",
                            url :  "/api/frontend/changemainfile/" + doc.documentid(),
                            ajax: true,
                            onSend: function() {},
                            ajaxerror: function(d,a){
                                doc.recall();
                            },
                            ajaxsuccess: function() {
                                doc.recall();
                            }}).send();
                    });
                }
            });

            return removeDocumentButton.el();
        },
        cantSignModal: function() {
          if(!this.model.document().hasProblems()) {
            return;
          }

          var content = $('<div class="designview-cant-sign-modal"/>');

          content.append($('<p class="paragraph"/>').text(localization.designview.cantSignModal.info1));

          var contentP2 = $('<p class="paragraph"/>');
          contentP2.html(localization.designview.cantSignModal.info2);
          content.append(contentP2);

          var contentP3 = $('<ul class="unordered-list"/>');
          contentP3.append($("<li>").text(localization.designview.cantSignModal.li1));
          contentP3.append($("<li>").text(localization.designview.cantSignModal.li2));
          contentP3.append($("<li>").text(localization.designview.cantSignModal.li3));
          contentP3.append($("<li>").text(localization.designview.cantSignModal.li4));
          content.append(contentP3);

          new Confirmation({title: localization.designview.cantSignModal.title,
                              cancelVisible: false,
                              content: content});
        },
        finalClick: function() {
            var view = this;
            var model = view.model;
            var document = model.document();

            var isSigning = document.authorCanSignFirst();

            mixpanel.track('Click sign button', {
                'Is Signing' : isSigning,
                'Uses eleg' : document.hasEleg(),
                'Uses email delivery' : document.hasEmail(),
                'Uses mobile delivery' : document.hasSMS(),
                'Uses pad delivery' : document.hasPad(),
                'Uses email and mobile delivery' : document.hasEmailAndSMS()
            });

            document.save();
            if(document.hasProblems()) {
                return;
            }

            if(BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
                mixpanel.track('Open blocking popup');
                mixpanel.people.set({
                    'Blocking Popup': new Date()
                });

                BlockingInfo.createPopup();
                return false;
            }

            if(isSigning)
                view.signConfirmation();
            else
                view.sendConfirmation();
        },
        spinnerSmall : function() {
            return new Spinner({
                lines  : 9,     // The number of lines to draw
                length : 3,     // The length of each line
                width  : 2,     // The line thickness
                radius : 5,     // The radius of the inner circle
                color  : '#000000', // #rbg or #rrggbb
                speed  : 1.5,    // Rounds per second
                trail  : 74,     // Afterglow percentage
                shadow : false   // Whether to render a shadow
            }).spin();
        },
        signConfirmation : function() {
            var self = this;
            var model = self.model;
            var document = model.document();
            var signatory = document.currentSignatory();

            var spinner = this.spinnerSmall();
            var spinnerContainer = $("<span class='spinner-container' />");
            spinnerContainer.append(spinner.el);

            var acceptButton = new Button({
              type : "action",
              text : localization.designview.sign,
              oneClick : true,
              onClick : function() {
                acceptButton.el().addClass('is-inactive').prepend(spinnerContainer); // Add the spinner and make inactive
                self.confirmationpopup.hideCancel();
                self.confirmationpopup.hideClose();

                mixpanel.track('Click accept sign', {
                 'Button' : 'sign'
                });
                document.takeSigningScreenshot(function() {
                  document.afterSave(function() {
                    self.signWithCSV(document, 1 , document.isCsv() ? document.csv().length - 1 : undefined);
                  });
                });
             }
            });

            var content = $("<span/>");
            if (document.authorIsOnlySignatory()) {
                if (document.author().name().trim() !== '') {
                  content = $(localization.designview.signModalContentAuthorOnlyWithName);
                  content.find('.put-signatory-name-here').text(document.author().name());
                } else {
                  content = $(localization.designview.signModalContentAuthorOnly);
                }
                content.find('.put-document-title-here').text(document.title());
            }
            else if (signatory.elegAuthentication()) {
                content = $(localization.designview.signModalContentEleg);
            }
            else {
                var parties = _.filter(document.signatories(), function(signatory) {
                                      return signatory.signs() && !signatory.current();
                               });
                if (document.author().name().trim() !== '') {
                  content = $(localization.designview.signModalContentWithName);
                  content.find('.put-signatory-name-here').text(document.author().name());
                } else {
                  content = $(localization.designview.signModalContent);
                }
                content.find('.put-document-title-here').text(document.title());

            }

            self.confirmationpopup = new Confirmation({
                title : localization.signByAuthor.modalTitle,
                acceptButton : acceptButton.el(),
                rejectText: localization.cancel,
                content  : content
            });

            // Let the system know that the sign confirmation is visible.
            model.trigger('visibility:signconfirmation');
        },
        sendConfirmation : function() {
            var view = this;
            var model = view.model;
            var document = model.document();
            var signatory = document.currentSignatory();
            var box = $('<div class="send-modal-body"/>');

            var otherSignatoriesSignInPerson = _.every(document.signatories(), function(sig) {
              return sig.padDelivery() || sig.author();
            });

            var content;
            if (otherSignatoriesSignInPerson) {
              content = $("<p/>").append($("<span/>").append(localization.designview.startSigningModalTextWithPad));
            } else if (document.authorIsOnlySignatory()) {
              content = $("<p/>").append($("<span/>").append(localization.designview.startSigningModalText));
            } else {
              content = $("<p/>").append($("<span/>").append(localization.designview.startSigningModalTextWithSendout));
              var parties = _.filter(document.signatories(), function(signatory) {
                                      return signatory.signs() && !signatory.current();
                               });
              var partiesNames = _.map(parties, function(signatory) {
                                      return signatory.nameForLists();
                               });
              content.find('.put-names-of-parties-here').append(buildBoldList(partiesNames));
            }
            box.append(content);

            var spinner = this.spinnerSmall();
            var spinnerContainer = $("<span class='spinner-container' />");
            spinnerContainer.append(spinner.el);

            var confirmation = new Confirmation({
                title : otherSignatoriesSignInPerson ? localization.process.startsigningtitle : localization.process.confirmsendtitle,
                acceptButton : new Button({
                    type : "action",
                    text : otherSignatoriesSignInPerson ? localization.process.startsigningbuttontext : localization.process.sendbuttontext,
                    oneClick : true,
                    onClick : function() {

                        confirmation.acceptButton().addClass('is-inactive').prepend(spinnerContainer);  // Add the spinner and make inactive
                        confirmation.hideCancel();
                        confirmation.hideClose();

                        mixpanel.track('Click accept sign', {
                            'Button' : 'send'
                        });
                        document.takeSigningScreenshot(function() {
                               view.sendWithCSV(document, 1, document.isCsv() ? document.csv().length - 1 : undefined);
                        });
                    }
                }).el(),
                rejectText: localization.cancel,
                content  : box
            });
    },
    signWithCSV : function(doc,index, totalCount) {
      var self = this;
      if (doc.csv() != undefined && doc.csv().length > 2) {
        doc.clone(function(doc2) {
            var name = doc.normalizeWithFirstCSVLine();
            var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
            $('.put-person-name',copyDocFor).text(name);
            var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
            $('.put-current-index',copyPartOf).text(index);
            $('.put-total-count',copyPartOf).text(totalCount);
            LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
            doc.save();
            doc.afterSave(function() {
              doc.makeReadyForSigning().add("authorsignsimmediately","YES").sendAjax(function(docdata) {
                  var newdoc = new Document(new Document({}).parse(docdata));
                  newdoc.set({"screenshots" : doc.get("screenshots")}); // We need to propagate screenshots
                  newdoc.sign().sendAjax(
                    function() {
                      doc2.dropFirstCSVLine();
                      doc2.save();
                      doc2.afterSave(function() {
                          self.signWithCSV(doc2,index + 1, totalCount);
                      });
                   });
              });
            });
        }).sendAjax();
      } else {
            var singleDocument = !doc.isCsv();
            var name = doc.normalizeWithFirstCSVLine();
            if (!singleDocument) {
              var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
              $('.put-person-name',copyDocFor).text(name);
              var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
              $('.put-current-index',copyPartOf).text(index);
              $('.put-total-count',copyPartOf).text(totalCount);
              LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
            }
            doc.save();
            doc.afterSave(function() {
            doc.makeReadyForSigning().add("authorsignsimmediately","YES").sendAjax(function(docdata) {
               var newdoc = new Document(new Document({}).parse(docdata));
               newdoc.set({"screenshots" : doc.get("screenshots")}); // We need to propagate screenshots
               newdoc.sign().sendAjax(function() {
                  window.location = "/d" + (singleDocument ? "/" + doc.documentid() : "");
                });
              });
            });
     }
  },
  sendWithCSV : function(doc,index, totalCount) {
      var self = this;
      if (doc.csv() != undefined && doc.csv().length > 2) {
        doc.clone(function(doc2) {
            var name = doc.normalizeWithFirstCSVLine();
            var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
            $('.put-person-name',copyDocFor).text(name);
            var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
            $('.put-current-index',copyPartOf).text(index);
            $('.put-total-count',copyPartOf).text(totalCount);
            LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
            doc.save();
            doc.afterSave(function() {
              doc.makeReadyForSigning().sendAjax(function() {
                  doc2.dropFirstCSVLine();
                  doc2.save();
                  doc2.afterSave(function() {
                      self.sendWithCSV(doc2, index + 1, totalCount);
                  });
              });
            });
        }).sendAjax();
      } else {
            var singleDocument = !doc.isCsv();
            var name = doc.normalizeWithFirstCSVLine();
            if (!singleDocument) {
              var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
              $('.put-person-name',copyDocFor).text(name);
              var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
              $('.put-current-index',copyPartOf).text(index);
              $('.put-total-count',copyPartOf).text(totalCount);
              LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
            }
            doc.save();
            doc.afterSave(function() {
            doc.makeReadyForSigning().sendAjax(function() {
                 window.location = "/d" + (singleDocument ? "/" + doc.documentid() : "");
              });
            });
     }
  }

});

    var DesignViewView = Backbone.View.extend({
        className: 'design-view-frame',
        initialize: function (args) {
            var view = this;
            _.bindAll(this, 'fix', 'unfix', 'affix', 'frame', 'render', 'afterInsert');
            view.tabsView    = new DesignViewTabsView({model : view.model});
            view.buttonBar   = new DesignViewButtonBarView({model : view.model});
            view.documentView = new DesignViewDocumentView({model : view.model.document(),
                                                            viewmodel : view.model});
            $(window).on('resize scroll', view.affix);
            view.model.document().setReferenceScreenshot("author");
            view.render();
        },
        fix: function() {
            var view = this;
            if(view.fixed)
                return;
            view.fixed = true;
            view.topBarHeight = view.topBar.outerHeight();
            view.topBar.css({position:'fixed',
                             top: 0,
                             left: $(view.topBar).offset().left});
        },
        unfix: function() {
            var view = this;
            if(!view.fixed)
                return;
            view.fixed = false;
            view.topBarHeight = view.topBar.outerHeight();
            view.topBar.css({position:'relative',
                             top: '',
                             left: ''});
        },
        affix: function() {
            var view = this;
            var model = view.model;

            var st = $(window).scrollTop();
            var docTop = view.docView.offset().top + view.topBarHeight;
            var top = view.$el.offset().top;
            var barHeight = view.topBar.outerHeight();
            var barTop = view.topBar.offset().top;
            if(st > top && st + barHeight < docTop) {
                view.unfix();
            } if(st > top) {
                view.fix();
            } else {
                view.unfix();
            }
        },
        frame: function() {
            var view = this;
            view.topBar = $("<div class='design-view-frame-top-bar'/>");
            view.topBar.append(view.tabsView.el());
            var div = $('<div/>');
            div.append(view.topBar);
            div.append(view.documentView.el);
            div.append(view.buttonBar.el);

            view.docView = view.documentView.$el;

            return div.children();
        },
        render: function() {
            var view = this;
            view.$el.html(view.frame());
            return view;
        },
        afterInsert: function() {
            var view = this;
            view.documentView.afterInsert();
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

        document.recall();

        this.el = function() {
            return $(view.el);
        };
        this.afterInsert = function() {
            view.afterInsert();
        };
        this.model = function() {
            return model;
        };
    };

});
