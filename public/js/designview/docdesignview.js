/*
 * New Design view
 *
 * Eric Normand
 */


(function(window){

    var DesignViewModel = Backbone.Model.extend({
        initialize: function (args) {
            var self = this;
            var model = this;
            _.bindAll(model);

            model.currentColorIndex = 0;
            model.colors = [
                '#ff3377',
                '#009999',
                '#ffd700',
                '#7908aa',
                '#53df00',
                '#990000'
            ];
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
        currentColor: function() {
            return this.colors[this.currentColorIndex % this.colors.length];
        },
        advanceColor: function() {
            this.currentColorIndex++;
            return this;
        },
        resetColor: function() {
            this.currentColorIndex = 0;
            return this;
        }
    });

  // expected model: DesignViewModel
    var DesignViewButtonBarView = Backbone.View.extend({
        className: 'design-view-button-bar',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
            view.model.document().bind('change:template change:file', view.render);
            view.model.document().bind('bubble',view.updateSaveButton);
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
                div.append(view.saveAsTemplate());
                if(model.document().mainfile())
                    div.append(view.removeDocumentButton());
            } else {
                div.append(view.saveAsDraft());
                div.append(view.saveAsTemplate());
                if(model.document().mainfile())
                    div.append(view.removeDocumentButton());
                div.append(view.send());
            }

            return div;
        },
        saveAsDraft: function() {
            var view = this;
            var model = view.model;

            var saveAsDraftButton = new Button({
                text: localization.saveAsDraft,
                color: 'blue',
                onClick: function() {
                    mixpanel.track('Click save as draft');
                    model.document().save(function() {
                      new Submit({
                        ajax : 'true',
                        method : 'POST',
                        url : '/d/save/' + view.model.document().documentid(),
                        ajaxsuccess : function() {
                          new FlashMessage({color: "green", content : localization.designview.saved});
                        }
                      }).send();
                    });
                }
            });

            return saveAsDraftButton.el();
        },
        saveAsTemplate: function() {
            var view = this;
            var model = view.model;

            var saveAsTemplateButton = new Button({
                text: localization.saveAsTemplate,
                color: 'blue',
                onClick: function() {
                    model.document().makeTemplate();
                    model.document().save();
                    mixpanel.track('Click save as template');
                }
            });

            return saveAsTemplateButton.el();
        },
        send: function() {
            var view = this;

            this.sendButton = new Button({
                text: localization.designview.startSigning,
                color: 'green',
                cssClass: 'sendButton'
            }).el();
            this.updateSaveButton();
            return this.sendButton;
        },
        updateSaveButton : function() {
           if (this.sendButton != undefined) {
             if (this.model.document().hasProblems(true)) {
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
                color: 'blue',
                onClick: function() {
                    mixpanel.track('Click remove file');
                    doc.markAsNotReady();
                    doc.removeTypeSetters();
                    doc.save();
                    doc.afterSave(function() {
                        new Submit({
                            method : "POST",
                            url :  "/api/frontend/changemainfile/" + doc.documentid(),
                            ajax: true,
                            onSend: function() {

                            },
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
          if(!this.model.document().hasProblems(true)) {
            return;
          }

          var content = $('<div class="designview-cant-sign-modal"/>');

          content.append($('<p/>').html(localization.designview.cantSignModal.info));

          var contentP2 = $('<p/>');
          contentP2.text(localization.designview.cantSignModal.emailFormatInfo);
          contentP2.append($('<br/>'));
          contentP2.append($('<div class="sample" />').text(localization.designview.cantSignModal.emailSample));
          content.append(contentP2);

          var contentP3 = $('<p/>');
          contentP3.html(localization.designview.cantSignModal.mobileFormatInfo);
          contentP3.append($('<br/>'));
          contentP3.append($('<div class="sample" />').text(localization.designview.cantSignModal.mobileSample));
          content.append(contentP3);

          Confirmation.popup({title: localization.designview.cantSignModal.title,
                              cantCancel: true,
                              cantClose: false,
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
            if(document.hasProblems(true)) {
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
        signConfirmation : function() {
            var view = this;
            var model = view.model;
            var document = model.document();
            var signatory = document.currentSignatory();
            var acceptButton;
            if (signatory.elegAuthentication()) {
                acceptButton = $("<span style='margin-top: -8px;'/>");
                var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
                var telia = $("<a href='#' class='telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
                var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
                var mbi = $("<a href='#' class='mbi'><img src='/img/mobilebankid.png' alt='Mobilt BankID' /></a>");
                var callback = function(params) {
                    document.afterSave(function(){
                      document.verifyEleg().addMany(params).sendAjax(function(resp) {
                        var resp = JSON.parse(resp);
                        if (resp.verified) {
                          document.makeReadyForSigning().add("skipauthorinvitation","YES").sendAjax(function(docdata) {
                            var newdoc = new Document(new Document({}).parse(docdata));
                            newdoc.set({"screenshots" : document.get("screenshots")}); // We need to propagate screenshots
                            newdoc.sign().addMany(params).sendAjax(function() {
                                window.location.reload();
                            });
                          });
                        }
                        else {
                          new FlashMessage({color: "red", content: "Elegitimation verification failed"});
                          if (view.confirmationpopup != undefined) view.confirmationpopup.close();
                        }
                      });

                    });
                };
                var bankidclicked = false;
                bankid.click(function() {
                    if (bankidclicked) return false; else bankidclicked = true;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'BankID'
                    });
                    document.takeSigningScreenshot(function() { Eleg.bankidSign(document,signatory, callback); });
                    return false;
                });
                var teliaclicked = false;
                telia.click(function() {
                    if (teliaclicked) return false; else teliaclicked = true;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Telia'
                    });
                    document.takeSigningScreenshot(function() { Eleg.teliaSign(document,signatory, callback); });
                    return false;
                });
                var nordeaclicked = false;
                nordea.click(function() {
                    if (nordeaclicked) return false; else nordeaclicked = true;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Nordea'
                    });
                    document.takeSigningScreenshot(function() { Eleg.nordeaSign(document,signatory,callback); });
                    return false;
                });
                var mbiclicked = false;
                mbi.click(function() {
                    if (mbiclicked) return false; else mbiclicked = true;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Mobile BankID'
                    });
                    document.takeSigningScreenshot(function() { Eleg.mobileBankIDSign(document,signatory,callback); });
                    return false;
                });
                acceptButton.append(bankid).append(telia).append(nordea).append(mbi);
            } else {
                acceptButton = new Button({
                    color : "green",
                    shape : "rounded",
                    text : localization.designview.sign,
                    oneClick : true,
                    onClick : function() {
                        mixpanel.track('Click accept sign', {
                            'Button' : 'sign'
                        });
                        document.takeSigningScreenshot(function() {
                            document.afterSave(function() {
                                view.signWithCSV(document, 1 , document.isCsv() ? document.csv().length - 1 : undefined);
                            });
                        });
                    }
                }).el();
            }
            var content = $("<span/>");
            if (document.authorIsOnlySignatory())
                content = $(localization.process.signatorysignmodalcontentauthoronly);
            else if (signatory.elegAuthentication())
                content = $(localization.process.signatorysignmodalcontentdesignvieweleg);
            else
                content = $(localization.process.signatorysignmodalcontent);

            DocumentDataFiller.fill(document, content);
            if (signatory.elegAuthentication()) {
                var subhead = $("<h6/>").text(localization.sign.eleg.subhead);
                var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
                var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
                content = content.add($("<span/>").append(subhead).append(p));
            }
            this.confirmationpopup = Confirmation.popup({
                title : localization.signByAuthor.modalTitle,
                icon : '/img/modal-icons/sign.png',
                acceptButton : acceptButton,
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
               content = $("<p/>").append($("<span/>").append(localization.process.confirmstartsigningtext));
              box.append(content);
            } else {
              content = $("<p/>").append($("<span/>").append(localization.process.confirmsendtext));
              if (!document.authorIsOnlySignatory())
                content.append($("<span/>").text(' ' + localization.toNoSpaces + ': ')).append("<br /><span class='unsignedpartynotcurrent'/>");
              box.append(DocumentDataFiller.fill(document,content));
            }

            Confirmation.popup({
                title : otherSignatoriesSignInPerson ? localization.process.startsigningtitle : localization.process.confirmsendtitle,
                icon: otherSignatoriesSignInPerson ? '/img/modal-icons/start-signing.png' : '/img/modal-icons/send.png',
                acceptButton : new Button({
                    color : "green",
                    shape : "rounded",
                    text : otherSignatoriesSignInPerson ? localization.process.startsigningbuttontext : localization.process.sendbuttontext,
                    oneClick : true,
                    onClick : function() {
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
            LoadingDialog.open(localization.designview.preparingDocumentFor + " " + name, index + " " + localization.designview.partOf + " " + totalCount);
            doc.save();
            doc.afterSave(function() {
              doc.makeReadyForSigning().add("skipauthorinvitation","YES").sendAjax(function(docdata) {
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
            if (!singleDocument)
            LoadingDialog.open(localization.designview.preparingDocumentFor + " " + name, index + " " + localization.designview.partOf + " " + totalCount);
            doc.save();
            doc.afterSave(function() {
            doc.makeReadyForSigning().add("skipauthorinvitation","YES").sendAjax(function(docdata) {
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
            LoadingDialog.open(localization.designview.preparingDocumentFor + " " + name, index + " " + localization.designview.partOf + " " + totalCount);
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
            if (!singleDocument)
            LoadingDialog.open(localization.designview.preparingDocumentFor + " " + name, index + " " + localization.designview.partOf + " " + totalCount);
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
            _.bindAll(view);
            view.tabsView    = new DesignViewTabsView({model : view.model});
            view.buttonBar   = new DesignViewButtonBarView({model : view.model});
            view.documentView = new DesignViewDocumentView({model : view.model.document(),
                                                            viewmodel : view.model});
            $(window).on('resize scroll', view.affix);
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

})(window);
