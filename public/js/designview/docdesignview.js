/*
 * New Design view
 *
 * Eric Normand
 */


(function(window){

    var DesignViewModel = Backbone.Model.extend({
        defaults : {
            step : undefined
        },
        initialize: function (args) {
            var self = this;
            var model = this;
            _.bindAll(model);
            args.document.bind('change:ready change:file', function() {
              if (self.document().mainfile() == undefined)
                self.setStep(undefined);
              else {
                var updateOnReady = function() {
                  if (self.document().mainfile().ready()) {
                     setTimeout(function() { self.setStep((self.step() || 1)); }, 800);
                  }
                };
                self.document().mainfile().bind('change', updateOnReady);
              }
            });

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
        step: function() {
            return this.get('step');
        },
        setStep: function(s) {
            this.set({step:s});
            return this;
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


var DesignViewTabsView = function(args) {
  var model = args.model
  var document = model.document();
  var participantsView = new DesignViewParticipantsView({ model : model});
  var draggablesView   = new DesignViewDraggablesView({ model : model});
  var processView = new DesignViewProcessView({ model : model });
  var tab1Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editParticipants);
  var tab2Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editDocument + ' ')
                      .append($("<span class='design-view-tab-text-optional'/>")
                         .text('(' + localization.designview.optional + ')'));
  var tab3Name = $("<div class='design-view-tab-text'/>")
                      .text(localization.designview.editSigningProcess + ' ')
                      .append($("<span class='design-view-tab-text-optional'/>")
                         .text('(' + localization.designview.optional + ')'));

  var tab1 = new Tab({
          name: tab1Name,
          pagehash : "participants",
          elems: [ $(participantsView.el)],
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '1'
                    });
          },
          onHide : function() {
               participantsView.closeAllParticipants();
          }
          });
  var tab2 =  new Tab({
          name: tab2Name,
          pagehash : "placements",
          available : document.mainfile() != undefined,
          elems: [ $(draggablesView.el)],
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '2'
                    });
            }
          });
  var tab3 = new Tab({
          name: tab3Name,
          pagehash : "process",
          elems: [ $(processView.el)],
          onShow : function() {
              processView.setupTinyMCE();
            },
          onActivate : function() {
               mixpanel.track('Click tab', {
                        Action: 'Open',
                        Tab: '3'
                    });
            }
          });

  var tabs = new KontraTabs({
      numbers : false,
      tabs: [tab1,tab2,tab3],
      tabsTail : $(),
      canHaveNoActiveTab : true,
      slideEffect : true
      });
  document.bind('change:ready change:file', function() {
          tab2.setAvailable(document.mainfile() != undefined);
          if (document.ready()) {
              if (document.mainfile() == undefined)
                tabs.deactive();
              else {
                var updateOnReady = function() {
                  if (document.mainfile().ready()) {
                     setTimeout(function() { tabs.activate(tabs.activeTab() || tab1); }, 800);
                  }
                }
                document.mainfile().bind('change', updateOnReady);
              }
          }
       })
  tab2.setAvailable(document.mainfile() != undefined);
  this.el = function() { return tabs.el();}

};

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
            var viewmodel = view.model;

            var div = $('<div />');
            div.addClass('design-view-button1');
            div.append($('<div />')
                       .addClass('design-view-button1-text')
                       .append(localization.saveAsDraft));

            div.click(function() {
                mixpanel.track('Click save as draft');
                viewmodel.document().save(function() {
                  new Submit({
                                          ajax : 'true',
                                          method : 'POST',
                                          url : '/d/save/' + viewmodel.document().documentid(),
                                          ajaxsuccess : function() {
                                              new FlashMessage({color: "green", content : localization.designview.saved});
                                          }
                                        }).send();
                });
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
                       .append(localization.saveAsTemplate));

            div.click(function() {
                viewmodel.document().makeTemplate();
                viewmodel.document().save();
                mixpanel.track('Click save as template');
            });

            return div;
        },
        send: function() {
            var view = this;

            this.sendButton = $('<div />');
            this.sendButton .addClass('design-view-button3');
            this.sendButton .append($('<div />')
                       .addClass('design-view-button3-text')
                       .append(localization.designview.startSigning));
            this.updateSaveButton();
            return this.sendButton ;
        },
        updateSaveButton : function() {
           if (this.sendButton != undefined) {
             if (this.model.document().hasProblems(true)) {
              this.sendButton.removeClass("active");
              this.sendButton.unbind('click');
             } else  {
              this.sendButton.addClass("active");
              this.sendButton.unbind('click').click(this.finalClick);
             }
           }
        },
        removeDocumentButton: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');
            div.addClass('design-view-button-remove');
            div.append(view.removeDocumentButtonLabel());
            div.click(function() {
                mixpanel.track('Click remove file');
                doc.setFlux();
                doc.removeTypeSetters();
                doc.save();
                doc.afterSave( function() {
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

            });
            return div;
        },
        removeDocumentButtonLabel: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-button-remove-label');
            div.append(localization.designview.removeThisDocument);
            return div;

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

            // putting this here makes problems start showing up
            // (meaning red border). The idea is that they have
            // shown the intention of trying to send the doc.
            // Now we want to help them complete this.
            // why not save the document before we validate it?
            document.save();

            if(!view.verificationBeforeSendingOrSigning(isSigning)) {
                view.performValidationActions(isSigning);
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
        performValidationActions: function(forSigning) {
            var view = this;
            var model = view.model;
            var doc = model.document();
            // we only flash the first error
            if(!doc.mainfile()) {
                mixpanel.track('Error',
                               {Message: 'no document'});
                new FlashMessage({color: 'red', content : localization.designview.validation.fileMustBeAdded});
                return;
            } else if(!doc.hasAtLeastOneSignatory()) {
                mixpanel.track('Error',
                               {Message: 'nobody signs'});
                new FlashMessage({color: 'red', content : localization.designview.validation.atLeastOnePersonMustSigns});
                model.setStep(1);
            } else if(doc.hasSignatoryProblems(forSigning)) {
                var s, f, sigs = doc.signatories(), fields;
                for(s=0;s<sigs.length;s++) {
                    var sig = sigs[s];
                    var fields = sig.fields();
                    for(f=0; f<fields.length;f++) {
                        var field = fields[f];
                          if(!field.doValidate(forSigning, function(text, object, validation) {
                            mixpanel.track('Error',
                                           {Field: field.name(),
                                            Value: field.value(),
                                            Message: validation.message()});
                            new FlashMessage({color: 'red', content : validation.message()});
                            model.setStep(1);
                            model.setParticipantDetail(sig);
                        }))
                            return;
                    }
                }
            }

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
                var callback = function(submit) {
                    document.afterSave(function(){
                        submit.sendAjax(function(resp) {
                            var link = JSON.parse(resp).link;
                            window.location = link;
                        });
                    });
                };
                bankid.click(function() {
                    if (alreadyClicked(acceptButton))
                        return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'BankID'
                    });
                    document.takeSigningScreenshot(function() { Eleg.bankidSign(document,signatory, document.signByAuthor(),callback); });
                    return false;
                });
                telia.click(function() {
                    if (alreadyClicked(acceptButton))
                        return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Telia'
                    });
                    document.takeSigningScreenshot(function() { Eleg.teliaSign(document,signatory, document.signByAuthor(),callback); });
                    return false;
                });
                nordea.click(function() {
                    if (alreadyClicked(acceptButton))
                        return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Nordea'
                    });
                    document.takeSigningScreenshot(function() { Eleg.nordeaSign(document,signatory, document.signByAuthor(),callback); });
                    return false;
                });
                mbi.click(function() {
                    if (alreadyClicked(acceptButton))
                        return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Mobile BankID'
                    });
                    document.takeSigningScreenshot(function() { Eleg.mobileBankIDSign(document,signatory,document.signByAuthor(),callback); });
                    return false;
                });
                acceptButton.append(bankid).append(telia).append(nordea).append(mbi);
            } else {
                acceptButton = Button.init({
                    size: "tiny",
                    color : "blue",
                    shape : "rounded",
                    text : localization.designview.sign,
                    onClick : function() {
                        if (alreadyClicked(this))
                            return;
                        mixpanel.track('Click accept sign', {
                            'Button' : 'sign'
                        });
                        document.takeSigningScreenshot(function() {
                            document.afterSave(function() {
                                document.signByAuthor().sendAjax(function(resp) {
                                    var link = JSON.parse(resp).link;
                                    window.location = link;
                                });
                            });
                        });
                    }
                }).input();
            }
            var content = $("<span/>");
            if (document.authorIsOnlySignatory())
                content = $(document.process().processLocalization().signatorysignmodalcontentauthoronly);
            else if (signatory.elegAuthentication())
                content = $(document.process().processLocalization().signatorysignmodalcontentdesignvieweleg);
            else
                content = $(document.process().processLocalization().signatorysignmodalcontent);

            DocumentDataFiller.fill(document, content);
            if (signatory.elegAuthentication()) {
                var subhead = $("<h6/>").text(localization.sign.eleg.subhead);
                var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
                var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
                content = content.add($("<span/>").append(subhead).append(p));
            }
            Confirmation.popup({
                title : localization.signByAuthor.modalTitle,
                acceptButton : acceptButton,
                rejectText: localization.cancel,
                content  : content
            });
        },
        sendConfirmation : function() {
            var view = this;
            var model = view.model;
            var document = model.document();
            var signatory = document.currentSignatory();
            var box = $('<div />');
            var content = $("<p/>").append($("<span/>").append(document.process().processLocalization().confirmsendtext));
            if (!document.authorIsOnlySignatory())
                    content.append($("<span/>").text(localization.to)).append("<span class='unsignedpartynotcurrent'/>");
            content.append($("<span>?</span>"));
            box.append(DocumentDataFiller.fill(document,content));

            Confirmation.popup({
                title : document.process().processLocalization().confirmsendtitle,
                acceptButton : Button.init({
                    size: "tiny",
                    color : "green",
                    shape : "rounded",
                    text : document.process().processLocalization().sendbuttontext,
                    onClick : function() {
                        if (alreadyClicked(this))
                            return;
                        mixpanel.track('Click accept sign', {
                            'Button' : 'send'
                        });
                        LoadingDialog.open(localization.designview.messages.sendingDocument);
                        document.afterSave(function() {
                            document.sendByAuthor().sendAjax(function(resp) {
                                var link = JSON.parse(resp).link;
                                window.location = link;
                            });
                        });
                    }
                }).input(),
                rejectText: localization.cancel,
                content  : box
            });
        },
        verificationBeforeSendingOrSigning : function() {
            return !this.model.document().hasProblems(true);
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
            view.docView.css({'padding-top' : 20 + view.topBarHeight});
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
            view.docView.css({'padding-top' : 20});
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

        // Show the NJ promo. This code will go away as soon as
        // Viktor and me can figure out some sort of requirements
        // and how to control different promos in an efficient way.
        if (Cookies.get('njpromo') && Cookies.get('njpromo') == 'true') {
          Cookies.set('njpromo', 'false');

          var modal = $('<div />');
          modal.append($('<img src="/img/njpromo_fake.png" />'));

          var acceptButton = new Button.init({
            size: "big",
            color: "green",
            text: localization.promo.accept,
            onClick: function() {
              if (alreadyClicked(this))
                return false;

                mixpanel.track('Accept NJ Promo');
              $('.promomodal img').attr('src', '/img/njpromo_ok.png');
              acceptButton.hide();
              declineButton.hide();

              modal.append(new Button.init({
                size: "big",
                color: "green",
                text: "Ok!",
                onClick: function() {
                    mixpanel.track('OK NJ promo');
                  Promo.close();
                }
              }).input().css({
                'float': 'right',
                'margin': '20px 0px 20px'
              }));
            }
          }).input().css({
            'float': 'right',
            'margin': '20px 0px 20px'
          });

          var declineButton = new Button.init({
            size: "tiny",
            text: localization.promo.decline,
            onClick: function() {
              if (alreadyClicked(this))
                return false;
                mixpanel.track('Decline NJ Promo');
              Promo.close();
            }
          }).input().css({
            'margin': '46px 0px',
            'float': 'left'
          });

          modal.append(acceptButton);
          modal.append(declineButton);
          Promo.open(modal);
        }

        this.afterInsert = function() {
            view.afterInsert();
        };

    };

})(window);
