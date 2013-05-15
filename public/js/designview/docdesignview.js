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
        setShowProblems: function(b) {
            this.set({showProblems:b});
            return this;
        },
        showProblems: function() {
            return this.get('showProblems');
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
                        .text(localization.designview.editParticipants));
            if(model.step() === 1)
                div.addClass('tab-active');
            div.click(function() {
                model.setStep(1);
            });

            div.mouseenter(function() {
                div.addClass('tab-hover');
            });

            div.mouseleave(function() {
                div.removeClass('tab-hover');
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
                        .text(localization.designview.editDocument + ' ')
                        .append($('<span />')
                                .addClass('design-view-tab2-text-optional')
                                .text('(' + localization.designview.optional + ')')));
            if(model.step() === 2)
                div.addClass('tab-active');
            div.click(function() {
                model.setStep(2);
            });

            div.mouseenter(function() {
                div.addClass('tab-hover');
            });

            div.mouseleave(function() {
                div.removeClass('tab-hover');
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
                        .text(localization.designview.editSigningProcess + ' ')
                        .append($('<span />')
                                .addClass('design-view-tab3-text-optional')
                                .text('(' + localization.designview.optional + ')')));
            if(model.step() === 3)
                div.addClass('tab-active');
            div.click(function() {
                model.setStep(3);
            });

            div.mouseenter(function() {
                div.addClass('tab-hover');
            });

            div.mouseleave(function() {
                div.removeClass('tab-hover');
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
            view.processView = DesignViewProcessView({ model : view.model });
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
                view.$el.html(view.processView.el);
                view.processView.afterInsertion();
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
            view.model.document().bind('change:template change:file', view.render);
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
                       .append($('<img />')
                               .addClass('design-view-button1-icon')
                               .attr('src', '/img/save.png'))
                       .append(localization.saveAsDraft));

            div.click(function() {
                viewmodel.document().save();
                viewmodel.setShowProblems(false);
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
                       .append(localization.saveAsTemplate));

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
                       .append(localization.designview.startSigning));

            div.click(view.finalClick);

            return div;
        },
        removeDocumentButton: function() {
            var view = this;
            var viewmodel = view.model;
            var doc = viewmodel.document();

            var div = $('<div />');
            div.addClass('design-view-button-remove');
            div.append(view.removeDocumentButtonLabel());
            div.click(function() {
                doc.setFlux();
                doc.removePlacements();
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
        removeDocumentButtonIcon: function() {
            var view = this;

            var img = $('<img />');
            img.addClass('design-view-button-remove-icon-img');
            img.attr('src', '/img/trash.png');

            return img;
        },
        removeDocumentButtonLabel: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-button-remove-label');
            div.append(view.removeDocumentButtonIcon());
            div.append(localization.designview.removeThisDocument);
            return div;

        },
        finalClick: function() {
            var view = this;
            var model = view.model;
            var document = model.document();

            var isSigning = document.authorCanSignFirst();

            mixpanel.track('Click sign button', {
                'Is Signing' : isSigning
            });

            // putting this here makes problems start showing up
            // (meaning red border). The idea is that they have
            // shown the intention of trying to send the doc.
            // Now we want to help them complete this.
            model.setShowProblems(true);

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
            } else if(doc.hasDuplicateEmails()) {
                mixpanel.track('Error',
                               {Message: 'duplicate emails'});
                new FlashMessage({color: 'red', content : localization.designview.validation.sameMails});
                model.setStep(1);
            } else if(doc.hasSignatoryProblems()) {
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
            if (document.elegAuthentication()) {
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
                                });;
                            });
                        });
                    }
                }).input();
            }
            var content = $("<span/>");
            if (document.authorIsOnlySignatory())
                content = $(document.process().processLocalization().signatorysignmodalcontentauthoronly);
            else if (document.elegAuthentication())
                content = $(document.process().processLocalization().signatorysignmodalcontentdesignvieweleg);
            else
                content = $(document.process().processLocalization().signatorysignmodalcontent);

            DocumentDataFiller.fill(document, content);
            if (document.elegAuthentication()) {
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
            return !this.model.document().hasProblems();
        }
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
                             left: 0});
            view.docView.css({'padding-top' : 20 + view.topBarHeight});
        },
        unfix: function() {
            var view = this;
            if(!view.fixed)
                return;
            view.fixed = false;
            view.topBarHeight = view.topBar.outerHeight();
            view.topBar.css({position:'relative',
                             top: 'none',
                             left: 'none'});
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

            if(st > top) {
                view.fix();
            } else {
                view.unfix();
            }
        },
        frame: function() {
            var view = this;
            var topBar = $('<div />');
            topBar.addClass('design-view-frame-top-bar');
            topBar.append(view.tabsView.el);
            topBar.append(view.actionsView.el);
            view.topBar = topBar;
            var div = $('<div/>');
            div.append(topBar);
            //div.append(view.tabsView.el);
            //div.append(view.actionsView.el);
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

    }

})(window);
