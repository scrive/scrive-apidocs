/*
 * Participants tab of design view.
 *
 */

(function(window){
    // model is Signatory
    var DesignViewNewFieldSelector = Backbone.View.extend({
        className: 'design-view-action-participant-new-field-selector',
        initialize: function() {
            var view = this;
            _.bindAll(view);
            view.bind('refresh', view.refresh);
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            view.$el.html(view.createNewButton());

            return view;
        },
        createNewButton: function() {
            var view = this;
            var sig = view.model;

            var button = Button.init({
                color: 'black',
                size: 'tiny',
                text: '+ ' + localization.designview.addField,
                onClick: function() {
                    view.popup();
                }
            });

            return button.input();
        },
        reset: function() {
            this.selected = null;
            this.customName = null;
        },
        popup: function() {
            var view = this;
            var sig = view.model;
            view.reset();
            view.confirmation = Confirmation.popup({
                title: localization.designview.addField,
                acceptText: localization.save,
                content: view.popupContent(),
                onAccept: function() {
                    view.acceptPopup();
                    view.confirmation.close();
                }
            });
        },
        acceptPopup: function() {
            var view = this;
            var sig = view.model;

            if(view.selected) {
                var name = view.selected === '--custom' ? view.customName : view.selected;
                var type = view.standardPlaceholders[view.selected] ? 'standard' : 'custom';

                var field = new Field({
                    name: name,
                    type: type,
                    signatory: sig,
                    obligatory: false
                });

                sig.addField(field);
            }
        },
        popupContent: function() {
            var view = this;

            if(!view.content) {
                var div = $('<div />');
                div.addClass('design-view-action-participant-new-field-popup-content');
                view.content = div;
            }

            view.refresh();

            return view.content;
        },
        refresh: function() {
            var view = this;

            var div = view.content;

            var top = $('<div />');
            top.addClass('design-view-action-participant-new-field-popup-content-top');
            top.html(view.selectBox());

            var bottom = $('<div />');
            bottom.addClass('design-view-action-participant-new-field-popup-content-bottom');
            bottom.html(view.customField());

            div.html(top);
            div.append(bottom);

            return div;
        },
        selectBox: function() {
            var view = this;
            var sig = view.model;

            var options = [];

            _.each(view.standardFields, function(f) {
                if(!sig.field(f[0], f[1]))
                    options.push({
                        name: view.placeholder(f[0]),
                        value: f[0]
                    });
            });

            options.push({
                name: localization.designview.customField,
                value: '--custom'
            });

            var name;

            if(!view.selected)
                name = localization.designview.addField;
            else if(view.selected === '--custom')
                name = localization.designview.customField;
            else
                name = view.placeholder(view.selected);

            var select = new Select({
                options: options,
                name: name,
                onSelect: function(v) {
                    view.selected = v;
                    view.trigger('refresh');
                }
            });

            return select.view().el;
        },
        customField: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');

            if(view.selected === '--custom') {
                var title = $('<div />');
                title.addClass('design-view-action-participant-new-field-popup-content-bottom-title');

                var input = InfoTextInput.init({
                    cssClass: 'design-view-action-participant-new-field-popup-content-bottom-input',
                    infotext: localization.designview.fieldName,
                    value: '',
                    onChange: function() {
                        view.customName = input.value();
                    },
                    onEnter: function() {
                        view.acceptPopup();
                        view.confirmation.close();
                    }
                });

                div.append(title);
                div.append(input.input());
            }

            return div.children();
        },
        standardFields: [
            ["sigpersnr", "standard"],
            ["sigcompnr", "standard"],
            ["sigco",     "standard"],
            ["mobile",    "standard"]
        ],
        standardPlaceholders: {
            sigcompnr: localization.companyNumber,
            sigpersnr: localization.personamNumber,
            sigco: localization.company,
            mobile: localization.phone
        },
        placeholder: function(name) {
            return this.standardPlaceholders[name] || name;
        }
    });

    var DesignViewParticipation = Backbone.View.extend({
        className: 'design-view-action-participant-details-participation',
        initialize: function() {
            var view = this;
            _.bindAll(view);
            view.model.bind('change', view.render);
            // if the document's signorder changes, we rerender in order
            // to get reset the sign order select box
            view.model.document().bind('change:signorder', view.render);
            view.render();
        },
        destroy : function() {
          this.model.unbind('change', this.render);
          this.model.document().unbind('change:signorder', this.render);
          this.off();
          this.remove();
        },
        render: function() {
            var view = this;
            var signatory = view.model;

            var div = $('<div />');

            div.append(view.detailsParticipationHeader());
            div.append(view.detailsParticipationFields());

            view.$el.html(div.children());
            return view;
        },
        detailsParticipationHeader: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-participation-header');
            div.text(localization.designview.defineParticipation);
            return div;
        },
        detailsParticipationFields: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-participation-fields');
            div.append(view.detailsParticipationFieldsSignOrder());
            div.append(view.detailsParticipationFieldsDelivery());
            div.append(view.detailsParticipationFieldsRole());
            div.append(view.detailsParticipationFieldsAuth());
            return div;
        },
        detailsParticipationFieldsSignOrder: function() {
            var view = this;
            var model = view.model;

            var i;

            var order = model.signorder();

            var options = [];
            for(i=1;i<=model.document().maxPossibleSignOrder();i++) {
                var ordinal = localization.code == 'sv' ? swedishOrdinal(i) : englishOrdinal(i);
                options.push({name: ordinal + ' ' +
                              localization.designview.toReceiveDocument,
                              value: i});
            }

            var ordinal = localization.code == 'sv' ? swedishOrdinal(order) : englishOrdinal(order);
            var select = new Select({
                options: options,
                name: ordinal + ' ' +
                    localization.designview.toReceiveDocument,
                onSelect: function(v) {
                    model.setSignOrder(v);
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-order');

            return select.view().el;
        },
        detailsParticipationFieldsDelivery: function() {
            var view = this;
            var sig = view.model;
            var delivery = sig.delivery();

            var deliveryTexts = {
                email : localization.designview.byEmail,
                pad : localization.designview.onThisTablet,
                mobile : localization.designview.bySMS,
                email_mobile : localization.designview.byEmailAndSMS
            };

            var deliveryTypes = ['email', 'pad', 'mobile', 'email_mobile'];

            var select = new Select({
                options: _.map(deliveryTypes, function(t) {
                    return {name: deliveryTexts[t], value:t};
                }),
                name: deliveryTexts[delivery],
                onSelect: function(v) {
                    sig.setDelivery(v);
                    sig.ensureMobile();
                    sig.ensureSignature();
                    sig.ensureEmail();
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-delivery');
            return select.view().el;
        },
        detailsParticipationFieldsRole: function() {
            var view = this;
            var sig = view.model;
            var role = sig.signs() ? 'signatory' : 'viewer';

            var roleTexts = {
                'signatory' : localization.designview.forSigning,
                'viewer'    : localization.designview.forViewing
            };

            var roleTypes = ['signatory', 'viewer'];

            var select = new Select({
                options: _.map(roleTypes, function(t) {
                    return {name: roleTexts[t], value:t};
                }),
                name: roleTexts[role],
                onSelect: function(v) {
                    if(v === 'signatory')
                        sig.makeSignatory();
                    else if(v === 'viewer')
                        sig.makeViewer();
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-role');
            return select.view().el;
        },
        detailsParticipationFieldsAuth: function() {
            var view = this;
            var sig = view.model;
            var auth = sig.authentication();

            var authTexts = {
                standard : localization.designview.noIDControl,
                eleg : localization.designview.withEleg
            };

            var authTypes = ['standard', 'eleg'];

            var select = new Select({
                options: _.map(authTypes, function(t) {
                    return {name: authTexts[t], value:t};
                }),
                name: authTexts[auth],
                onSelect: function(v) {
                    sig.setAuthentication(v);
                    sig.ensurePersNr();
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-auth');
            return select.view().el;
        }
    });

    var DesignViewParticipantsNewView = Backbone.View.extend({
        className: 'design-view-action-participant-new-box',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
        },
        render: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-participant-new-box');
            div.append(view.newButtons());

            view.$el.html(div.children());
            return view;
        },
        newButtons: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-new-box-buttons');
            div.append(view.multi());
            div.append(view.single());
            return div;
        },
        single: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-new-single');

            var button = Button.init({
                color: 'green',
                size: 'tiny',
                text: '+ ' + localization.designview.addParty,
                onClick: function() {
                    model.setParticipantDetail(null);

                    var doc = model.document();
                    var sig = new Signatory({
                        document:doc,
                        signs:true
                    });
                    doc.addExistingSignatory(sig);
                    model.setParticipantDetail(sig);
                    sig.ensureEmail();
                    return false;
                }
            });

            div.append(button.input());

            return div;
        },
        multi: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-new-multi');

            var button = Button.init({
                color: 'black',
                size: 'tiny',
                text: '+ ' + localization.designview.addMultisend,
                onClick: function() {
                    model.setParticipantDetail(null);

                    var doc = model.document();
                    var sig = new Signatory({
                        document:doc,
                        signs:true
                    });

                    sig.giveStandardFields();

                    CsvSignatoryDesignPopup.popup({
                        signatory: sig,
                        onAccept: function() {
                            doc.addExistingSignatory(sig);
                            doc.save();
                            model.setParticipantDetail(sig);
                        }
                    });
                    return false;
                }
            });

            div.append(button.input());

            return div;
        }
    });

    var DesignViewParticipantsView = Backbone.View.extend({
        className: 'design-view-action-participant-container',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.addNew = new DesignViewParticipantsNewView(args);
            view.reset();
            // any changes to the signatories of a document and we rerender
            // this includes adding and removing signatories
            view.model.document().bind('change:signatories', view.reset);
        },
        reset: function() {
            var view = this;
            if (view.participants != undefined) _.each(view.participants,function(pv) {pv.destroy();});
            view.setup();
            view.render();
        },
        setup: function() {
            var view = this;
            var model = view.model;

            view.participants = [];

            var doc = model.document();
            var sigs = doc.signatories();

            model.resetColor();

            $.each(model.document().signatories(), function(i, s) {
                s.setColor(model.currentColor());
                model.advanceColor();
                view.participants.push(new DesignViewParticipantView({model  : s,
                                                                      number : i,
                                                                      viewmodel : model
                                                                     }));
            });
            return view;
        },
        render: function() {
            var view = this;
            var model = view.model;
            view.$el.children().detach();
            var box = $("<div class='design-view-action-participant-container-participants-box'>");
            //box.css("max-height",($(window).height() - 460) + "px");
            $.each(view.participants, function(i, p) {
               box.append(p.el);
            });

            view.$el.append(box).append(view.addNew.el);

            return view;
        }
    });

    // order icon + order selection
    var DesignViewOrderIconView = Backbone.View.extend({
        className: 'design-view-action-participant-icon-order',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.model.bind('change:signorder', view.render);
            view.render();
            view.$el.click(function() {
                var sig = view.model;
                var max = sig.document().maxPossibleSignOrder();
                var o = sig.signorder() + 1;
                if(o > max)
                    o = 1;
                sig.setSignOrder(o);
                return false;
            });
        },
        destroy : function() {
          this.model.unbind('change:signorder', this.render);
          this.off();
          this.remove();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-order-inner')
                .text(sig.signorder());

            view.$el.html(div);

            return view;
        }
    });

    // role icon + role selection
    var DesignViewRoleIconView = Backbone.View.extend({
        className: 'design-view-action-participant-icon-role',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.model.bind('change:role', view.render);
            view.render();
            view.$el.click(function() {
                if(view.model.role() === 'viewer')
                    view.model.makeSignatory();
                else
                    view.model.makeViewer();
                return false;
            });

        },
        icons: {
            viewer: '/img/viewer.png',
            signatory: '/img/signatory.png'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var role = sig.role();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-role-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-role-icon')
                        .attr('src', view.icons[role]));

            view.$el.html(div);

            return view;
        }
    });

    // delivery icon + delivery selection
    var DesignViewDeviceIconView = Backbone.View.extend({
        className: 'design-view-action-participant-icon-device',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.model.bind('change:delivery', view.render);
            view.render();
            view.$el.click(function() {
                if(view.model.delivery() === 'email')
                    view.model.setDelivery('pad');
                else if(view.model.delivery() === 'pad')
                    view.model.setDelivery('mobile');
                else if(view.model.delivery() === 'mobile')
                    view.model.setDelivery('email_mobile');
                else
                    view.model.setDelivery('email');

                view.model.ensureMobile();

                return false;
            });
        },
        icons: {
            email: '/img/email.png',
            pad: '/img/pad2.png',
            api: '/img/pad2.png',
            mobile: '/img/phone2.png',
            email_mobile : '/img/email_mobile.png'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var delivery = sig.delivery();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-device-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-device-icon')
                        .attr('src', view.icons[delivery]));
            view.$el.html(div);

            return view;
        }
    });

    // auth icon + auth selection
    var DesignViewAuthIconView = Backbone.View.extend({
        className: 'design-view-action-participant-icon-auth',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.model.bind('change:authentication', view.render);
            view.render();
            view.$el.click(function() {
                var sig = view.model;
                var auth = sig.authentication()
                if(auth === 'standard')
                    sig.setAuthentication('eleg');
                else
                    sig.setAuthentication('standard');
                sig.ensurePersNr();
                return false;
            });
        },
        icons: {
            standard: '/img/noauth.png',
            eleg: '/img/eleg.png'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var auth = sig.authentication();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-auth-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-auth-icon')
                        .attr('src', view.icons[auth]));

            view.$el.html(div);

            return view;
        }
    });

    /* model::Signatory
       viewmodel::DesignViewModel
    */
    var DesignViewParticipantDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.viewmodel = args.viewmodel;
            view.participation = new DesignViewParticipation({model:view.model});
            view.newFieldSelector = new DesignViewNewFieldSelector({model:view.model});
            view.viewmodel.bind('change:showProblems', view.render);
            view.model.bind('change:fields', view.render);
            view.model.bind('change:authentication', view.render);
            view.model.bind('change:delivery', view.render);
            view.render();
        },
        destroy : function() {
          this.viewmodel.unbind('change:showProblems', this.render);
          this.model.unbind('change:fields', this.render);
          this.model.unbind('change:authentication', this.render);
          this.model.unbind('change:delivery', this.render);
          this.off();
          if (this.participation != undefined) this.participation.destroy();
          this.remove();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            if(!sig.isRemoved) {
                var div = $('<div />');

                div.append(view.detailsInformation());
                div.append(view.participation.el);

                view.$el.html(div.children());
            }
            return view;
        },
        detailsInformation: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information');
            div.append(view.detailsInformationHeader());
            div.append(view.detailsInformationFields());

            return div;
        },
        detailsInformationHeader: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-header');
            div.text('Information');

            return div;
        },
        detailsInformationFields: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-fields');

            if(sig.isCsv()) {
                var csvButton = Button.init({
                    color: 'blue',
                    text: localization.designview.viewCSV,
                    size: 'tiny',
                    onClick: function() {
                        CsvSignatoryDesignPopup.popup({
                            signatory: sig,
                            onAccept: function() {
                                sig.document().save();
                            }
                        });
                    }
                });
                div.append(csvButton.input());
            } else {
                // always show these three fields first
                div.append(view.detailsFullNameField());
                div.append(view.detailsInformationField('email', 'standard', 'Email'));
                div.append(view.detailsInformationField('sigco', 'standard', 'Company'));

                $.each(sig.fields(), function(i, e) {
                    if(e.name() !== 'fstname' &&
                       e.name() !== 'sndname' &&
                       e.name() !== 'sigco'   &&
                       e.name() !== 'email'   &&
                       e.isText())
                        div.append(view.detailsInformationField(e.name(), e.type(), e.nicename()));
                });

                div.append(view.newFieldSelector.el);
            }

            div.append(view.newFieldSelector.el);

            return div;
        },
        detailsFullNameField: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var value = sig.name();
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.val(value);
            input.attr('placeholder', localization.designview.fullName);

            input.bind('keypress keydown keyup change input', function() {
                setTimeout(function() {
                    var str = input.val().trim();
                    var i = str.indexOf(' ');
                    var f, s;
                    if(i >= 0) {
                        f = str.slice(0,i).trim();
                        s = str.slice(i+1).trim();
                    } else {
                        f = str.trim();
                        s = '';
                    }
                    sig.fstnameField().setValue(f);
                    sig.sndnameField().setValue(s);
                },0); // do this with the current value (not value before keypress)
            });

            var optionOptions = sig.author()?['sender']:['signatory', 'sender'];

            var options = new FieldOptionsView({
                model: sig.fstnameField(),
                options: optionOptions
            });

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');

            div.append(closer);
            div.append(input);
            div.append(options.el);

            return div;
        },
        detailsInformationField: function(name, type, placeholder) {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var field = sig.field(name, type);
            if(!field)
                return null;

            var value = field.value();

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');
            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.val(value);
            input.attr('placeholder', placeholder || name);

            input.bind('keyup keydown keypress change input', function() {
                setTimeout(function() {
                    field.setValue(input.val().trim());
                    if(viewmodel.showProblems() && !field.isValid())
                        input.addClass('redborder');
                    else
                        input.removeClass('redborder');
                },0);
            });

            var optionOptions = ['optional', 'signatory', 'sender'];

            if(sig.author())
                optionOptions = _.without(optionOptions, 'signatory');

            if(name === 'email')
                optionOptions = _.without(optionOptions, 'optional');

            if(name === 'email' && sig.needsEmail())
                optionOptions = ['sender'];

            if(name === 'mobile' && sig.needsMobile())
                optionOptions = ['sender'];

            if(name === 'sigpersnr' && sig.needsPersonalNumber())
                optionOptions = _.without(optionOptions, 'optional');

            var options = new FieldOptionsView({
                model: field,
                options : optionOptions
            });

            if(viewmodel.showProblems() && !field.isValid())
                input.addClass('redborder');
            else
                input.removeClass('redborder');

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');

            if(field.canBeRemoved()) {
                closer.addClass("active").click(function() {
                    sig.deleteField(field);
                });
            }

            div.append(closer);
            div.append(input);
            div.append(options.el);

            return div;
        }
    });

    // single line view which can open
    var DesignViewParticipantView = Backbone.View.extend({
        className: 'design-view-action-participant',
        initialize: function(args) {
            var view = this;
            var sig = view.model;
            var viewmodel = args.viewmodel;
            view.viewmodel = args.viewmodel;
            _.bindAll(view);

            view.orderIcon  = new DesignViewOrderIconView ({model:view.model,
                                                            viewmodel: view.viewmodel});
            view.roleIcon   = new DesignViewRoleIconView  ({model:view.model,
                                                            viewmodel: view.viewmodel});
            view.deviceIcon = new DesignViewDeviceIconView({model:view.model,
                                                            viewmodel: view.viewmodel});
            view.authIcon   = new DesignViewAuthIconView  ({model:view.model,
                                                            viewmodel: view.viewmodel});

            view.detailsView = new DesignViewParticipantDetailsView({model:view.model,
                                                                     viewmodel: view.viewmodel});

            viewmodel.bind('change:participantDetail', view.updateOpened);
            viewmodel.bind('change:step', view.render);
            sig.bind('change:fields', view.updateOpened);
            view.render();
        },
        destroy : function() {
          this.viewmodel.unbind('change:participantDetail', this.updateOpened);
          this.viewmodel.unbind('change:step', this.render);
          this.model.unbind('change:fields', this.updateOpened);
          this.off();
          if (this.detailsView != undefined)
            this.detailsView.destroy();
          if (this.orderIcon != undefined)
            this.orderIcon.destroy();

        },
        render: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var div = $('<div />');

            if(!sig.isRemoved) {
                div.append(view.closeBox());

                div.append(view.inner());

                setTimeout(function() {
                    view.updateOpened();
                }, 0);
            }

            view.$el.html(div.children());
            return view;
        },
        open: function() {
            var view = this;
            if(!view.active) {
                view.active = true;
                var detailsHeight = view.detailsView.$el.outerHeight();
                var totalHeight = detailsHeight + 58;
                if(!view.opened) {
                    view.innerDiv.animate({height: totalHeight}, {
                        duration: 250,
                        easing: "linear",
                        complete: function() {
                            view.innerDiv.css({overflow:'visible',
                                               'z-index': 500});
                            $(window).resize();
                            view.opened = true;
                            view.active = false;
                        },
                        step: function() {
                            // we change the position of the document box, so we need to trigger resize
                            $(window).resize();
                        }
                    });
                } else {
                    // don't animate, just set them
                    view.innerDiv.css({height: totalHeight,
                                       overflow:'visible',
                                       'z-index': 500});
                    view.active = false;
                }
            } else {
                setTimeout(function() {
                    view.open();
                }, 0);
            }
            return view;
        },
        close: function() {
            var view = this;
            if(!view.active) {
                view.active = true;
                if(view.opened) {
                    view.innerDiv.css({'overflow': 'hidden',
                                       'z-index': 1});
                    view.innerDiv.animate({height:58}, {
                        duration: 250,
                        easing: "linear",
                        step: function() {
                            // we change the position of the document box, so we need to trigger resize
                            $(window).resize();
                        },
                        complete: function() {
                            $(window).resize();
                        }
                    });
                    view.active = false;
                    view.opened = false;
                } else {
                    view.innerDiv.css({height:58,
                                       overflow:'hidden',
                                       'z-index': 1});
                    view.active = false;
                }
            } else {
                setTimeout(function() {
                    view.close();
                }, 0);
            }
            return view;
        },
        updateOpened: function() {
            var view = this;
            var sig = view.model;
            if(view.viewmodel.participantDetail() === sig)
                view.open();
            else
                view.close();

            $(window).resize();
            return view;
        },
        inner: function() {
            var view = this;
            var div = $('<div />');
            div.addClass('design-view-action-participant-inner');
            div.append(view.infoBox());
            div.append(view.detailsView.el);

            // save the div for later
            view.innerDiv = div;

            return div;
        },
        closeBox: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var div = $('<div />');

            if(!sig.author()) {
                div.addClass('design-view-action-participant-close')

                div.click(function() {
                    viewmodel.setParticipantDetail(null);
                    _.each(sig.fields(), function(field) {
                        field.removeAllPlacements();
                    });
                    sig.document().removeSignatory(sig);
                });
            }
            return div;
        },
        infoBox: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;


            var div = $('<div />');
            div.addClass('design-view-action-participant-info-box');
            div.append(view.color());
            div.append(view.name());
            div.append(view.company());
            div.append(view.email());
            div.append(view.orderIcon.el);
            div.append(view.roleIcon.el);
            div.append(view.deviceIcon.el);
            div.append(view.authIcon.el);
            div.click(function() {
                if(viewmodel.participantDetail() === sig)
                    viewmodel.setParticipantDetail(null);
                else
                    viewmodel.setParticipantDetail(sig);
                return false;
            });
            return div;
        },
        name: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-info-name');
            var txt = $('<div />');
            txt.addClass('design-view-action-participant-info-name-inner');
            txt.text(sig.name());

            var f = function() {
                txt.text(sig.name());
            };

            sig.fstnameField().bind('change:value', f);
            sig.sndnameField().bind('change:value', f);

            div.append(txt);

            return div;
        },
        color: function() {
            var view = this;
            var sig = view.model;
            return $('<div />')
                .addClass('design-view-action-participant-info-color')
                .text('')
                .css('background-color', sig.color() || 'black');
        },
        email: function() {
            var view = this;
            var viewmodel = view.viewmodel;
            var sig = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-info-email');
            var txt = $('<div />');
            txt.addClass('design-view-action-participant-info-email-inner');
            txt.text(sig.email());

            var f = function() {
                txt.text(sig.email());
            };

            sig.emailField().bind('change:value', f);

            div.append(txt);
            return div;
        },
        company: function() {
            var view = this;
            var sig = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-info-company');
            var txt = $('<div />');
            txt.addClass('design-view-action-participant-info-company-inner');
            if(sig.companyField()) {
                txt.text(sig.company());

                var f = function() {
                    txt.text(sig.company());
                };


                sig.companyField().bind('change:value', f);
            }
            div.append(txt);
            return div;
        }
    });

    /**
       model is field
     **/
    var FieldOptionsView = Backbone.View.extend({
        className: 'design-view-action-participant-details-information-field-options-wrapper',
        initialize: function(args) {
            var view = this;
            view.options = args.options;
            view.extraClass = args.extraClass;
            var field = view.model;
            _.bindAll(view);
            view.render();
            if(field) {
                field.bind('change:obligatory', view.render);
                field.bind('change:shouldbefilledbysender', view.render);
            }
        },
        render: function() {
            var view = this;
            var field = view.model;
            var selected;
            if(!field) {
                selected = 'optional';
            } else if(field.isOptional()) {
                selected = 'optional';
            } else if(field.shouldbefilledbysender()) {
                selected = 'sender';
            } else {
                selected = 'signatory';
            }
            var values = view.options;
            var options = {
                optional  : {abbrev : localization.designview.optionalFieldAbbrev,
                             name: localization.designview.optionalField,
                             value : 'optional'
                            },
                signatory : {name : localization.designview.mandatoryForRecipient,
                             abbrev : localization.designview.mandatoryForRecipientAbbrev,
                             value : 'signatory'
                            },
                sender    : {name : localization.designview.mandatoryForSender,
                             abbrev: localization.designview.mandatoryForSenderAbbrev,
                             value : 'sender'
                            }
            };
            var select = new Select({
                options: _.map(values, function(v) {
                    return options[v];
                }),
                name: options[selected].abbrev,
                offset: options[selected].offset,
                onSelect: function(v) {
                    if(field) {
                        if(v === 'optional') {
                            field.makeOptional();
                        } else if(v === 'signatory') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(false);
                        } else if(v === 'sender') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(true);
                        }
                    }
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-information-field-options');
            if(view.extraClass)
                select.view().$el.addClass(view.extraClass);
            view.$el.html(select.view().el);
            return view;
        }
    });

    window.DesignViewParticipantsView = function(args) {
        return new DesignViewParticipantsView(args);
    }

    window.FieldOptionsView = FieldOptionsView;

}(window));
