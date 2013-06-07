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
                    view.addOne();
                }
            });

            return button.input();
        },
        addOne: function() {

            var view = this;
            var sig = view.model;

            var field = new Field({
                name: '',
                type: '',
                signatory: sig,
                obligatory: false,
                shouldbefilledbysender: sig.author()
            });

            if(field.obligatory() && field.shouldbefilledbysender())
                field.authorObligatory = 'sender';
            else if(field.obligatory())
                field.authorObligatory = 'recipient';
            else
                field.authorObligatory = 'optional';


            sig.addField(field);
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
            div.append(view.detailsParticipationFieldsRole());
            div.append(view.detailsParticipationFieldsDelivery());
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
            view.model.bind('change:participantDetail', view.render);
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

            if(model.participantDetail()) {
                div.append(view.doneButton());
            } else {
                var thereIsMultiSendAlready = _.any(model.document().signatories(), function(x) { return x.isCsv(); });
                if(!thereIsMultiSendAlready) {
                    div.append(view.multi());
                }
                div.append(view.single());
            }

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
        },
        doneButton: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-done');

            var button = Button.init({
                color: 'green',
                size: 'tiny',
                text: localization.save,
                onClick: function() {
                    model.setParticipantDetail(null);
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
            view.model.bind('change:participantDetail', view.rescroll);
            $(window).resize(view.resizeOnWindowResize);
        },
        rescroll: function() {
            var view = this;
            var participantDetail = view.model.participantDetail();
            if( participantDetail ) {
                var allSignatories = view.model.document().signatories();
                var lastSignatory = allSignatories[allSignatories.length - 1];
                if( participantDetail === lastSignatory ) {
                    setTimeout(function() {
                        view.scrollBox.mCustomScrollbar("update");
                        view.scrollBox.mCustomScrollbar("scrollTo","bottom");
                    }, 300);
                }
            }
        },
        reset: function() {
            var view = this;
            if (view.participants != undefined) _.each(view.participants,function(pv) {pv.destroy();});
            view.setup();
            view.render();
        },
        closeAllParticipants : function() {
            console.log("Closing participants")
            this.model.setParticipantDetail(null);

            console.log("Closing is done")

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
        resizeOnWindowResize: function() {
            var view = this;
            var newHeight = $(window).height() - 350;
            if( newHeight<250 ) {
                newHeight = 250;
            }
            view.scrollBox.css("max-height", newHeight + "px");
            setTimeout(function() {
                view.scrollBox.mCustomScrollbar('update');
            }, 500);
            return false;
        },
        render: function() {
            var view = this;
            var model = view.model;
            view.$el.children().detach();
            var box = $("<div class='design-view-action-participant-container-participants-box'>");
            var newHeight = $(window).height() - 350;
            if( newHeight<250 ) {
                newHeight = 250;
            }
            box.css("max-height", newHeight + "px");
            $.each(view.participants, function(i, p) {
               box.append(p.el);
            });

            view.$el.append(box).append(view.addNew.el);


            box.mCustomScrollbar({ mouseWheel: true
                                   , theme: "dark-2"
                                   , advanced:{
                                       updateOnContentResize: true // this is polling, might want to have update called in proper times
                                   }
                                   , scrollInertia: 0
                                   , scrollButtons:{
                                       enable: false // we do not want buttons, also we do not want pngs that come with the buttons
                                   }});
            view.scrollBox = box;
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
            viewer: 'design-view-action-participant-icon-role-icon-viewer',
            signatory: 'design-view-action-participant-icon-role-icon-signatory'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var role = sig.role();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-role-inner')
                .append($('<div />')
                        .addClass('design-view-action-participant-icon-role-icon')
                        .addClass(view.icons[role]));

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
                view.model.ensureEmail();

                return false;
            });
        },
        icons: {
            email: 'design-view-action-participant-icon-device-icon-email',
            pad: 'design-view-action-participant-icon-device-icon-pad',
            api: 'design-view-action-participant-icon-device-icon-pad',
            mobile: 'design-view-action-participant-icon-device-icon-phone',
            email_mobile : 'design-view-action-participant-icon-device-icon-email-mobile'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var delivery = sig.delivery();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-device-inner')
                .append($('<div />')
                        .addClass('design-view-action-participant-icon-device-icon')
                        .addClass(view.icons[delivery]));
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
            standard: 'design-view-action-participant-icon-auth-icon-noauth',
            eleg: 'design-view-action-participant-icon-auth-icon-eleg'
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var auth = sig.authentication();

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-auth-inner')
                .append($('<div />')
                        .addClass('design-view-action-participant-icon-auth-icon')
                        .addClass(view.icons[auth]));

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
            view.model.bind('change:fields', view.render);
            view.model.bind('change:authentication', view.render);
            view.model.bind('change:delivery', view.render);
            view.render();
        },
        destroy : function() {
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
                var wrapperdiv = $('<div />');
                wrapperdiv.addClass('design-view-action-participant-details-information-field-wrapper');
                wrapperdiv.append(csvButton.input());
                div.append(wrapperdiv);
            } else {
                // always show these three fields first
                div.append(view.detailsFullNameField());
                div.append(view.detailsInformationField('email', 'standard', localization.email));
                var ignores = ['fstname', 'sndname', 'email'];
                $.each(sig.fields(), function(i, e) {
                    if(e.isBlank())
                        div.append(view.detailsInformationNewField(e));
                    else if(e.noName())
                        div.append(view.detailsInformationCustomFieldName(e));
                    else if(!_.contains(ignores, e.name()) && e.isText())
                        div.append(view.detailsInformationField(e.name(), e.type(), e.nicename()));
                });

                div.append(view.newFieldSelector.el);
            }

            div.append(view.newFieldSelector.el);

            return div;
        },
        detailsInformationCustomFieldName: function(field) {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var setter = function() {
                if(input.value()) {
                    field.setName(input.value());
                    sig.trigger('change:fields');
                    field.unbind('change:name', changer);
                }
            };

            var changer = function() {
                input.setValue(field.name());
            };

            var input = InfoTextInput.init({
                cssClass: 'design-view-action-participant-new-field-name-input',
                infotext: localization.designview.fieldName,
                value: '',
                onEnter: setter
            });

            field.bind('change:name', changer);

            if(!field.isValid(true))
                $(input.input()).addClass('redborder');
            else
                $(input.input()).removeClass('redborder');

            var button = Button.init({
                color: 'black',
                size: 'tiny',
                text: localization.ok,
                width: 64,
                onClick: setter
            });

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');
            closer.addClass("active").click(function() {
                field.removeAllPlacements();
                sig.deleteField(field);
            });

            div.append(closer);
            div.append(input.input());
            div.append(button.input());

            return div;

        },
        detailsInformationNewField: function(field) {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var allFieldOptions = view.possibleFields.concat([]);

            function isUnique(field) {
                return _.every(allFieldOptions, function(o) {
                    return field.name() !== o.name && field.type() !== o.type;
                });
            }

            _.each(viewmodel.document().signatories(), function(signatory) {
                _.each(signatory.fields(), function(field) {
                    if(field.isText() && isUnique(field))
                        allFieldOptions.push({name: field.name(),
                                              type: field.type()});
                });
            });

            var options = [];

            // keep only fields not already part of signatory
            _.each(allFieldOptions, function(f) {
                if(!sig.field(f.name, f.type))
                    options.push({
                        name: view.placeholder(f.name),
                        value: f
                    });
            });

            options.push({name: localization.designview.customField,
                          value: {name: '--custom',
                                  type: '--custom'}}); // type is not used for custom

            var name;

            if(!view.selected)
                name = localization.designview.whatField;
            else if(view.selected.name === '--custom')
                name = localization.designview.customField;
            else
                name = view.placeholder(view.selected.name);

            var select = new Select({
                options: options,
                name: name,
                onSelect: function(v) {
                    if(v.name === '--custom') {
                        field.setType('custom');
                    } else {
                        field.setType(v.type);
                        field.setName(v.name);
                    }
                    sig.trigger('change:fields');
                    return true;
                }
            });

            $(select.view().el).addClass('design-view-action-participant-new-field-select');

            if(!field.isValid(true))
                $(select.view().el).addClass('redborder');
            else
                $(select.view().el).removeClass('redborder');

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');
            closer.addClass("active").click(function() {
                field.removeAllPlacements();
                sig.deleteField(field);
            });

            div.append(select.view().el);
            div.append(closer);

            return div;

        },
        detailsFullNameField: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

            var value = sig.name();
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');
            var fstnameField = sig.fstnameField();
            var sndnameField = sig.sndnameField();

            var input = InfoTextInput.init({
                cssClass: 'design-view-action-participant-details-information-field',
                infotext: localization.designview.fullName,
                value: value,
                onChange: function(val) {
                    var str = val.trim();
                    var i = str.indexOf(' ');
                    var f, s;
                    if(i >= 0) {
                        f = str.slice(0,i).trim();
                        s = str.slice(i+1).trim();
                    } else {
                        f = str.trim();
                        s = '';
                    }
                    /*
                     * First we need to set the value silently, then
                     * broadcast the info about changes to the world.
                     * Otherwise the hard link to full name breaks.
                     */
                    fstnameField.setValueSilent(f);
                    sndnameField.setValueSilent(s);
                    fstnameField.setValue(f);
                    sndnameField.setValue(s);
                }
            });

            fstnameField.bind('change', function() {
                if(!fstnameField.isValid(true))
                    input.input().addClass('redborder');
                else
                    input.input().removeClass('redborder');
                input.setValue(sig.name());
            });

            sndnameField.bind('change', function() {
                if(!fstnameField.isValid(true) )
                    input.input().addClass('redborder');
                else
                    input.input().removeClass('redborder');
                input.setValue(sig.name());
            });
            if(!fstnameField.isValid(true))
                    input.input().addClass('redborder');
            else
                    input.input().removeClass('redborder');

            var optionOptions = sig.author()?['sender']:['signatory', 'sender'];

            var options = new FieldOptionsView({
                model: fstnameField,
                options: optionOptions
            });

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');

            div.append(closer);
            div.append(input.input());
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

            var input = InfoTextInput.init({
                cssClass: 'design-view-action-participant-details-information-field',
                infotext: placeholder || name,
                value: value,
                onChange: function(val) {
                    if(typeof val === 'string')
                        field.setValue(val.trim());
                }
            });

            field.bind('change', function() {
                if(!field.isValid(true))
                    input.input().addClass('redborder');
                else
                    input.input().removeClass('redborder');
                input.setValue(field.value());
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

            if(!field.isValid(true))
                input.input().addClass('redborder');
            else
                input.input().removeClass('redborder');

            var closer = $('<div />');
            closer.addClass('design-view-action-participant-details-information-closer');

            if(field.canBeRemoved()) {
                closer.addClass("active").click(function() {
                    field.removeAllPlacements();
                    sig.deleteField(field);
                });
            }

            div.append(closer);
            div.append(input.input());
            div.append(options.el);

            return div;
        },
        possibleFields: [
            {name: "fstname",
             type: 'standard'},
            {name: "sndname",
             type: 'standard'},
            {name: "email",
             type: 'standard'},
            {name: "sigco",
             type: 'standard'},
            {name: "sigpersnr",
             type: 'standard'},
            {name: "sigcompnr",
             type: 'standard'},
            {name: "mobile",
         type: 'standard'}
        ],
        fieldNames: {
            fstname: localization.fstname,
            sndname: localization.sndname,
            email: localization.email,
            sigcompnr: localization.companyNumber,
            sigpersnr: localization.personamNumber,
            sigco: localization.company,
            mobile: localization.phone
        },
        placeholder: function(name) {
            return this.fieldNames[name] || name;
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
            view.innerDiv = $("<div class='design-view-action-participant-inner'/>");
            view.innerDiv.append(view.infoBox());
            view.innerDiv.append(view.detailsView.el);
            return view.innerDiv;
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

            if( sig.isCsv()) {
                txt.text(localization.csv.title);
            }
            else {
                txt.text(sig.name());

                var f = function() {
                    txt.text(sig.name());
                };

                sig.bind('change:name', f);

            }

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

            sig.bind('change:email', f);

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


                sig.bind('change:company', f);
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
                            field.authorObligatory = 'optional';
                        } else if(v === 'signatory') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(false);
                            field.authorObligatory = 'signatory';
                        } else if(v === 'sender') {
                            field.makeObligatory();
                            field.setShouldBeFilledBySender(true);
                            field.authorObligatory = 'sender';
                        }
                        field.addedByMe = false;
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
