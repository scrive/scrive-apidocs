/* 
 * Participants tab of design view.
 *
 */

(function(window){
    /** The large detail box for multi-sendout.
        The model is the DesignViewModel
     **/
    var DesignViewParticipantsNewMultiDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-new-multi-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
            view.model.bind('change:participantDetail', view.updateHidden);
        },
        hide: function() {
            var view = this;
            if(!view.$el.hasClass('hidden'))
                view.$el.addClass('hidden');
            return view;
        },
        show: function() {
            var view = this;
            view.$el.removeClass('hidden');
            return view;
        },
        updateHidden: function() {
            var view = this;
            var model = view.model;
            if(model.participantDetail() === 'multi')
                view.show();
            else
                view.hide();
            return view;
        },
        render: function() {
            var view = this;

            var div = $('<div />');

            view.$el.html(div.children());
            view.updateHidden();
            return view;
        },
        details: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details');
            div.append(view.detailsInformation());

            view.detailsDiv = div;
            
            return div;
        },
        detailsInformation: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information');
            div.append(view.detailsInformationHeader());
            div.append(view.detailsInformationFields());
            
            return div;
        },
        detailsInformationHeader: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-header');
            div.text('Information');
            
            return div;
        },
        detailsInformationFields: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-fields');
            div.append(view.detailsFullNameField());
            div.append(view.detailsInformationField('email'));
            div.append(view.detailsInformationField('sigco'));
            
            return div;
        },
        detailsFullNameField: function() {
            var view = this;
            var model = view.model;

            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.attr('placeholder', 'Full name');
            return input;
        },
        detailsInformationField: function(name) {
            var view = this;
            var model = view.model;
            
            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.attr('placeholder', name);
            
            return input;
        }
    });

    /** The large details view for a single new signatory 
        Model is the DesignViewModel 
    **/
    var DesignViewParticipantsNewSingleDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-new-single-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
            view.model.bind('change:participantDetail', view.updateHidden);
            view.model.bind('change:newSingle', view.render);
        },
        hide: function() {
            var view = this;
            if(!view.$el.hasClass('hidden'))
                view.$el.addClass('hidden');
            return view;
        },
        show: function() {
            var view = this;
            view.$el.removeClass('hidden');
            return view;
        },
        updateHidden: function() {
            var view = this;
            var model = view.model;
            if(model.participantDetail() === 'single')
                view.show();
            else
                view.hide();
            return view;
        },
        render: function() {
            var view = this;

            var div = $('<div />');

            div.append(view.detailsInformation());
            //div.append(view.detailsParticipation());

            view.$el.html(div.children());
            view.updateHidden();
            return view;
        },
        detailsInformation: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information');
            div.append(view.detailsInformationHeader());
            div.append(view.detailsInformationFields());
            
            return div;
        },
        detailsInformationHeader: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-header');
            div.text('Information');
            
            return div;
        },
        detailsInformationFields: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-fields');
            div.append(view.detailsFullNameField());
            div.append(view.detailsInformationField('email', 'standard', 'Email'));
            div.append(view.detailsInformationField('sigco', 'standard', 'Company'));
            
            return div;
        },
        detailsFullNameField: function() {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.attr('placeholder', 'Full name');

            var fstname = model.newSingle().fstnameField();
            var sndname = model.newSingle().sndnameField();

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
                    fstname.setValue(f);
                    sndname.setValue(s);
                },0); // do this with the current value
            });


            var options = new FieldOptionsView({
                model: fstname
            });

            fstname.bind('change:obligatory', function() {
                if(fstname.isOptional())
                    sndname.makeOptional();
                else
                    sndname.makeObligatory();
            });

            div.append(input);
            div.append(options.el);
            return div;
        },
        detailsInformationField: function(name, type, placeholder) {
            var view = this;
            var model = view.model;
            
            var div = $('<div />');
            div.addClass('design-view-action-participant-details-information-field-wrapper');

            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.attr('placeholder', placeholder || name);

            var options = new FieldOptionsView({
                model: model.newSingle().field(name, type)
            });

            div.append(input);
            div.append(options.el);
            return div;
        }
        
    });

    var DesignViewParticipation = Backbone.View.extend({
        className: 'design-view-action-participant-details-participation',
        initialize: function() {
            var view = this;
            _.bindAll(view);

            view.render();
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
            div.text('Define participation:');
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
            for(i=1;i<=model.document().maxPossibleSignOrder() + 1;i++)
                options.push({name: englishOrdinal(i) + ' to receive document',
                              value: i});
            
            var select = new Select({
                options: options,
                name: englishOrdinal(order) + ' to receive document',
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
                email : "by email",
                pad : "on this tablet",
                api : "via API"
            };

            var deliveryTypes = ['email', 'pad', 'api'];

            var select = new Select({
                options: _.map(deliveryTypes, function(t) {
                    return {name: deliveryTexts[t], value:t};
                }),
                name: deliveryTexts[delivery],
                onSelect: function(v) {
                    sig.setDelivery(v);
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-delivery');
            return select.view().el;
        },
        detailsParticipationFieldsRole: function() {
            var view = this;
            var sig = view.model;
            var role = sig.signs();

            var roleTexts = {
                true : "for signing",
                false : "for viewing"
            };

            var roleTypes = [true, false];

            var select = new Select({
                options: _.map(roleTypes, function(t) {
                    return {name: roleTexts[t], value:t};
                }),
                name: roleTexts[role],
                onSelect: function(v) {
                    if(v)
                        sig.makeSignatory();
                    else
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
                standard : "with no additional ID control",
                eleg : "with Elegitimation"
            };

            var authTypes = ['standard', 'eleg'];

            var select = new Select({
                options: _.map(authTypes, function(t) {
                    return {name: authTexts[t], value:t};
                }),
                name: authTexts[auth],
                onSelect: function(v) {
                    sig.setAuthorization(v);
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
            view.singleDetails = new DesignViewParticipantsNewSingleDetailsView(args);
            view.multiDetails  = new DesignViewParticipantsNewMultiDetailsView(args);
            view.render();
            view.model.bind('change:participantDetail', view.updateOpened);
        },
        render: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-participant-new-box');
            div.append(view.newButtons());
            div.append(view.singleDetails.el);
            div.append(view.multiDetails.el);

            view.$el.html(div.children());
            view.updateOpened();
            return view;
        },
        newButtons: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-new-box-buttons');
            div.append(view.single());
            div.append(view.multi());
            return div;
        },
        single: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-new-single');
            div.append($('<div />')
                       .addClass('design-view-action-participant-new-single-text')
                       .text('+ Add party'));

            div.click(function() {
                if(model.participantDetail() === 'single') 
                    model.setParticipantDetail(null);
                else {
                    model.setNewSingle(new Signatory({}));
                    model.setParticipantDetail('single');
                }
                return false;
            });



            return div;
        },
        multi: function() {
            var view = this;
            var model = view.model;
            var div = $('<div />');
            div.addClass('design-view-action-participant-new-multi');
            div.append($('<div />')
                       .addClass('design-view-action-participant-new-multi-text')
                       .text('+ Add multisend'));

            div.click(function() {
                if(model.participantDetail() === 'multi')
                    model.setParticipantDetail(null);
                else
                    model.setParticipantDetail('multi');
                return false;
            });

            return div;
        },
        openSingle: function() {
            var view = this;
            view.$el.addClass('opensingle');
            view.$el.removeClass('openmulti');
            return view;
        },
        close: function() {
            var view = this;
            view.$el.removeClass('opensingle');
            view.$el.removeClass('openmulti');
            return view;
        },
        openMulti: function() {
            var view = this;
            view.$el.addClass('openmulti');
            view.$el.removeClass('opensingle');
            return view;
        },
        updateOpened: function() {
            var view = this;
            var model = view.model;

            if(model.participantDetail() === 'single')
                view.openSingle();
            else if(model.participantDetail() === 'multi')
                view.openMulti();
            else 
                view.close();

            return view;
        }
    });

    var DesignViewParticipantsView = Backbone.View.extend({
        className: 'design-view-action-participant-container',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.addNew = new DesignViewParticipantsNewView(args);
            view.reset();
            // TODO: clean up events that trigger change:signatories!
            //view.model.document().bind('change:signatories', view.reset);
        },
        reset: function() {
            var view = this;
            view.setup();
            view.render();
        },
        setup: function() {
            var view = this;
            var model = view.model;

            view.participants = [];

            var doc = model.document();
            var sigs = doc.signatories();
            
            $.each(model.document().signatories(), function(i, s) {
                view.participants.push(new DesignViewParticipantView({model  : s, 
                                                                      number : i,
                                                                      viewmodel : model}));
            });
            return view;
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');
            $.each(view.participants, function(i, p) {
                div.append(p.el);
            });

            div.append(view.addNew.el);

            view.$el.html(div.children());

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
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-role-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-role-icon')
                        .attr('src', '/img/signatory.png'));
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
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-device-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-device-icon')
                        .attr('src', '/img/pad2.png'));
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
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />')
                .addClass('design-view-action-participant-icon-auth-inner')
                .append($('<img />')
                        .addClass('design-view-action-participant-icon-auth-icon')
                        .attr('src', '/img/eleg.png'));
            view.$el.html(div);

            return view;
        }
    });

    var DesignViewParticipantDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.viewmodel = args.viewmodel;
            view.participation = new DesignViewParticipation({model:view.model});
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            view.$el.append(view.detailsInformation());
            view.$el.append(view.participation.el);
            
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
            div.append(view.detailsFullNameField());
            div.append(view.detailsInformationField('email', 'standard', 'Email'));
            div.append(view.detailsInformationField('sigco', 'standard', 'Company'));

            $.each(sig.fields(), function(i, e) {
                if(e.name() !== 'fstname' && 
                   e.name() !== 'sndname' &&
                   e.name() !== 'sigco'   &&
                   e.name() !== 'email')
                    div.append(view.detailsInformationField(e.name));
            });
            
            return div;
        },
        detailsFullNameField: function() {
            var view = this;
            var sig = view.model;

            var value = sig.name();
            var div = $('<div />');
            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.val(value);
            input.attr('placeholder', 'Full name');

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
                },0); // do this with the current value
            });
            
            var options = new FieldOptionsView({
                model: sig.fstnameField()
            });

            div.append(input);
            div.append(options.el);
            return div.children();
        },
        detailsInformationField: function(name, type, placeholder) {
            var view = this;
            var sig = view.model;

            var field = sig.field(name, type);
            if(!field)
                return null;

            var value = field.value();

            var div = $('<div />');
            var input = $('<input />');
            input.addClass('design-view-action-participant-details-information-field');
            input.val(value);
            input.attr('placeholder', placeholder || name);

            input.bind('keyup keydown keypress change input', function() {
                setTimeout(function() {
                    field.setValue(input.val().trim());
                },0);
            });
            
            var options = new FieldOptionsView({
                model: field
            });

            div.append(input);
            div.append(options.el);

            return div.children();
        }
    });

    // single line view
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

            //sig.bind('change', view.render);
            viewmodel.bind('change:participantDetail', view.updateOpened);
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;
            console.log(view.opened);

            var div = $('<div />');

            div.append(view.closeBox());

            div.append(view.inner());

            view.updateOpened();

            view.$el.html(div.children());
            return view;
        },
        open: function() {
            var view = this;
            if(!view.opened) {
                view.innerDiv.animate({height: view.detailsView.$el.height() + 50},
                                      500, "linear", function() {
                                          view.innerDiv.css({overflow:'visible',
                                                             'z-index': 500});
                                      });
                view.opened = true;
            } else {
                // don't animate, just set them
                view.innerDiv.css({height: view.detailsView.$el.height() + 50,
                                   overflow:'visible',
                                   'z-index': 500});
            }
            return view;
        },
        close: function() {
            var view = this;
            if(view.opened) {
                view.innerDiv.css({'overflow': 'hidden',
                                   'z-index': 1});
                view.innerDiv.animate({height:50}, 500, "linear");
                view.opened = false;
            } else {
                view.innerDiv.css({height:50, 
                                   overflow:'hidden',
                                   'z-index': 1});
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
            var div = $('<div />');
            div.addClass('design-view-action-participant-close')

            var txt = $('<div />');
            txt.addClass('design-view-action-participant-close-x');
            txt.text('x');
            div.html(txt);

            div.click(function() {
                sig.document().removeSignatory(sig);
            });

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
            div.append(view.email());
            div.append(view.company());
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
            return $('<div />')
                .addClass('design-view-action-participant-info-color')
                .text('');
        },
        email: function() {
            var view = this;
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
            txt.text(sig.company());

            var f = function() {
                txt.text(sig.company());
            };

            sig.companyField().bind('change:value', f);

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
            var field = view.model;
            _.bindAll(view);
            view.render();
            field.bind('change:obligatory', view.render);
        },
        render: function() {
            var view = this;
            var field = view.model;
            var selected = field.obligatory() ? 'signatory' : 'optional';
            var values = ['optional', 'signatory'];
            var options = {
                optional  : {name : 'Optional'                , value : 'optional'},
                signatory : {name : 'Mandatory for signatory' , value : 'signatory'}
            };
            var select = new Select({
                options: _.map(_.filter(values, function(v) {
                    return v !== selected;
                }), function(v) {
                    return options[v];
                }),
                name: options[selected].name,
                onSelect: function(v) {
                    if(v === 'optional')
                        field.makeOptionalM();
                    else if(v === 'signatory')
                        field.makeObligatoryM();
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-information-field-options');
            view.$el.html(select.view().el);
            return view;
        }
    });

    window.DesignViewParticipantsView = function(args) {
        return new DesignViewParticipantsView(args);
    }

}(window));
