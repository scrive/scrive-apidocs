/* 
 * Participants tab of design view.
 *
 */

(function(window){
    /* model is view model */
    var DesignViewNewParticipation = Backbone.View.extend({
        className: 'design-view-action-participant-details-participation',
        initialize: function() {
            var view = this;
            _.bindAll(view);
            // if the signatory changes, we rerender
            view.model.newSingle().bind('change', view.render);
            view.render();
        },
        render: function() {
            var view = this;
            var model = view.model;
            var sig = model.newSingle();

            if(sig) {
                var div = $('<div />');

                div.append(view.detailsParticipationHeader());
                div.append(view.detailsParticipationFields());
                
                view.$el.html(div.children());
            }
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
            var viewmodel = view.model;
            var model = viewmodel.newSingle();
            
            var i;

            var order = model.signorder();

            var options = [];
            for(i=1;i<=viewmodel.document().maxPossibleSignOrder() + 1;i++)
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
            var viewmodel = view.model;
            var sig = viewmodel.newSingle();
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
            var viewmodel = view.model;
            var sig = viewmodel.newSingle();
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
            var viewmodel = view.model;
            var sig = viewmodel.newSingle();
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
                    sig.setAuthentication(v);
                    return true;
                }
            });
            select.view().$el.addClass('design-view-action-participant-details-participation-auth');
            return select.view().el;
        }
    });

    /** The large detail box for multi-sendout.
        The model is the DesignViewModel
     **/
    var DesignViewParticipantsNewMultiDetailsView = Backbone.View.extend({
        className: 'design-view-action-participant-new-multi-details',
        initialize: function(args) {
            var view = this;
            _.bindAll(view);
            view.render();
        },
        render: function() {
            var view = this;

            var div = $('<div />');

            view.$el.html(div.children());
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
            // if the new signatory gets changed, we rerender
            view.model.bind('change:newSingle', view.render);
        },
        render: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');

            if(model.newSingle()) {
                view.participation = new DesignViewNewParticipation({model:model});
                div.append(view.detailsInformation());
                div.append(view.participation.el);
                div.append(view.buttons());
            }

            view.$el.html(div.children());
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
        },
        buttons: function() {
            var view = this;
            var model = view.model;

            var div = $('<div />');
            div.addClass('design-view-action-participant-buttons');

            var cancel = Button.init({
                color: 'red',
                text: 'Cancel',
                size: 'tiny',
                onClick: function() {
                    model.setNewSingle(null);
                }
            });

            var save = Button.init({
                color: 'blue',
                text: 'Save',
                size: 'tiny',
                onClick: function() {
                    model.document().addExistingSignatory(model.newSingle());
                    model.setNewSingle(null);
                }
            });

            div.append(cancel.input());
            div.append(save.input());

            return div;
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
                    sig.setAuthentication(v);
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
            // when newSingle no longer exists, we want to close new signatory box
            view.model.bind('change:newSingle', function() {
                if(!view.model.newSingle())
                    view.close();
            });
        },
        render: function() {
            var view = this;

            var div = $('<div />');
            div.addClass('design-view-action-participant-new-box');
            div.append(view.newButtons());
            div.append(view.singleDetails.el);
            div.append(view.multiDetails.el);

            view.$el.html(div.children());
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
                if(model.newSingle()) {
                    model.setParticipantDetail(null);
                    model.setNewSingle(null);
                } else {
                    model.setParticipantDetail(null);
                    model.setNewSingle(new Signatory({
                        document:model.document(),
                        signs: true
                    }));
                    view.openSingle();
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
                model.setParticipantDetail(null);
                model.setNewSingle(null);

                var doc = model.document()
                var sig = new Signatory({
                    document:doc,
                    signs:true
                });
                CsvSignatoryDesignPopup.popup({
                    signatory: sig,
                    onAccept: function() {
                        doc.addExistingSignatory(sig);
                        doc.save();
                        model.setParticipantDetail(sig);
                    }
                });
                return false;
            });

            return div;
        },
        openSingle: function() {
            var view = this;
            view.$el.animate({height: view.singleDetails.$el.height() + 50}, {
                duration: 500,
                easing: "linear",
                complete: function() {
                    view.$el.css('overflow', 'visible');
                    $(window).resize();
                },
                step: function() {
                    $(window).resize();
                }
            });
            return view;
        },
        close: function() {
            var view = this;
            view.$el.animate({height: 50}, {
                duration: 500,
                easing: "linear", 
                complete: function() {
                    view.$el.css('overflow', 'hidden');
                    $(window).resize();
                },
                step: function() {
                    $(window).resize();
                }
            });
            return view;
        },
        openMulti: function() {
            var view = this;
            view.$el.animate({height: view.singleDetails.$el.height() + 50},
                             500, "linear", function() {
                                 view.$el.css('overflow', 'visible');
                             });
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
            // any changes to the signatories of a document and we rerender
            // this includes adding and removing signatories
            view.model.document().bind('change:signatories', view.reset);
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
            view.viewmodel.bind('change:showProblems', view.render);
            view.render();
        },
        render: function() {
            var view = this;
            var sig = view.model;

            var div = $('<div />');

            div.append(view.detailsInformation());
            div.append(view.participation.el);
            
            view.$el.html(div.children());
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
                    text: 'View CSV',
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
            }
            
            return div;
        },
        detailsFullNameField: function() {
            var view = this;
            var sig = view.model;
            var viewmodel = view.viewmodel;

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
                },0); // do this with the current value (not value before keypress)
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
            var viewmodel = view.viewmodel;

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
                    if(viewmodel.showProblems() && !field.isValid())
                        input.addClass('redborder');
                    else
                        input.removeClass('redborder');
                },0);
            });
            
            var options = new FieldOptionsView({
                model: field
            });

            if(viewmodel.showProblems() && !field.isValid())
                input.addClass('redborder');
            else
                input.removeClass('redborder');

            div.append(input);
            div.append(options.el);

            return div.children();
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
                view.innerDiv.animate({height: view.detailsView.$el.height() + 50}, {
                    duration: 500,
                    easing: "linear",
                    complete: function() {
                        view.innerDiv.css({overflow:'visible',
                                           'z-index': 500});
                        $(window).resize();
                    },
                    step: function() {
                        // we change the position of the document box, so we need to trigger resize
                        $(window).resize();
                    }
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
                view.innerDiv.animate({height:50}, {
                    duration: 500,
                    easing: "linear",
                    step: function() {
                        // we change the position of the document box, so we need to trigger resize
                        $(window).resize();
                    },
                    complete: function() {
                        $(window).resize();
                    }
                });
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

            if(!sig.author()) {
                div.addClass('design-view-action-participant-close')
                
                var txt = $('<div />');
                txt.addClass('design-view-action-participant-close-x');
                txt.text('x');
                div.html(txt);
                
                div.click(function() {
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

            if(viewmodel.showProblems() && !sig.emailField().isValid())
                div.addClass('redborder');
            else
                div.removeClass('redborder');            

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
