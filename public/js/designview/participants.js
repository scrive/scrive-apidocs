/* 
 * Participants tab of design view.
 *
 * Instrumented with Mixpanel events.
 */


(function(window){


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

var DesignViewParticipantsNewSingleDetailsView = Backbone.View.extend({
    className: 'design-view-action-participant-new-single-details',
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
        div.append(view.detailsParticipation());

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
        div.append(view.detailsInformationField('email'));
        div.append(view.detailsInformationField('sigco'));
        
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

        var options = $('<select />');
        options.addClass('design-view-action-participant-details-information-field-options');
        options.append($('<option value="optional" />')
                       .text('Optional'));
        options.append($('<option value="sender" />')
                       .text('Mandatory for sender'));
        options.append($('<option value="signatory" />')
                       .text('Mandatory for signatory'));

        div.append(input);
        div.append(options);
        return div;
    },
    detailsInformationField: function(name) {
        var view = this;
        var model = view.model;
        
        var div = $('<div />');
        div.addClass('design-view-action-participant-details-information-field-wrapper');

        var input = $('<input />');
        input.addClass('design-view-action-participant-details-information-field');
        input.attr('placeholder', name);

        var options = $('<select />');
        options.addClass('design-view-action-participant-details-information-field-options');
        options.append($('<option value="optional" />')
                       .text('Optional'));
        options.append($('<option value="sender" />')
                       .text('Mandatory for sender'));
        options.append($('<option value="signatory" />')
                       .text('Mandatory for signatory'));

        div.append(input);
        div.append(options);
        return div;
    },
    detailsParticipation: function() {
        var view = this;
        var model = view.model;

        var div = $('<div />');
        div.addClass('design-view-action-participant-details-participation');
        div.append(view.detailsParticipationHeader());
        div.append(view.detailsParticipationFields());
        return div;
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
        
        var input = $('<select />');
        var i;
        
        for(i=1;i<=model.document().maxPossibleSignOrder() + 1;i++)
            input.append($('<option />').val(i).text(i + 'st to sign'));
        
        return input;
    },
    detailsParticipationFieldsDelivery: function() {
        var view = this;

        var input = $('<select />');
        input.append($('<option value="device" />').text('on this tablet'));
        input.append($('<option value="email" />').text('email to participant'));
        
        return input;
    },
    detailsParticipationFieldsRole: function() {
        var view = this;

        var input = $('<select />');
        input.append($('<option value="signs" />').text('for signing'));
        input.append($('<option value="views" />').text('for viewing'));
        
        return input;
    },
    detailsParticipationFieldsAuth: function() {
        var view = this;

        var input = $('<select />');
        input.append($('<option value="standard" />').text('with no additional ID required'));
        input.append($('<option value="eleg" />').text('with Elegitimation'));
        
        return input;
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
            else
                model.setParticipantDetail('single');
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

        view.updateHidden();

        return view;
    },
    hide: function() {
        var view = this;
        view.$el.toggleClass('hidden', true);
        return view;
    },
    unhide: function() {
        var view = this;
        view.$el.toggleClass('hidden', false);
        return view;
    },
    updateHidden: function() {
        var view = this;
        var viewmodel = view.model;
        if(viewmodel.step() === 1)
            view.unhide();
        else
            view.hide();
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
        view.render();
        view.viewmodel.bind('change:participantDetail', view.updateHidden);
    },
    updateHidden: function() {
        var view = this;
        var sig = view.model;
        var viewmodel = view.viewmodel;
        if(viewmodel.participantDetail() === sig)
            view.unhide();
        else
            view.hide();
        return view;
    },
    hide: function() {
        var view = this;
        view.$el.toggleClass('hidden', true);
        return view;
    },
    unhide: function() {
        var view = this;
        view.$el.toggleClass('hidden', false);
        return view;
    },
    render: function() {
        var view = this;
        var sig = view.model;

        view.$el.append(view.detailsInformation());

        view.updateHidden();
        
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
        div.append(view.detailsInformationField('email'));
        div.append(view.detailsInformationField('sigco'));

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
        var input = $('<input />');
        input.addClass('design-view-action-participant-details-information-field');
        input.val(value);
        input.attr('placeholder', 'Full name');
        return input;
    },
    detailsInformationField: function(name) {
        var view = this;
        var sig = view.model;

        var field = sig.field(name);
        if(!field)
            return null;

        var value = field.value();
        
        var input = $('<input />');
        input.addClass('design-view-action-participant-details-information-field');
        input.val(value);
        input.attr('placeholder', name);
        
        return input;
    }
});

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

        sig.bind('change', view.render);
        viewmodel.bind('change:participantDetail', view.updateOpened);
        view.render();
    },
    render: function() {
        var view = this;
        var sig = view.model;
        var viewmodel = view.viewmodel;

        var div = $('<div />');

        div.append(view.closeBox());

        div.append(view.inner());

        view.updateOpened();

        view.$el.html(div.children());
        return view;
    },
    open: function() {
        var view = this;
        if(!view.$el.hasClass('open'))
            view.$el.addClass('open');
        return view;
    },
    close: function() {
        var view = this;
        return view.$el.removeClass('open');
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

        return div;
    },
    closeBox: function() {
        return $('<div />')
            .addClass('design-view-action-participant-close')
            .text('x');
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
        return $('<div />')
            .addClass('design-view-action-participant-info-name')
            .append($('<div />')
                    .addClass('design-view-action-participant-info-name-inner')
                    .text(view.model.name()));
    },
    color: function() {
        var view = this;
        return $('<div />')
            .addClass('design-view-action-participant-info-color')
            .text('');
    },
    email: function() {
        var view = this;
        var model = view.model;
        return $('<div />')
            .addClass('design-view-action-participant-info-email')
            .append($('<div />')
                    .addClass('design-view-action-participant-info-email-inner')
                    .text(model.email()));
    },
    company: function() {
        var view = this;
        var sig = view.model;
        return $('<div />')
            .addClass('design-view-action-participant-info-company')
            .append($('<div />')
                    .addClass('design-view-action-participant-info-company-inner')
                    .text(sig.company()));
    }
});

    window.DesignViewParticipantsView = function(args) {
        return new DesignViewParticipantsView(args);
    }

}(window));
