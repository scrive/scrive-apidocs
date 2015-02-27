define(['Backbone', 'legacy_code'], function(Backbone) {

window.TextTypeSetterOptionsView = Backbone.View.extend({
    initialize: function(args) {
        var view = this;
        view.options = args.options;
        view.extraClass = args.extraClass;
        var field = view.model;
        _.bindAll(this, 'render');
        view.render();
        if(field) {
            this.listenTo(field,'change:obligatory change:shouldbefilledbysender', view.render);
            this.listenTo(field.signatory(),'change:delivery change:confirmationdelivery change:authentication', view.render);
        }
    },
    render: function() {
        var view = this;
        var field = view.model;
        var selected,selectedName;
        if(!field) {
            selected = 'optional';
            selectedName = localization.designview.optionalField;
        } else if(field.isOptional()) {
            selected = 'optional';
            selectedName = localization.designview.optionalField;
        } else if(field.shouldbefilledbysender()) {
            selected = 'sender';
            selectedName = localization.designview.mandatoryForSender;
        } else {
            selected = 'signatory';
            selectedName = localization.designview.mandatoryForRecipient;
        }

        var options = [];
        if (selected != 'sender')
          options.push({ name : localization.designview.mandatoryForSender,
                         value : 'sender'
                        });
        if (selected != 'signatory' && field.canBeSetByRecipent())
          options.push({ name : localization.designview.mandatoryForRecipient,
                         value : 'signatory'
                        });
        if (selected != 'optional' && field.canBeOptional())
          options.push({ name : localization.designview.optionalField,
                         value : 'optional'
                        });


        var select = new Select({
            options: options,
            inactive: (options.length == 0),
            name: selectedName,
            cssClass : (view.extraClass || ""),
            style: 'font-size: 16px;',
            textWidth: 191,
            optionsWidth: "218px",
            onSelect: function(v) {
                if(field) {
                    mixpanel.track('Choose obligation', {
                        Subcontext: 'inline'
                    });
                    if(v === 'optional') {
                        field.makeOptional();
                        field.setShouldBeFilledBySender(false);
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
        $(view.el).empty().append((select.el()));
        return view;
    }
});

});
