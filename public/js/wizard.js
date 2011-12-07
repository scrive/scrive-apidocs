// Represents a series of steps that guide data collection

// Written by Eric Normand

(function(window) {
    window.Wizard = Backbone.Model.extend({
        defaults: {
            currentStep: -1,
            steps: new Backbone.Collection
        },
        initialize: function() {

        },
        steps: function() {
            return this.get('steps');
        },
        hasCurrentStep: function() {
            return this.stepIndex() !== -1;
        },
        stepIndex: function() {
            return this.get('currentStep');
        },
        setStepIndex: function(i) {
            this.set({'currentStep': i});
            return this;
        },
        nextStep: function() {
            return this.setStepIndex(this.stepIndex() + 1);
        },
        previousStep: function() {
            return this.setStepIndex(this.stepIndex() - 1);
        },
        currentStep: function() {
            return this.steps().at(this.stepIndex());
        },
        addStep: function(s) {
            var wiz = this;
            if(!wiz.hasCurrentStep())
                // sorry for the set, but I need to make it silent!
                wiz.set({'currentStep': 0}, {silent:true});
            s.set({wizard : wiz}, {silent: true});
            this.steps().add(s);
            this.trigger('change');
            return this;
        }
    });

    window.WizardView = Backbone.View.extend({
        initialize: function (args) {
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.model.view = this;
        },
        render: function() {
            var wiz = this.model;
            $(this.el).children().detach();
            if(wiz.hasCurrentStep()) {
                $(this.el).append(wiz.currentStep().view.el);
                wiz.currentStep().view.render();
            }
        }
    });

    window.WizardStep = Backbone.Model.extend({
        defaults: {
            wizard: null
        },
        initialize: function() {
            var step = this;
            step.view = function() { return new Backbone.View; };
        },
        myIndex: function() {
            return this.wizard().steps().indexOf(this);
        },
        wizard: function() {
            return this.get('wizard');
        },
        setWizard: function(wiz) {
            this.set({'wizard':wiz});
            return this;
        }
    });
})(window);
