// Represents a series of steps that guide data collection

// Written by Eric Normand

(function(window) {
    window.Wizard = Backbone.Model.extend({
        defaults: {
            currentStep: -1,
            steps: new Backbone.Collection
        },
        initialize: function() {
            var wiz = this;
            var steps = wiz.get('steps');

        },
        hasCurrentStep: function() {
            console.log("currentStep: " + this.get('currentStep'));
            return this.get('currentStep') !== -1;
        },
        getCurrentStep: function() {
            return this.get('currentStep');
        },
        setCurrentStep: function(i) {
            console.log("setCurrentStep: " + i);
            this.set({'currentStep': i});
            console.log("after");
            return this;
        },
        nextStep: function() {
            return this.setCurrentStep(this.getCurrentStep() + 1);
        },
        previousStep: function() {
            return this.setCurrentStep(this.getCurrentStep() - 1);
        },
        currentStep: function() {
            console.log("currentStep; currentStep = " + this.getCurrentStep());
            return this.get('steps').at(this.getCurrentStep());
        },
        addStep: function(s) {
            console.log("addStep");
            var wiz = this;
            if(wiz.getCurrentStep() === -1)
                wiz.set({'currentStep': 0}, {silent:true});
            s.set({wizard : wiz}, {silent: true});
            console.log("before add");
            this.get('steps').add(s);
            console.log("addStep: now we have: " + this.get('steps').length);
            this.trigger('change');

            return this;
        }
    });

    window.WizardView = Backbone.View.extend({
        initialize: function (args) {
            console.log('initialize wizardview');
            console.log(this);
            _.bindAll(this, 'render');
            this.model.bind('change', this.render);
            this.model.view = this;
        },
        render: function() {
            console.log("wizardview render");
            console.log(this);
            var wiz = this.model;
            $(this.el).children().detach();
            if(wiz.hasCurrentStep()) {
                console.log("wizard has current step");
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
        getIndex: function() {
            return this.get('wizard').indexOf(this);
        }
    });
})(window);
