/* Progressbar
 */

(function(window){

var ProgressBarModel = Backbone.Model.extend({
  defaults: {
    cssClass: 'four-circle-progress-bar',
    currentStep: 1
  },
  step: function(){
    return this.get("currentStep");
  },
  extraClass: function(){
    return this.get("cssClass");
  }
});

var ProgressBarView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.model.bind('change:currentStep', this.render);
        this.render();
    },
    render: function () {
       var bar = $(this.el);
       bar.addClass('progressbar').addClass(this.model.extraClass());

       this.inner = this.inner || $('<div class="inner">');
       var step = this.model.step();

       // TODO is there a nicer way?
       this.inner.removeClass('step-' + (step-1)).addClass('step-' + step);

       bar.append(this.inner);

       return bar;
    },
    hide: function() {
        $(this.el).hide();
    }
});

window.ProgressBar = function(args) {
    var model = new ProgressBarModel(args);
    var view = new ProgressBarView({model: model, el: $('<div/>')});

    return new Object({
        setStep: function(step) { model.set({'currentStep': step}); },
        hide: function() { view.hide(); },
        el: function() { return $(view.el); }
    });
};

})(window);
