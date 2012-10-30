
(function(window){

var  CreateFromTemplateModel = Backbone.Model.extend({
  defaults: {
  }
});

var CreateFromTemplateView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  render: function() {
     var model = this.model
     return this;
  }
});

window.CreateFromTemplate = function(args) {
    var model = new CreateFromTemplateModel(args);
    var view = new CreateFromTemplateView({ model: model});
    return {
      el: function() { return $(view.el); }
    };
};


})(window);
