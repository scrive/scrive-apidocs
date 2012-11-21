(function(window){

var AuthorViewSignatoriesModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  }
});

var AuthorViewSignatoriesView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {

    }

});

window.AuthorViewSignatories = function(args) {
          var model = new AuthorViewSignatoriesModel(args);
          var view =  new AuthorViewSignatoriesView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);