(function(window){

var AuthorViewTitleBoxModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  }
});

var AuthorViewTitleBoxView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {

    }

});

window.AuthorViewTitleBox = function(args) {
          var model = new AuthorViewTitleBoxModel(args);
          var view =  new AuthorViewTitleBoxView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);