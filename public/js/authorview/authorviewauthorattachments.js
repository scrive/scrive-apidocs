(function(window){

var AuthorViewAuthorAttachmentsModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  }
});

var AuthorViewAuthorAttachmentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {

    }

});

window.AuthorViewAuthorAttachments = function(args) {
          var model = new AuthorViewAuthorAttachmentsModel(args);
          var view =  new AuthorViewAuthorAttachmentsView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);