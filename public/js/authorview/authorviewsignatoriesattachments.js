(function(window){

var AuthorViewSignatoriesAttachmentsModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document :function() {
     return this.get("document");
  }
});

var AuthorViewSignatoriesAttachmentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {

    }

});

window.AuthorViewSignatoriesAttachments = function(args) {
          var model = new AuthorViewSignatoriesAttachmentsModel(args);
          var view =  new AuthorViewSignatoriesAttachmentsView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};

};


})(window);