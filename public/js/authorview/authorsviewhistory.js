(function(window){

var AuthorViewHistoryModel = Backbone.Model.extend({
  defaults : {
  },
  initialize: function (args) {
  },
  document :function() {
     return this.authorview().document();
  },
  authorview : function() {
     return this.get("authorview"); 
  },
  history : function() {
    if (this.get("history") == undefined)
      this.set({"history" : new DocumentHistory({document : this.document()})}, {silent : true});
    return this.get("history");
  },
});

var AuthorViewHistoryView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
        $(this.el).append(this.model.history().el());
    }

});

window.AuthorViewHistory = function(args) {
          var model = new AuthorViewHistoryModel(args);
          var view =  new AuthorViewHistoryView({model : model, el : $("<div class='history-box'/>")});
          this.el = function() {return $(view.el);};

};


})(window);
