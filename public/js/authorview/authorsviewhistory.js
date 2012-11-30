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
  text: function() {
    var document = this.document();
    if (this.document().currentViewerIsAuthor() && this.document().currentSignatoryCanSign()) {
      return localization.authorview.signNow;
    } else if (document.isSignedAndClosed()) {
      return localization.authorview.signedAndClosed;
    } else if (document.closed()) {
      return localization.authorview.closed;
    } else if (document.isSignedNotClosed()) {
      return localization.authorview.signedNotClosed;
    } else if (document.canceled()) {
      return localization.authorview.canceled;
    } else if (document.rejected()) {
      return localization.authorview.rejected;
    } else if (document.timedout()) {
      return localization.authorview.timeouted;
    } else {
      console.error("Unsure what state we're in");
      return ""
    }
    
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
        $(this.el).append($("<div class='headline'/>").text(this.model.text()));
        $(this.el).append(this.model.history().el());
    }

});

window.AuthorViewHistory = function(args) {
          var model = new AuthorViewHistoryModel(args);
          var view =  new AuthorViewHistoryView({model : model, el : $("<div class='history-box'/>")});
          this.el = function() {return $(view.el);};

};


})(window);
