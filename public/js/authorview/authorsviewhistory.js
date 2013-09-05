(function(window){

var AuthorViewHistoryModel = Backbone.Model.extend({
  defaults : {
    dontRefresh : false
  },
  initialize: function (args) {
  },
  destroy : function() {
    this.off();
    this.history().destroy();
    this.clear();
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
    } else if (document.pending()) {
      return localization.authorview.pending;
    } else if (document.canceled()) {
      return localization.authorview.canceled;
    } else if (document.rejected()) {
      return localization.authorview.rejected;
    } else if (document.timedout()) {
      return localization.authorview.timeouted;
    } else {
      console.error("Unsure what state we're in");
      return "";
    }

  },
  history : function() {
    if (this.get("history") == undefined)
      this.set({"history" : new DocumentHistory({
                                      document : this.document()
                                    })}, {silent : true});
    return this.get("history");
  },
  dontRefresh : function() {
    return this.get("dontRefresh");
  },
  setDontRefresh : function() {
    this.set({"dontRefresh" : true });
  },
  checkIfHistoryHasChangedAndRefresh : function() {
    var self = this;
    this.history().checkIfHistoryChangedAndCallback( function() {
                                        self.authorview().reload();
                                      });
  },
  ready : function() {
    return this.history().ready() && this.document().ready();
  }
});

var AuthorViewHistoryView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    destroy : function() {
        this.model.setDontRefresh();
        this.model.destroy();
        $(this.el).remove();
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
          this.ready = function() {return model.ready()};
          this.setDontRefresh = function() { model.setDontRefresh();};
          this.expanded = function() { return model.history().expanded();};
          this.setExpanded = function(expanded) { model.history().setExpanded(expanded);};
          this.destroy = function() {view.destroy();}
          var checkAndRefresh = function(i) {
                  if (model.dontRefresh())
                    return; // No checkAndRefresh will be called anymore if this happends
                  model.checkIfHistoryHasChangedAndRefresh();
                  setTimeout(function() {checkAndRefresh(i> 30 ? 30 : i+1);},i * 1000);
          };
          checkAndRefresh(1);

};


})(window);
