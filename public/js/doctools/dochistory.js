/* Signatory view of document
 */


(function(window) {

var DocumentHistoryModel = Backbone.Model.extend({
  defaults : {
      expanded : false
  },
  initialize : function(){
  },
  destroy : function() {
    this.off();
    this.stopListening();
    this.historyList().destroy();
    this.clear();
  },
  newHistoryList : function() {
     var list = new KontraList({
        name : "Document history",
        schema: new Schema({
            url: "/api/frontend/history/" + this.document().documentid(),
            paging : new Paging({disabled: true, showLimit : this.get("expanded") ? undefined : 15 }),
            cells : [
                new Cell({name: localization.archive.documents.columns.status, width:"46px", field:"status",
                  rendering: function(status) {
                      return jQuery("<div class='icon status "+status+"'></div>");
                  }
                }),
                new Cell({name: localization.history.time,  width:"150px",  field:"time",
                  rendering: function(time) {
                         if (time != undefined && time != "")
                           return $("<div/>").text(new Date(Date.parse(time)).toTimeAbrev());
                         else return $("<div/>");
                  }
                }),
                new Cell({name: localization.history.party, width:"200px", field:"party"}),
                new Cell({name: localization.history.description, width:"460px",  field:"text" ,special: "rendered",
                          rendering: function(value) {
                            return $("<div style='margin-right:30px'>").html(value);
                          }})
                ]
            })

    });
    this.listenTo(list.model(),"change", function() {this.trigger("change:history")});
    this.listenTo(list.model(),"reset",  function() {this.trigger("change:history")});
    return list;

  },
  historyList : function() {
        if (this.get("historyList") == undefined)
            this.set({'historyList' : this.newHistoryList() }, {silent : true});
        return this.get('historyList');

  },
  checkIfHistoryChangedAndCallback : function(callback) {
      if (!this.historyList().ready()) { console.log("Not reloading, because list is not ready"); return; }// If we don't have current history, we can't really say that it changed
      this.historyList().fetchWithCallback(function(currentlist,newlist) {
         console.log("List fetched, and it is changed: " + (currentlist.length != newlist.length));
         if (currentlist.length != newlist.length)
           callback();
      });
  },
  expanded : function() {
      return this.get("expanded");
  },
  setExpanded : function(bool) {
      this.set({'expanded' : bool }, {silent : true});
      this.historyList().setShowLimit(bool ? undefined : 15);
      this.trigger("change:history");
  },
  document : function(){
       return this.get("document");
  },
  ready : function() {
    console.log("Checking if list is ready " + this.historyList().ready());
    return this.historyList().ready();
  }
});

var DocumentHistoryView = Backbone.View.extend({
    initialize: function(args) {
        var self = this;
        _.bindAll(this, 'render');
        this.listenTo(this.model,"change:history",self.render);
        this.render();
    },
    destroy : function() {
        this.stopListening();
        this.model.destroy();
        $(this.el).remove();
    },
    updateOption : function() {
         if (this.model.expanded()) {
            this.checkbox.addClass("expanded");
            this.label.text(localization.history.hide);
         }
        else {
            this.checkbox.removeClass("expanded");
            this.label.text(localization.history.expand + " (" + (this.model.historyList().model().length - 15) + " " +localization.history.available+ ")");
        }
    },
    expandAllOption : function() {
        var model = this.model;
        var view = this;
        var option = $("<div class='option'/>");
        this.checkbox = $("<div class='expandable'>");
        this.label = $("<div class='label'/>");
        this.updateOption();
        option.click(function() {
            model.setExpanded(!model.expanded());
            view.updateOption();
            return false;
        });
        return option.append(this.checkbox).append(this.label);
    },
    render: function() {
      var self = this;
      var container = $(this.el);
      this.model.historyList().el().detach();
      container.empty();

      var historyList = this.model.historyList();

      container.append(historyList.el());

      if (historyList.model().length >= 15) {
           var footer = $("<div class='document-history-footer'/>");
           footer.append(this.expandAllOption());
           container.append(footer);
           self.updateOption();
        }

      return this;
    }
});


window.DocumentHistory = function(args){
        var model = new DocumentHistoryModel( {
                        document : args.document
                    });
        var view = new DocumentHistoryView ({
                        model: model,
                        el: $("<div class='document-history-container'/>")
                    });
        this.el     = function() {return $(view.el);};
        this.recall = function() { model.recall();};
        this.expanded = function() { return model.expanded();};
        this.setExpanded = function(expanded) {model.setExpanded(expanded);};
        this.ready  = function() {return model.ready()};
        this.destroy = function() {view.destroy();this.checkIfHistoryChangedAndCallback = function() {};};
        this.checkIfHistoryChangedAndCallback = function(callback) {return model.checkIfHistoryChangedAndCallback(callback);};
};

})(window);
