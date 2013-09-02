/* Signatory view of document
 */


(function(window) {

var DocumentHistoryModel = Backbone.Model.extend({
  defaults : {
      showAll : false
  },
  initialize : function(){
  },
  newHistoryList : function() {
     return  new KontraList({
        name : "Document history",
        schema: new Schema({
            url: "/api/frontend/history/" + this.document().documentid(),
            paging : new Paging({disabled: true, showLimit : this.get("showAll") ? undefined : 15 }),
            cells : [
                new Cell({name: localization.archive.documents.columns.status, width:"46px", field:"status",
                  rendering: function(status) {
                      return jQuery("<div class='icon status "+status+"'></div>");
                  }
                }),
                new Cell({name: localization.history.time,  width:"150px",  field:"time",
                  rendering: function(time) {
                         if (time != undefined && time != "")
                           return $("<div/>").text(new Date(Date.parse(time)).fullTime());
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
  showAll : function() {
      return this.get("showAll");
  },
  toogleShowAll : function() {
      this.set({'showAll' : !this.showAll() }, {silent : true});
      this.historyList().setShowLimit(this.get("showAll") ? undefined : 15);
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
        this.model.historyList().model().bind("change", function() {self.render();});
        this.model.historyList().model().bind("reset", function() {self.render();});
        var view = this;
        this.render();
    },
    updateOption : function() {
         if (this.model.showAll()) {
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
            model.toogleShowAll();
            view.updateOption();
            return false;
        });
        return option.append(this.checkbox).append(this.label);
    },
    render: function() {
      var self = this;
      var container = $(this.el);
      container.children().detach();
      var historyList = this.model.historyList();

      container.append(historyList.el());

      var footer = $("<div class='document-history-footer'/>");
        if (historyList.model().length <= 15)
            footer.hide();
        else
            self.updateOption();
      footer.append(this.expandAllOption());
      container.append(footer);


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
        this.ready  = function() {return model.ready()};
        this.checkIfHistoryChangedAndCallback = function(callback) {return model.checkIfHistoryChangedAndCallback(callback);}
};

})(window);
