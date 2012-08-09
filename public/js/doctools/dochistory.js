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
            url: "/d/evidencelog/" + this.document().documentid(),
            paging : new Paging({disabled: true, showLimit : this.get("showAll") ? undefined : 5 }),
            cells : [
                new Cell({name: "Time",  width:"80px",  field:"time"}),
                new Cell({name: "Party", width:"100px", field:"party"}),
                new Cell({name: "Description", width:"200px",  field:"text" })
                ]
            })
         
    });
  },
  historyList : function() {
        if (this.get("historyList") == undefined)
            this.set({'historyList' : this.newHistoryList() }, {silent : true});
        return this.get('historyList');
        
  },
  showAll : function() {
      return this.get("showAll");
  },
  toogleShowAll : function() {
      this.set({'showAll' : !this.showAll() }, {silent : true});
      this.historyList().model.schema.paging().setShowLimit(this.get("showAll") ? undefined : 5);
      this.historyList().model.schema.trigger("change");

  },
  document : function(){
       return this.get("document");
  }
});
var DocumentHistoryView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        var view = this;
        this.render();
    },
    updateCheckbox : function() {
         if (this.model.showAll())
            this.checkbox.addClass("checked");
        else
            this.checkbox.removeClass("checked");
    },
    expandAllOption : function() {
        var model = this.model;
        var view = this;
        var option = $("<div class='option'/>")
        this.checkbox = $("<div class='checkbox'>");
        var label = $("<div class='label'/>").text("Expand to view all");
        this.updateCheckbox();
        this.checkbox.click(function() {
            model.toogleShowAll();
            view.updateCheckbox();
            return false;
        })
        return option.append(this.checkbox).append(label);
    },
    render: function() {
      var container = $(this.el)
      container.children().detach();
      var historyList = this.model.historyList();
      var header = $("<div class='document-history-header'/>");
      var title = $("<div class='title'/>").text("Events:");
      var showAll = $("<div class='option'/>").text("Expand to view all");
      showAll.click(function() {
          historyList.model.schema.paging().setShowLimit(undefined);
          historyList.model.schema.trigger("change");
          this.render();
          return false;
      })
      header.append(title).append(this.expandAllOption())
      container.append(header);
      container.append(historyList.view.el)
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
        return {
              model    : function() {return model;}
            , view     : function() {return view;}
            , recall   : function() { model.recall();}
         };
};

})(window);
