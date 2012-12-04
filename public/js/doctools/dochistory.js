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
                new Cell({name: localization.history.time,  width:"60px",  field:"time"}),
                new Cell({name: localization.history.party, width:"80px", field:"party"}),
                new Cell({name: localization.history.description, width:"200px",  field:"text" ,special: "rendered",
                          rendering: function(value) {
                            return $("<div>").html(value);
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
  showAll : function() {
      return this.get("showAll");
  },
  toogleShowAll : function() {
      this.set({'showAll' : !this.showAll() }, {silent : true});
      this.historyList().setShowLimit(this.get("showAll") ? undefined : 5);
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
    updateOption : function() {
         if (this.model.showAll()) {
            this.checkbox.addClass("expanded");
            this.label.text(localization.history.hide)
         }   
        else {
            this.checkbox.removeClass("expanded");
            this.label.text(localization.history.expand + " (" + (this.model.historyList().model().length - 5) + " " +localization.history.available+ ")")
        }    
    },
    expandAllOption : function() {
        var model = this.model;
        var view = this;
        var option = $("<div class='option'/>")
        this.checkbox = $("<div class='expandable'>");
        this.label = $("<div class='label'/>");
        this.updateOption();
        option.click(function() {
            model.toogleShowAll();
            view.updateOption();
            return false;
        })
        return option.append(this.checkbox).append(this.label);
    },
    render: function() {
      var self = this;
      var container = $(this.el)
      container.children().detach();
      var historyList = this.model.historyList();
      
      container.append(historyList.el())

      var footer = $("<div class='document-history-footer'/>");
      historyList.model().bind('reset', function() {
        if (historyList.model().length <= 5)
            footer.hide();
        else
            self.updateOption();
          
      })
      footer.append(this.expandAllOption())
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
};

})(window);
