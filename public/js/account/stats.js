/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){
 
var StatsModel = Backbone.Model.extend({

  dayTable : function() {
        if (this.get("dayTable") != undefined) return this.get("dayTable");
        this.set({ "dayTable" : KontraList().init({
              name : "Past Month Table",
              schema: new Schema({
                url: "/account/usagestats/days/json",
                cells : [
                    new Cell({name: "$_usageStatsDate()$",   width:"100px", field:"date"}),
                    //$if(iscompanyadmin)$new Cell({name: "$_statsSender()$", width:"100px", field:"name", special:"expandable"}),$endif$
                    new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", tdclass: 'num'}),
                    new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", tdclass: 'num'}),
                    new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", tdclass: 'num'})
                ]})
          })          
        });
        return this.dayTable();
  },
  monthTable : function() {
        if (this.get("monthTable") != undefined) return this.get("monthTable");
        this.set({ "monthTable" :   KontraList().init({
              name : "Past 6 Month Table",
              schema: new Schema({
              url: "/account/usagestats/months/json",
              cells : [
                  new Cell({name: "$_usageStatsMonth()$",   width:"100px", field:"date"}),
                  //$if(iscompanyadmin)$new Cell({name: "$_statsSender()$", width:"100px", field:"name", special:"expandable"}),$endif$
                  new Cell({name: "$_closedDocuments()$",  width:"70px",  field:"closed", tdclass: 'num'}),
                  new Cell({name: "$_sentDocuments()$",    width:"70px",  field:"sent", tdclass: 'num'}),
                  new Cell({name: "$_closedSignatures()$", width:"70px",  field:"signatures", tdclass: 'num'})
              ]})
          })
        });
        return this.monthTable();
  },
  refresh : function() {
                    this.dayTable().recall();
                    this.monthTable().recall();
  }
});



var StatsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        var view = this;
        this.render();
    },
    render: function () {
       var container = $(this.el);
       var model = this.model;
       var subbox = $("<div class='tab-content account usagestats'/>");
       subbox.append("<h2>$_last30Days()$</h2>");
       subbox.append($("<div class='jsdaytable'></div>").append(model.dayTable().view.el));
       subbox.append("<h2>$_last6Months()$</h2>");
       subbox.append($("<div class='jsmonthtable'></div>").append(model.monthTable().view.el));
       container.append(subbox);
       return this;
    }
});


window.Stats = function(args) {
          var model = new StatsModel(args);
          var view =  new StatsView({model : model, el : $("<div class='tab-container'/>")});
          return {
              refresh : function() {model.refresh();},
              el  : function() {return $(view.el);}
            };
};

})(window);
