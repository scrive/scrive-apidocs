/* Main archive definition. Its a tab based set of different documents lists. */
define(['Backbone', 'legacy_code'], function() {

var StatsModel = Backbone.Model.extend({
  withCompany : function() {
     return this.get("withCompany");
  },
  userid : function() {
    return this.get("userid");
  },
  companyid : function() {
    return this.get("companyid");
  },
  dayTableUrl : function() {
     if (this.userid())
       return "/adminonly/useradmin/usagestats/days/"+this.userid();
     else if (this.companyid())
       return "/adminonly/companyadmin/usagestats/days/"+this.companyid();
     else
       return  "/account/usagestats/days/json" ;
  },
  dayTable : function() {
        if (this.get("dayTable") != undefined) return this.get("dayTable");
        this.set({ "dayTable" : new KontraList({
              name : "Past Month Table",
              loadOnInit : false,
              schema: new Schema({
                extraParams : (this.withCompany() ? {withCompany : "true"} : {}),
                url:this.dayTableUrl(),
                cells : _.flatten([
                    [new Cell({name: localization.account.stats.columnDate,   width:"100px", field:"date"})],
                    (this.withCompany() ? [new Cell({name:  localization.account.stats.columnSender, width:"100px", field:"name", special:"expandable"})] : []),
                    [new Cell({name:  localization.account.stats.columnClosedDocuments,  width:"70px",  field:"closed", tdclass: 'num'})],
                    [new Cell({name:  localization.account.stats.columnSendDocuments,    width:"70px",  field:"sent", tdclass: 'num'})],
                    [new Cell({name:  localization.account.stats.columnClosedSignatures, width:"70px",  field:"signatures", tdclass: 'num'})]
                ])})
          })
        });
        return this.dayTable();
  },
  monthTableUrl : function() {
     if (this.userid())
       return "/adminonly/useradmin/usagestats/months/"+this.userid();
     else if (this.companyid())
       return "/adminonly/companyadmin/usagestats/months/"+this.companyid();
     else
       return  "/account/usagestats/months/json" ;
  },
  monthTable : function() {
        if (this.get("monthTable") != undefined) return this.get("monthTable");
        this.set({ "monthTable" :   new KontraList({
              name : "Past 6 Month Table",
              loadOnInit : false,
              schema: new Schema({
                extraParams : (this.withCompany() ? {withCompany : "true"} : {}),
                url: this.monthTableUrl(),
                cells : _.flatten([
                    [new Cell({name: localization.account.stats.columnMonth,   width:"100px", field:"date"})],
                    (this.withCompany() ? [new Cell({name: localization.account.stats.columnSender, width:"100px", field:"name", special:"expandable"})] : []),
                    [new Cell({name: localization.account.stats.columnClosedDocuments,  width:"70px",  field:"closed", tdclass: 'num'})],
                    [new Cell({name: localization.account.stats.columnSendDocuments,    width:"70px",  field:"sent", tdclass: 'num'})],
                    [new Cell({name: localization.account.stats.columnClosedSignatures, width:"70px",  field:"signatures", tdclass: 'num'})]
                ])})
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
        this.render();
    },
    render: function () {
       var container = $(this.el);
       var model = this.model;
       var subbox = $("<div class='tab-content account usagestats'/>");
       subbox.append($("<h2/>").text(localization.account.stats.last30days));
       subbox.append($("<div class='jsdaytable'></div>").append(model.dayTable().el()));
       subbox.append($("<h2/>").text(localization.account.stats.last6months));
       subbox.append($("<div class='jsmonthtable'></div>").append(model.monthTable().el()));
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

});
