var React = require("react");
var StatsTable = require("../../../scripts/account/usersandstats/statstable");
var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");

/* Main archive definition. Its a tab based set of different documents lists. */

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
  dayTableDef : function() {
    return React.createElement(StatsTable,{
      withCompany: this.withCompany(),
      url : this.dayTableUrl() + (this.withCompany() ? "?withCompany=true" : "")
   });
  },
  monthTableUrl : function() {
     if (this.userid())
       return "/adminonly/useradmin/usagestats/months/"+this.userid();
     else if (this.companyid())
       return "/adminonly/companyadmin/usagestats/months/"+this.companyid();
     else
       return  "/account/usagestats/months/json" ;
  },
  monthTableDef : function() {
    return React.createElement(StatsTable,{
      withCompany: this.withCompany(),
      url : this.monthTableUrl() + (this.withCompany() ? "?withCompany=true" : "")
    });
  }
});



var StatsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    refresh : function() {
      if(this.daytable != undefined)
        this.daytable.reload();
      if(this.monthtable != undefined)
        this.monthtable.reload();
    },
    render: function () {
       var container = $(this.el);
       var model = this.model;
       var subbox = $("<div class='tab-content account usagestats'/>");

       subbox.append($("<h2/>").text(localization.account.stats.last30days));
       var daytable = $("<div class='jsdaytable'></div>");
       subbox.append($(daytable));
       this.daytable = React.render(model.dayTableDef(),daytable[0]);

       subbox.append($("<h2/>").text(localization.account.stats.last6months));
       var monthstable = $("<div class='jsmonthtable'></div>");
       subbox.append($(monthstable));
       this.monthtable = React.render(model.monthTableDef(),monthstable[0]);

       container.append(subbox);
       return this;
    }
});


var Stats = exports.Stats = function(args) {
          var model = new StatsModel(args);
          var view =  new StatsView({model : model, el : $("<div class='tab-container'/>")});
          return {
              refresh : function() {view.refresh();},
              el  : function() {return $(view.el);}
            };
};

