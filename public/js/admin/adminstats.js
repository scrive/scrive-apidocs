// The admin stats view, very similar to the user/company stats view.
// Not happy about this.

(function(window){

var StatsModel = Backbone.Model.extend({
    dayTable : function() {
        if (this.get("dayTable") != undefined) return this.get("dayTable");

        this.set({"dayTable":
            new KontraList({
                name: "Past Month Table",
                timeout: 30000,
                schema: new Schema({
                    url: "/adminonly/statsbyday",
                    cells: [
                        new Cell({name: "Date",
                            width: "100px", field: "date"}),
                        new Cell({name: "Closed documents",
                            width: "70px", field: "closed", tdclass: "num"}),
                        new Cell({name: "Sent documents",
                            width: "70px", field: "sent", tdclass: "num"}),
                        new Cell({name: "Closed signatures",
                            width: "70px", field: "signatures", tdclass: "num"}),
                        new Cell({name: "Average signatures per document",
                            width: "70px", field: "avg", tdclass: "num",
                            rendering: function(_, _, doc) {
                                return jQuery("<span>").text(doc.field("avg").toFixed(2));
                            }
                        }),
                        new Cell({name: "New users (TOS)",
                            width: "70px",  field: "users", tdclass: "num"})
                    ]
                })
            })});
        return this.dayTable();
    },

    monthTable : function() {
        if (this.get("monthTable") != undefined) return this.get("monthTable");

        this.set({"monthTable":
            new KontraList({
                name: "Past 6 Months Table",
                timeout: 30000,
                schema: new Schema({
                    url: "/adminonly/statsbymonth",
                    cells: [
                        new Cell({name: "Month",
                            width: "100px", field: "date"}),
                        new Cell({name: "Closed documents",
                            width: "70px", field: "closed", tdclass: "num"}),
                        new Cell({name: "Sent documents",
                            width: "70px", field: "sent", tdclass: "num"}),
                        new Cell({name: "Closed signatures",
                            width: "70px", field: "signatures", tdclass: "num"}),
                        new Cell({name: "Average signatures per document",
                            width: "70px", field: "avg", tdclass: "num",
                            rendering: function(_, _, doc) {
                                return jQuery("<span>").text(doc.field("avg").toFixed(2));
                            }
                        }),
                        new Cell({name: "New users (TOS)",
                            width: "70px",  field: "users", tdclass: "num"})
                    ]
                })
            })});
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
       subbox.append($("<h2/>").text("30 days"));
       subbox.append($("<div class='jsdaytable'></div>").append(model.dayTable().el()));
       subbox.append($("<h2/>").text("6 months"));
       subbox.append($("<div class='jsmonthtable'></div>").append(model.monthTable().el()));
       container.append(subbox);
       return this;
    }
});

// Don't override window.Stats
window.AdminStats = function(args) {
    var model = new StatsModel(args);
    var view = new StatsView({model: model, el: $("<div class='tab-container' style='padding: 30px;background: none repeat scroll 0 0 #FFFFFF;'/>")});
    return {
        refresh: function() { return model.refresh() },
        el: function() {return $(view.el);}
    };
};

})(window);
