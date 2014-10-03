/** @jsx React.DOM */

define(['legacy_code'], function() {

return Backbone.Model.extend({
        defaults: {
            filters: []
        },
        filters : function() {
          return this.get("filters");
        },
        filteringValue: function(name) {
            var res = "";
            _.each(this.filters(),function(f) {
              if (f.name == name)
                res = f.value;
            });
            return res;
        },
        setFilter: function(name,value) {
            var isSet = false;
            for(var i=0;i< this.filters().length;i++) {
              if (this.filters()[i].name == name) {
                isSet = true;
                this.filters()[i].value = value;
              }
            }
            if (!isSet)
              this.filters().push({name : name, value : value});
            this.trigger("change");
        },
        valueToMaybe : function(v) {
          if (v == undefined || v == "")
            return "Nothing";
          return ("Just " + v);
        },

        asJSON : function() {
          var self = this;
          var resfilters = [];
          // Hack for double filters. We need to encode them in strange way to match old API version. But with new version we will be able to fix it.
          _.each(self.get("filters"),function(f) {
            if (f.name.indexOf("from_") == 0) {
              var name = f.name.substr("from_".length);
              if (_.find(resfilters,function(f) {return f.name == name;}))
                return;
              var toFilter = self.filteringValue("to_" + name);
              resfilters.push({name : name, value : "("+self.valueToMaybe(f.value) + "," +self.valueToMaybe(toFilter) + ")"});

            }
            else if (f.name.indexOf('to_') == 0) {
              var name = f.name.substr("to_".length);
              if (_.find(resfilters,function(f) {return f.name == name;}))
                return;
              var fromFilter =  self.filteringValue("from_" + name);
              resfilters.push({name : name, value : "("+  self.valueToMaybe(fromFilter)  + "," + self.valueToMaybe(f.value)  + ")"});
            }
            else {
                resfilters.push(f);
            }
          });
          return JSON.stringify(resfilters);
        }
    });




});

