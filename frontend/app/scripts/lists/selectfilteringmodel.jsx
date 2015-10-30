/** @jsx React.DOM */

define(['legacy_code'], function() {

return Backbone.Model.extend({
        defaults: function () {
          return {filters: []};
        },
        filters : function() {
          return this.get("filters");
        },
        filteringValue: function(name) {
            var res;
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
        }
    });




});

