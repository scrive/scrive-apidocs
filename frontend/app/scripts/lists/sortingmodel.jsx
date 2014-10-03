/** @jsx React.DOM */

define(['legacy_code'], function() {


return Backbone.Model.extend({
        defaults: {
            current: undefined,
            order: true
        },
        isCurrent: function(field) {
            return this.get("current") != undefined && this.get("current") == field;
        },
        current: function() {
            return this.get("current");
        },
        isAsc: function() {
            return this.get("order") != undefined && this.get("order") == true;
        },
        isDesc: function() {
            return this.get("order") != undefined && this.get("order") == false;
        },
        sortOn: function(field) {
            if (this.isCurrent(field)) {
                this.set({ order: !this.get("order") });
            } else {
                this.set({order: true, current: field});
            }
        }
  });
});

