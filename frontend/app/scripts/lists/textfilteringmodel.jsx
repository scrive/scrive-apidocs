/** @jsx React.DOM */

define(['legacy_code'], function() {


return Backbone.Model.extend({
        defaults: {
            text: ""
        },
        text: function() {
            return this.get("text");
        },
        setText: function(text) {
            this.set({ text: text });
        }
    });
});

