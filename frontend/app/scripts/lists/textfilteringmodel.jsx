var Backbone = require("backbone");



module.exports = Backbone.Model.extend({
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
