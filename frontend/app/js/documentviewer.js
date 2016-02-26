var Backbone = require("backbone");

/* Document viewer is person that is looking at document - used to identify current signatory
 */


var DocumentViewer = exports.DocumentViewer = Backbone.Model.extend({
    authorcompanyadmin : function() {
        return this.get("authorcompanyadmin");
    },
    signatoryid: function() {
      return this.get("signatoryid");
    },
    forFetch: function() {
        return {
            signatoryid: this.signatoryid()
        };
    }
});

