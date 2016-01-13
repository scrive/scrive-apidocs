/* Document viewer is person that is looking at document - used to identify current signatory
 */

define(['Backbone', 'legacy_code'], function(Backbone) {

window.DocumentViewer = Backbone.Model.extend({
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

});
