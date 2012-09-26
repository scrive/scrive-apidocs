/* Whole document schema
 */


(function(window){
/*
 * Document has id and knows if its signed
 */


window.Process = Backbone.Model.extend({
    corename : function() { // The name of process used in comunication with server
        return this.get("corename");
    },
    localization: function() {
        var l = localization.process;
        switch (this.corename()) {
        case "Offer":
            return l.offer;
        case "Order":
            return l.order;
        default:
            return l.contract;
        }
    },
    numberedsignatories : function() {
        return this.get("numberedsignatories");
    }
});

window.Region = Backbone.Model.extend({
    region : function() {
        return this.get("region");
    },
    gb : function(){
        return this.region() == "gb";
    },
    se : function(){
        return this.region() == "se";
    },
    setGB : function() {
        this.set({region : "gb"});
    },
    setSE : function() {
        this.set({region : "se"});
    },
    draftData : function() {
        return this.region();
    }
});

})(window);
