/* Whole document schema
 */


(function(window){
/*
 * Document has id and knows if its signed
 */


window.Process = Backbone.Model.extend({
    defaults : {
      process : "Contract"
    },
    name : function() {
        return this.get("process");
    },
    process : function() {
        return this.get("process");
    },
    isContract : function() {
      return this.process() == "Contract";
    },
    isOffer : function() {
      return this.process() == "Offer";
    },
    isOrder : function() {
      return this.process() == "Order";
    },
    changeToContract : function() {
      this.set({"process": "Contract"});
    },
    changeToOffer : function() {
       this.set({"process": "Offer"});
    },
    changeToOrder : function() {
       this.set({"process": "Order"});
    },
    localization: function() {
        var l = localization.process;
        if (this.isOffer())
            return l.offer;
        else if (this.isOrder())
            return l.order;
        else
            return l.contract;
    },
    numberedsignatories : function() {
       return true;
    }
});

window.Lang = Backbone.Model.extend({
    lang : function() {
        return this.get("lang");
    },
    en : function(){
        return this.lang() == "en";
    },
    sv : function(){
        return this.lang() == "sv";
    },
    setEN : function() {
        this.set({lang : "en"});
    },
    setSV : function() {
        this.set({lang : "sv"});
    },
    draftData : function() {
        return this.lang();
    }
});

})(window);
