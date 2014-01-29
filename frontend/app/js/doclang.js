/* Whole document schema
 */

define(['Backbone', 'legacy_code'], function() {

window.DocLang = Backbone.Model.extend({
    lang : function() {
        return this.get("lang");
    },
    simpleCode : function() {
      if (this.en())
        return "en";
      else if (this.sv())
        return "sv";
    },
    en : function(){
        return this.lang() == "en" || this.lang() == "gb";
    },
    sv : function(){
        return this.lang() == "sv" || this.lang() == "se";
    },
    setEN : function() {
        this.set({lang : "en"});
    },
    setSV : function() {
        this.set({lang : "sv"});
    },
    draftData : function() {
        return this.lang();
    },
    setLanguage: function(l) {
        this.set({lang:l});
        return this;
    }
});

});
