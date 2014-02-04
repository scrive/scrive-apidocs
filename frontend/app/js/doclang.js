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
      else if (this.de())
        return "de";
    },
    en : function(){
        return this.lang() == "en" || this.lang() == "gb";
    },
    sv : function(){
        return this.lang() == "sv" || this.lang() == "se";
    },
    de : function(){
        return this.lang() == "de";
    },
    setEN : function() {
        this.set({lang : "en"});
    },
    setSV : function() {
        this.set({lang : "sv"});
    },
    setDE : function() {
        this.set({lang : "de"});
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
