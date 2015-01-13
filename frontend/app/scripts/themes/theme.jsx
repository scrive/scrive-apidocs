/** @jsx React.DOM */

/* View model for  */
define(['Backbone', 'legacy_code'], function() {

return Backbone.Model.extend({
  defaults: {
   "id" : undefined,
   "name" : undefined,
   "logo" : undefined,
   "brandColor" : undefined,
   "brandTextColor" : undefined,
   "actionColor" : undefined,
   "actionTextColor" : undefined,
   "actionSecondaryColor" : undefined,
   "actionSecondaryTextColor" : undefined,
   "positiveColor" : undefined,
   "positiveTextColor" : undefined,
   "negativeColor" : undefined,
   "negativeTextColor" : undefined,
   "font" : undefined,
   "dirty" : false
  },
  initialize : function(args) {
    var self = this;
    if (args.listobject)
      this.set({
        "id" : args.listobject.field("id"),
        "name" : args.listobject.field("name"),
        "logo" : args.listobject.field("logo"),
        "brandColor": args.listobject.field("brandColor"),
        "brandTextColor" : args.listobject.field("brandTextColor"),
        "actionColor" : args.listobject.field("actionColor"),
        "actionTextColor" : args.listobject.field("actionTextColor"),
        "actionSecondaryColor" : args.listobject.field("actionSecondaryColor"),
        "actionSecondaryTextColor" : args.listobject.field("actionSecondaryTextColor"),
        "positiveColor" : args.listobject.field("positiveColor"),
        "positiveTextColor" : args.listobject.field("positiveTextColor"),
        "negativeColor" : args.listobject.field("negativeColor"),
        "negativeTextColor" : args.listobject.field("negativeTextColor"),
        "font" : args.listobject.field("font"),
        "listobject" : undefined,
        "dirty" : false
      });
  },
  themeid: function() {
    return this.get("id");
  },
  name: function() {
    return this.get("name");
  },
  setName: function(c) {
    this.set({"name":c,"dirty": true});
  },
  logo: function() {
    return this.get("logo");
  },
  setLogo: function(c) {
    this.set({"logo":c,"dirty": true});
  },
  brandColor: function() {
    return this.get("brandColor");
  },
  setBrandColor : function(c) {
    this.set({"brandColor":c,"dirty": true});
  },
  brandTextColor: function() {
    return this.get("brandTextColor");
  },
  setBrandTextColor : function(c) {
    this.set({"brandTextColor":c,"dirty": true});
  },
  actionColor: function() {
    return this.get("actionColor");
  },
  setActionColor : function(c) {
    this.set({"actionColor":c,"dirty": true});
  },
  actionTextColor: function() {
    return this.get("actionTextColor");
  },
  setActionTextColor : function(c) {
    this.set({"actionTextColor":c,"dirty": true});
  },
  actionSecondaryColor: function() {
    return this.get("actionSecondaryColor");
  },
  setActionSecondaryColor: function(c) {
    this.set({"actionSecondaryColor":c,"dirty": true});
  },
  actionSecondaryTextColor: function() {
    return this.get("actionSecondaryTextColor");
  },
  setActionSecondaryTextColor: function(c) {
    this.set({"actionSecondaryTextColor":c,"dirty": true});
  },
  positiveColor: function() {
    return this.get("positiveColor");
  },
  setPositiveColor: function(c) {
    this.set({"positiveColor":c,"dirty": true});
  },
  positiveTextColor: function() {
    return this.get("positiveTextColor");
  },
  setPositiveTextColor: function(c) {
    this.set({"positiveTextColor":c,"dirty": true});
  },
  negativeColor: function() {
    return this.get("negativeColor");
  },
  setNegativeColor: function(c) {
    this.set({"negativeColor":c,"dirty": true});
  },
  negativeTextColor: function() {
    return this.get("negativeTextColor");
  },
  setNegativeTextColor: function(c) {
    this.set({"negativeTextColor":c,"dirty": true});
  },
  font: function() {
    return this.get("font");
  },
  setFont : function(f) {
    this.set({"font":f,"dirty": true});
  },
  dirty: function() {
    return this.get("dirty");
  },
  save : function(url,callback) {
    var self = this;
    new Submit({
      url : url,
      method : "POST",
      ajax : true,
      ajaxsuccess : function() {
        self.set({"dirty" : false});
        callback();
      },
      theme : JSON.stringify(self.attributes)
    }).send();
  }
});


});
