var Backbone = require("backbone");
var Submit = require("../../../js/submits.js").Submit;

/* View model for  */

module.exports = Backbone.Model.extend({
  defaults: {
   "ready" : false,
   "dirty" : false
  },
  initialize : function(args) {
    var self = this;
    self.url = "/adminonly/brandeddomain/details/" + args.id;
    self.fetch();
  },
  domainid: function() {
    return this.get("id");
  },
  mainDomain: function() {
    return this.get("mainDomain");
  },
  getUrl : function() {
    return this.get("url");
  },
  setUrl: function(u) {
    this.set({"url":u, "dirty" : true});
  },
  smsOriginator : function() {
    return this.get("smsOriginator");
  },
  setSmsOriginator: function(s) {
    this.set({"smsOriginator":s, "dirty" : true});
  },
  emailOriginator : function() {
    return this.get("emailOriginator");
  },
  setEmailOriginator: function(s) {
    this.set({"emailOriginator":s, "dirty" : true});
  },
  mailTheme : function() {
    return this.get("mailTheme");
  },
  setMailTheme: function(s) {
    this.set({"mailTheme":s, "dirty" : true});
  },
  signviewTheme : function() {
    return this.get("signviewTheme");
  },
  setSignviewTheme: function(s) {
    this.set({"signviewTheme":s, "dirty" : true});
  },
  serviceTheme : function() {
    return this.get("serviceTheme");
  },
  setServiceTheme: function(s) {
    this.set({"serviceTheme":s, "dirty" : true});
  },
  loginTheme : function() {
    return this.get("loginTheme");
  },
  setLoginTheme: function(s) {
    this.set({"loginTheme":s, "dirty" : true});
  },
  browserTitle : function() {
    return this.get("browserTitle");
  },
  setBrowserTitle: function(s) {
    this.set({"browserTitle":s, "dirty" : true});
  },
  favicon : function() {
    return this.get("favicon");
  },
  setFavicon: function(s) {
    this.set({"favicon":s, "dirty" : true});
  },
  participantColor1 : function() {
    return this.get("participantColor1");
  },
  setParticipantColor1: function(s) {
    this.set({"participantColor1":s, "dirty" : true});
  },
  participantColor2 : function() {
    return this.get("participantColor2");
  },
  setParticipantColor2: function(s) {
    this.set({"participantColor2":s, "dirty" : true});
  },
  participantColor3 : function() {
    return this.get("participantColor3");
  },
  setParticipantColor3: function(s) {
    this.set({"participantColor3":s, "dirty" : true});
  },
  participantColor4 : function() {
    return this.get("participantColor4");
  },
  setParticipantColor4: function(s) {
    this.set({"participantColor4":s, "dirty" : true});
  },
  participantColor5 : function() {
    return this.get("participantColor5");
  },
  setParticipantColor5: function(s) {
    this.set({"participantColor5":s, "dirty" : true});
  },
  participantColor6 : function() {
    return this.get("participantColor6");
  },
  setParticipantColor6: function(s) {
    this.set({"participantColor6":s, "dirty" : true});
  },
  draftColor : function() {
    return this.get("draftColor");
  },
  setDraftColor: function(s) {
    this.set({"draftColor":s, "dirty" : true});
  },
  cancelledColor : function() {
    return this.get("cancelledColor");
  },
  setCancelledColor: function(s) {
    this.set({"cancelledColor":s, "dirty" : true});
  },
  initatedColor : function() {
    return this.get("initatedColor");
  },
  setInitatedColor: function(s) {
    this.set({"initatedColor":s, "dirty" : true});
  },
  sentColor : function() {
    return this.get("sentColor");
  },
  setSentColor: function(s) {
    this.set({"sentColor" : s, "dirty" : true});
  },
  deliveredColor : function() {
    return this.get("deliveredColor");
  },
  setDeliveredColor: function(s) {
    this.set({"deliveredColor":s, "dirty" : true});
  },
  openedColor : function() {
    return this.get("openedColor");
  },
  setOpenedColor: function(s) {
    this.set({"openedColor":s, "dirty" : true});
  },
  reviewedColor : function() {
    return this.get("reviewedColor");
  },
  setReviewedColor: function(s) {
    this.set({"reviewedColor":s, "dirty" : true});
  },
  signedColor : function() {
    return this.get("signedColor");
  },
  setSignedColor: function(s) {
    this.set({"signedColor":s, "dirty" : true});
  },
  dirty: function() {
    return this.get("dirty");
  },
  ready: function() {
    return this.get("ready");
  },
  reload : function() {
    this.set({ready : false});
    this.fetch();
  },
  parse : function(data) {
    data['ready'] = true;
    data['dirty'] = false;
    return data;
  },
  deleteTheme : function(tid,callback) {
    var self = this;
    new Submit({
      url : "/adminonly/brandeddomain/deletetheme/" + self.domainid() + "/" + tid,
      method : "POST",
      ajax : true,
      ajaxsuccess : callback
    }).send();
  },
  save : function(callback) {
    var self = this;
    new Submit({
      url : "/adminonly/brandeddomain/details/change/" + self.domainid(),
      method : "POST",
      ajax : true,
      ajaxsuccess : function() {
        self.set({"dirty" : false});
        callback();
      },
      domain : JSON.stringify(self.attributes)
    }).send();
  }
});
