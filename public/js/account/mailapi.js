
(function(window){

window.MailApi = Backbone.Model.extend({
  defaults : {
      key         : undefined,
      sent    : 0,
      limit   : 0
  },
  key : function() {
     return this.get("key");
  },
  contractMail : function() {
     return this.get("contractMail");
  },
  offerMail : function() {
     return this.get("offerMail");
  },
  orderMail : function() {
     return this.get("orderMail");
  },
  sent : function() {
     return this.get("sent");
  },
  limit : function() {
     return this.get("limit");
  },
});



})(window);
