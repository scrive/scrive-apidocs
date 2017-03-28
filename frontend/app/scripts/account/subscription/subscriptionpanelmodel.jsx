var Backbone = require("backbone");
var Submit = require("../../../js/submits.js").Submit;
var User = require("../../../js/account/user.js").User;
var Subscription = require("../subscription");
var $ = require("jquery");

/* View model for account settings */

module.exports = Backbone.Model.extend({
  initialize: function () {
    var self = this;
    var subscription = new Subscription({});
    var user = new User({});
    this.set({"subscription": subscription, "user": user});
    subscription.bind("change", function () {
      self.trigger("change");
    });
    user.bind("change", function () {
      self.trigger("change");
    });
    subscription.set({"ready": false}, {silent: true});
    user.set({"ready": false}, {silent: true});
    subscription.fetch({cache: false, processData: true});
    user.fetch({cache: false, processData: true});
  },
  subscription: function () {
     return this.get("subscription");
  },
  user: function () {
     return this.get("user");
  },
  ready: function () {
     return this.subscription().ready() && this.user().ready();
  },
  reload: function () {
    this.subscription().reload();
    this.user().reload();
  }
});
