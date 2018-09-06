var Backbone = require("backbone");
var Submit = require("../../../js/submits.js").Submit;
var User = require("../../../js/account/user.js").User;
var Subscription = require("../subscription");
var $ = require("jquery");

/* View model for account settings */

module.exports = Backbone.Model.extend({
  initialize: function () {
    var self = this;
    var user = new User({});
    user.bind("change", function () {
      self.trigger("change");
    });
    user.set({"ready": false}, {silent: true});
    this.set({"user": user});

    var addSubscription = function (m, r, o) {
      var subscription = new Subscription({current_user_is_admin: m.companyadmin()});
      self.set({"subscription": subscription});
      subscription.bind("change", function () {
        self.trigger("change");
      });
      subscription.set({"ready": false}, {silent: true});
      subscription.fetch({cache: false, processData: true});
    };

    user.fetch({cache: false, processData: true, success: addSubscription});
  },
  subscription: function () {
     return this.get("subscription");
  },
  user: function () {
     return this.get("user");
  },
  ready: function () {
     if (this.subscription() && this.user()) {
       return this.subscription().ready() && this.user().ready();
     } else {
       return false;
     }
  },
  reload: function () {
    this.subscription().reload();
    this.user().reload();
  }
});
