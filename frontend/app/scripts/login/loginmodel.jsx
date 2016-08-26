var React = require("react");
var Backbone = require("backbone");
var LocalStorage = require("../../js/storage.js").LocalStorage;
var Submit = require("../../js/submits.js").Submit;
var Language = require("../../js/utils/language.js").Language;
var Track = require("../common/track");
var $ = require("jquery");


module.exports = Backbone.Model.extend({
  defaults: {
        view: "login",
        email : "",
        password : "",
        referer : "",
        autofocus: false,
        nolinks : false,
        langprefix : "/"+ localization.code +"/"
  },
  initialize : function() {
    if (this.email() == "" && LocalStorage.get('login','last_login_email') != "") {
      this.set({email : LocalStorage.get('login','last_login_email')});
    }
  },
  langprefix : function() {
    return this.get("langprefix");
  },
  nolinks : function() {
     return this.get("nolinks");
  },
  reminderView : function() {
     return this.get("view") == "reminder";
  },
  loginView : function() {
    return this.get("view") == "login";
  },
  signupView : function() {
    return this.get("view") == "signup";
  },
  goToReminderView : function() {
    this.set({view: "reminder"});
  },
  goToSignupView : function() {
    this.set({view: "signup"});
  },
  goToLoginView : function() {
    this.set({view: "login"});
  },
  pad : function() {
    return this.get("pad") == true;
  },
  email : function() {
    return this.get("email");
  },
  password : function() {
     return this.get("password");
  },
  referer : function() {
    return this.get("referer");
  },
  autofocus : function() {
    return this.get("autofocus");
  },
  setEmail: function(v) {
    this.set({email : v}, {silent: true});
  },
  setPassword: function(v) {
    this.set({password : v}, {silent: true});
  },
  login : function(callback) {
    var model = this;
    var email = model.email();
    if (email !== '' && email !== undefined && email !== null) {
      LocalStorage.set('login','last_login_email', email);
    }
    var submit = new Submit({
          method: "POST",
          url: "/login",
          ajax: true,
          email : email,
          password : model.password(),
          ajaxsuccess: function(rs) {
            callback(rs);
          }
        });
    if (model.pad())
      submit.add("pad","true");
    submit.send();
  },
  sendPasswordReminder : function(callback) {
    var model = this;
    new Submit({
          method: "POST",
          url: "/api/frontend/sendpasswordresetmail",
          ajax: true,
          email : model.email(),
          ajaxsuccess: function(rs) {
            callback(rs);
          }
        }).send();
  },
  signup : function(callback) {
    var model = this;
    Track.track('Submit signup');
    new Submit({
      method: 'POST',
      url: "/api/frontend/signup",
      ajax: true,
      lang : Language.current(),
      email: model.email(),
      ajaxsuccess: function(rs) {
        callback(rs);
      }
    }).send();
  }
});
