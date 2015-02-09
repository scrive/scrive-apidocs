/** @jsx React.DOM */

define(['React', 'Backbone', 'common/hubspot_service', 'common/adwords_conversion_service', 'legacy_code'], function(React, Backbone, HubSpot, AdwordsConversionService) {

var stripHash = function (hash) {
  if (typeof hash !== "string") return "";
  return hash.replace("#", "");
};

return Backbone.Model.extend({
  defaults: {
        view: stripHash(window.location.hash),
        views: ["login", "reminder", "signup"],
        defaultView: "login",
        email : "",
        password : "",
        referer : "",
        autofocus: false,
        nolinks : false,
        langprefix : "/"+ localization.code +"/"
  },
  initialize : function() {
    if (this.email() == "" && LocalStorage.get('login','last_login_email') != "")
      this.set({email : LocalStorage.get('login','last_login_email')});

    if (this.get("views").indexOf(this.get("view")) == -1) {
      this.setView(this.get("defaultView"));
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
  setView : function(view, silent) {
    if (this.get("views").indexOf(view) !== -1) {
      if (view == this.get("defaultView")) {
        window.location.hash = "";
      } else {
        window.location.hash = "#" + view;
      }
      return this.set({view: view});
    }

    console.warn("no view with name " + view + " exists");
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
  login : function() {
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
            var resp = JSON.parse(rs);
            if (resp.logged == true)
            {
                trackTimeout('Login successful', {}, function() {
                    window.location = model.referer() != undefined && model.referer() != "" && model.referer() != "/" ? model.referer() : "/newdocument";
                });
            }
            else {
                if( resp.ipaddr ) {
                    mixpanel.track('Error',
                                   {Message: 'login failed due to IP restriction',
                                   IP: resp.ipaddr,
                                   Admin: resp.adminname});
                    var text = $("<span>" + localization.loginModal.loginFailedBadIP + "</span>");
                    $(".put-ip-here",text).text(resp.ipaddr);
                    $(".put-adminname-here",text).text(resp.adminname);
                    new FlashMessage({ content: text, type : "error"});
                }
                else {
                    mixpanel.track('Error',
                                   {Message: 'login failed'});
                    new FlashMessage({ content: localization.loginModal.loginFailed, type : "error"});
                }
            }
          }
        });
    if (model.pad())
      submit.add("pad","true");
    submit.send();
  },
  sendPasswordReminder : function() {
    var model = this;
    new Submit({
          method: "POST",
          url: "/api/frontend/sendpasswordresetmail",
          ajax: true,
          email : model.email(),
          ajaxsuccess: function(rs) {
            var resp = JSON.parse(rs);
            if (resp.send == true)
            {
                mixpanel.track('Password reminder sent');
              new FlashMessage({ content: localization.loginModal.passwordReminderSend, type : "success"});
            }
            else
            {
              var text = "";
              if (resp.badformat)
                text = localization.loginModal.invalidEmail;
              else if (resp.nouser)
                text = localization.loginModal.noUser;
              else if (resp.toomuch)
                text = localization.loginModal.tooMuch;
              mixpanel.track('Error',
                             {Message: 'password reminder failed: ' + text});
              new FlashMessage({ content: text, type : "error"});
            }
          }
        }).send();
  },
  signup : function() {
    var model = this;
    mixpanel.track('Submit signup');
    new Submit({
      method: 'POST',
      url: "/api/frontend/signup",
      ajax: true,
      lang : Language.current(),
      email: model.email(),
      ajaxsuccess: function(rs) {
        try {
          resp = JSON.parse(rs);
        } catch (e) {
          resp = JSON.parse($(rs).text());
        }
        if (resp.sent === true) {
          _gaq.push(['_trackEvent', 'Signup', 'Clicked']);
          AdwordsConversionService.markAsSignupConversion();
          mixpanel.track('Create new account', {
              'Email' : model.email()
          });
          mixpanel.people.set({
              '$email'        : model.email()
          });
          var content = localization.payments.outside.confirmAccountCreatedUserHeader;
          new FlashMessage({content: content, type: 'success'});
        } else if (resp.sent === false) {
          mixpanel.track('Error',
                         {Message : 'signup failed'});
          new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, type: 'error'});
        }
      }
    }).send();
  }
});

});
