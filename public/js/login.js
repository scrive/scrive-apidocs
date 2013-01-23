// Login + Password reminder modal

(function(window){

var LoginModel = Backbone.Model.extend({
  defaults: {
        reminderView: false,
        email : "",
        password : "",
        referer : "",
        visible : true,
        rememberPassword : false
  },
  reminderView : function() {
     return this.get("reminderView") == true;
  },
  loginView : function() {
    return !this.reminderView();
  },
  toogleView : function() {
    this.set({reminderView : ! this.reminderView()});
  },
  visible : function() {
    return this.get("visible");
  },
  pad : function() {
    return this.get("pad") == true;
  },
  show: function() {
    this.set({visible : true});
  },
  hide: function() {
    this.set({visible : false});
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
  rememberPassword: function() {
    return this.get("rememberPassword");
  },
  toogleRememberPassword : function(v) {
    this.set({rememberPassword : !this.rememberPassword()},{silent: true});
    this.trigger("rememberPasswordChange");
  },
  setEmail: function(v) {
    this.set({email : v}, {silent: true});
  },
  setPassword: function(v) {
    this.set({password : v}, {silent: true});
  },
  login : function() {
    var model = this;
    var submit = new Submit({
          method: "POST",
          url: "/login",
          ajax: true,
          email : model.email(),
          password : model.password(),
          ajaxsuccess: function(rs) {
            var resp = JSON.parse(rs);
            if (resp.logged == true)
            {
                mixpanel.track('Login successful', {}, function() {
                    window.location = model.referer() != undefined && model.referer() != "" && model.referer() != "/" ? model.referer() : "/newdocument";
                });
            }  
            else
            {
                mixpanel.track('Error',
                               {Message: 'login failed'});
              new FlashMessage({ content: localization.loginModal.loginFailed, color: "red"});
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
          url: "/amnesia",
          ajax: true,
          email : model.email(),
          ajaxsuccess: function(rs) {
            var resp = JSON.parse(rs);
            if (resp.send == true)
            {
                mixpanel.track('Password reminder sent');
              new FlashMessage({ content: localization.loginModal.passwordReminderSend, color: "green"});
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
              new FlashMessage({ content: text, color: "red"});
            }
          }
        }).send();
  }
});

var LoginView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.render();
    },
    loginSection : function() {
      var model = this.model;
      var content = $("<div class='short-input-container login'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      var header = $("<header class='shadowed'/>");
      header.append($("<h1/>").text(localization.welcomeback));
      if (!model.pad()) $(this.el).append(header);
      content.append(wrapper.append(body));

      var emailinput = InfoTextInput.init({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              cssClass : "big-input",
              inputtype : "text",
              name : "email",
              onEnter : function() {  model.login();}

      });
      emailinput.input().attr("autocomplete","false");
      body.append($("<div class='position first'/>").append(emailinput.input()));
      emailinput.input().click();
      var passwordinput = InfoTextInput.init({
              infotext: localization.loginModal.password,
              value : model.password(),
              onChange : function(v) {model.setPassword(v);} ,
              inputtype : "password",
              cssClass : "big-input",
              name : "password",
              onEnter : function() {  model.login();}

      });
      passwordinput.input().attr("autocomplete","false");
      body.append($("<div class='position'/>").append(passwordinput.input()));

      var loginButton = Button.init({
                  size  : "small",
                  color : "blue",
                  text  : localization.loginModal.login + " ›",
                  cssClass : "login-button ",
                  onClick : function() {
                        model.login();
                    }
                });
                
      body.append($("<div class='position'/>").append(loginButton.input()));


      if (!model.pad()) {
        var footer = $("<div class='short-input-container-footer'/>");
        content.append(footer);
      
        var toogleOption = $("<a href='#' class='s-forgot-password'/>").text(localization.loginModal.forgotpassword + "?").click(function(){ model.toogleView();return false;});
        footer.append($("<p class='float-right'/>").append(toogleOption));
      }
      return content;
    },
    reminderSection : function() {
      var model = this.model;
      //Content
      var content = $("<div class='short-input-container recovery'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");

      var header = $("<header class='shadowed recovery'/>");
      header.append($("<h1/>").text(localization.resetYourPassword));
      header.append($("<h2/>").text(localization.resetYourPasswordCheckEmail));
      $(this.el).append(header);
                                     
      content.append(wrapper.append(body));
     
      var emailinput = InfoTextInput.init({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              cssClass : "big-input",
              onEnter : function() { model.sendPasswordReminder();}
                                          
      });
      emailinput.input().attr("autocomplete","false");
      
     var remindButton = Button.init({
                  size  : "small",
                  color : "blue",
                  text  : localization.loginModal.sendNewPassword + " ›",
                  cssClass : "recovery-password-submit",
                  onClick : function() {
                        model.sendPasswordReminder();
                    }
                });
      body.append($("<div class='position first'/>").append(emailinput.input()).append(remindButton.input()));

      return content;

    },
    render: function () {
       var model = this.model;
       $("#page-login").addClass("button-gray");
       $(this.el).empty();
       if (model.loginView())
           $(this.el).append(this.loginSection());
       else
           $(this.el).append(this.reminderSection());
       return this;
    }
});


window.Login = function(args) {
          var model = new LoginModel(args);
          var view =  new LoginView({model : model, el : $("<div class='short-input-section'/>")});
          this.el    = function() {
                        return $(view.el);
                      };
};

})(window);
