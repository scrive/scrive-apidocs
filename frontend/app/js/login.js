// Login + Password reminder modal

define(['Backbone', 'legacy_code'], function() {

var LoginModel = Backbone.Model.extend({
  defaults: {
        reminderView: false,
        email : "",
        password : "",
        referer : "",
        visible : true,
        rememberPassword : false,
        autofocus: false,
        servicelinkcolour : '',
        textscolour : ''
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
  servicelinkcolour : function() {
     return this.get("servicelinkcolour");
  },
  textscolour : function() {
     return this.get("textscolour");
  },
  buttoncolorclass: function() {
     return this.get("buttoncolorclass");
  },
  referer : function() {
    return this.get("referer");
  },
  autofocus : function() {
    return this.get("autofocus");
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
                    new FlashMessage({ content: text, color: "red"});
                }
                else {
                    mixpanel.track('Error',
                                   {Message: 'login failed'});
                    new FlashMessage({ content: localization.loginModal.loginFailed, color: "red"});
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
      var header = $("<div class='shadowed'/>");
      var loginFunction = function() {
          LocalStorage.set('login','last_login_email',model.email());
          model.login();
      };
      model.setEmail(LocalStorage.get('login','last_login_email'));
      header.append($("<h1/>").text(localization.welcomeback));
      if (!model.pad()) $(this.el).append(header);
      content.append(wrapper.append(body));

      var emailinput = new InfoTextInput({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              cssClass : "big-input",
              style : "padding:0px;width:310px;",
              inputStyle : "padding: 14px;width:282px;",
              inputtype : "text",
              name : "email",
              autocomplete : true,
              onEnter : loginFunction

      });
      body.append($("<div class='position first'/>").append(emailinput.el()));
      var passwordinput = new InfoTextInput({
              infotext: localization.loginModal.password,
              value : model.password(),
              onChange : function(v) {model.setPassword(v);} ,
              inputtype : "password",
              cssClass : "big-input",
              name : "password",
              onEnter : loginFunction

      });
      body.append($("<div class='position'/>").append(passwordinput.el()));

      // Automatically focus the appropriate login field.
      if(model.autofocus()) {
          $(document).ready(function() {
              if(emailinput.value() != "" && emailinput.value() != undefined) {
                  passwordinput.focus();
              } else {
                  emailinput.focus();
              }
          });
      }

      var loginButton = new Button({
                  size  : "small",
                  color : "green",
                  text  : localization.loginModal.login,
                  cssClass : "login-button ",
                  onClick : loginFunction
                });

      body.append($("<div class='position'/>").append(loginButton.el()));


      if (!model.pad()) {
        var footer = $("<div class='short-input-container-footer'/>");
        content.append(footer);

        var toogleOption = $("<a href='#' class='s-forgot-password'/>").text(localization.loginModal.forgotpassword).click(function(){ model.toogleView();return false;});
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

      var header = $("<div class='shadowed recovery'/>");
      header.append($("<h1/>").text(localization.resetYourPassword + "."));
      header.append($("<h2/>").text(localization.resetYourPasswordCheckEmail));
      $(this.el).append(header);

      content.append(wrapper.append(body));

      var emailinput = new InfoTextInput({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              cssClass : "big-input",
              onEnter : function() { model.sendPasswordReminder();}

      });

     var remindButton = new Button({
                  size  : "big",
                  color : "green",
                  text  : localization.loginModal.sendNewPassword,
                  cssClass : "recovery-password-submit",
                  onClick : function() {
                        model.sendPasswordReminder();
                    }
                });
      body.append($("<div class='position first'/>").append(emailinput.el()).append(remindButton.el()));

      return content;

    },
    render: function () {
       var model = this.model;
       $(this.el).empty();
       if (model.loginView())
           $(this.el).append(this.loginSection());
       else
           $(this.el).append(this.reminderSection());
       return this;
    }
});


var LoginBrandedView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.render();
    },
    loginSection : function() {
      var model = this.model;
      var content = $("<div style='width:'/>");
      var wrapper = $("<div/>");
      var body = $("<div/>");
      var header = $("<div style='margin-bottom: 50px; margin-top: 50px;text-align:center;'/>");
      content.append(wrapper.append(body));
      $(this.el).append(header);

      var loginFunction = function() {
          LocalStorage.set('login','last_login_email',model.email());
          model.login();
      };
      model.setEmail(LocalStorage.get('login','last_login_email'));
      header.append($("<img alt='logo' src='/brandedlogo'/>"));
      header.append($("<div class='divider-line'/>"));
      var poweredLabel = $("<label style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
      if (model.textscolour() != undefined) poweredLabel.css("color",model.textscolour());
      header.append(poweredLabel);

      var loginLabel =$("<label style='padding-left:10px;'/>").text(localization.login + ":");
      if (model.textscolour() != undefined) loginLabel.css("color",model.textscolour());

      body.append($("<div class='position first' style='text-align: left;height:30px'/>").append(loginLabel));




      var emailinput = new InfoTextInput({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              onEnter : loginFunction,
              autocomplete : true,
              inputStyle : "width :  245px; padding : 7px 14px ",
              style : "width : 273px; padding: 0px; font-size : 16px "

      });
      body.append($("<div class='position' style='margin-bottom:6px;'/>").append(emailinput.el()));
      emailinput.el().click();
      var passwordinput = new InfoTextInput({
              infotext: localization.loginModal.password,
              value : model.password(),
              onChange : function(v) {model.setPassword(v);} ,
              inputtype : "password",
              name : "password",
              onEnter : loginFunction,
              style : "width : 245px; padding : 7px 14px ; font-size : 16px"

      });
      body.append($("<div class='position'/>").append(passwordinput.el()));

      // Automatically focus the appropriate login field.
      if(model.autofocus()) {
          $(document).ready(function() {
              if(emailinput.value() != "" && emailinput.value() != undefined) {
                  passwordinput.focus();
              } else {
                  emailinput.focus();
              }
          });
      }

      var fp_position = $("<div class='position' style='text-align:left;height:30px;'/>");
      body.append(fp_position);

      if (!model.pad()) {
        var toogleOption = $("<label class='s-forgot-password' style='border-bottom: 1px solid #999999;color:#999999;font-style:italic;font-size:10px;line-height: 12px;'/>").text(localization.loginModal.forgotpassword).click(function(){ model.toogleView();return false;});
        if (model.textscolour() != undefined) toogleOption.css("color",model.textscolour());
        fp_position.append($("<div style='display:inline-block;width:224px;text-align:left;vertical-align: bottom;margin-left:4px;'/>").append(toogleOption));
      }
      var button_position = $("<div class='position' style='text-align:center'/>");
      body.append(button_position);

      var loginButton = new Button({
                  size  : "tiny",
                  color : model.buttoncolorclass(),
                  text  : localization.loginModal.login,
                  style : "width:245px;",
                  onClick : loginFunction
                });

      button_position.append(loginButton.el());

      var dontHaveAccount = $("<label class='label-with-link'/>").html(localization.loginModal.dontHaveAccount);
      dontHaveAccount.find('a').attr('href', '/signup');
      var paymentsPage = $("<label class='label-with-link'/>").html(localization.visitOurPricingPage);
      paymentsPage.find('a').attr('href', '/pricing');

      if (model.textscolour() != undefined) {
        dontHaveAccount.css("color",model.textscolour());
        paymentsPage.css("color",model.textscolour());
      }

      if (model.servicelinkcolour()) {
        dontHaveAccount.find('a').css('color', model.servicelinkcolour());
        paymentsPage.find('a').css('color', model.servicelinkcolour());
      }
      body.append($("<div class='position' style='text-align:center;margin-top:20px;'/>").append(dontHaveAccount).append(paymentsPage));

      return content;
    },
    reminderSection : function() {
      var model = this.model;
      //Content
      var content = $("<div style='width:'/>");
      var wrapper = $("<div/>");
      var body = $("<div/>");
      var header = $("<div style='margin-bottom: 103px;text-align:center;'/>");

      header.append($("<img alt='logo' src='/brandedlogo'/>"));
      header.append($("<div class='divider-line'/>"));

      var poweredLabel = $("<label style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
      if (model.textscolour() != undefined) poweredLabel.css("color",model.textscolour());
      header.append(poweredLabel);

      $(this.el).append(header);

      content.append(wrapper.append(body));

      var resetLabel = $("<label style='padding-left:10px;'/>").text(localization.resetYourPassword + ":");
      if (model.textscolour() != undefined) resetLabel.css("color",model.textscolour());

      body.append($("<div class='position first' style='text-align: left;height:30px''/>").append(resetLabel));

      var emailinput = new InfoTextInput({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              style : "width : 245px;padding : 7px 14px; font-size : 16px;",
              onEnter : function() { model.sendPasswordReminder();}

      });

     var remindButton = new Button({
                  size  : "tiny",
                  color : model.buttoncolorclass(),
                  text  : localization.loginModal.sendNewPassword,
                  cssClass : "recovery-password-submit",
                  style : "width:245px;",
                  onClick : function() {
                        model.sendPasswordReminder();
                    }
                });

      body.append($("<div class='position'/>").append(emailinput.el()));
      body.append($("<div class='position' style='text-align:center;margin-top:10px;'/>").append(remindButton.el()));

      return content;

    },
    render: function () {
       var model = this.model;
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
          var view;
          if (args.branded)
            view = new LoginBrandedView({model : model, el : $("<div style='width:275px;margin:20px auto'/>") });
          else
            view = new LoginView({model : model, el : $("<div class='short-input-section'/>")});

          this.el    = function() {
                        return $(view.el);
                      };
};

});
