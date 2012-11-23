// Login + Password reminder modal

(function(window){

var LoginModel = Backbone.Model.extend({
  defaults: {
        reminderView: false,
        email : "",
        password : "",
        referer : "",
        visible : true
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
    return this.get("pad");
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
              window.location = model.referer() != undefined && model.referer() != "" ? model.referer() : "/newdocument";
            }  
            else
            {
              FlashMessages.add({ content: localization.loginModal.loginFailed, color: "red"});
            }
          }
        });
    if (model.pad() == true)
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
              FlashMessages.add({ content: localization.loginModal.passwordReminderSend, color: "green"});
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
              FlashMessages.add({ content: text, color: "red"});
            }
          }
        }).send();
  }
});

var LoginView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        var view = this;
        this.render();
    },
    popupLoginModal : function() {
      var model = this.model;
      //Content
      var content = $("<div class='body'/>");
      var wrapper = $("<div class='wrapper' style='text-align:right'/>");
      content.append(wrapper);

      var emailinput = InfoTextInput.init({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              onEnter : function() {  model.login();}

      });
      emailinput.input().attr("autocomplete","false");
      wrapper.append($("<span class='txt'/>").text(localization.loginModal.email)).append(emailinput.input()).append("<BR/>");
      emailinput.input().click();
      var passwordinput = InfoTextInput.init({
              infotext: localization.loginModal.password,
              value : model.password(),
              onChange : function(v) {model.setPassword(v);} ,
              inputtype : "password",
              name : "password",
              onEnter : function() {  model.login();}

      });
      passwordinput.input().attr("autocomplete","false");
      wrapper.append($("<span class='txt'/>").text(localization.loginModal.password)).append(passwordinput.input()).append("<BR/>");
      wrapper.append("<BR/>");
      var toogleOption = $("<a href='#' class='txt-link message s-forgot-password'/>").text(localization.loginModal.forgotpassword).click(function(){ model.toogleView();});

      this.popupLoginModalConfirmation = Confirmation.popup({
        title: localization.loginModal.login,
        rejectText: localization.cancel,
        content: content,
        extraOption : toogleOption,
        cssClass : "login-container",
        mask: {
            color: standardDialogMask,
            top: standardDialogTop,
            loadSpeed: 0,
            opacity: 0.9
        },
        acceptButton:  Button.init({
                  size  : "small",
                  color : "blue",
                  text  : localization.loginModal.login,
                  cssClass : "login-button",
                  onClick : function() {
                        model.login();
                    }
                }).input()
      });
      if (model.email() == undefined || model.email() == "")
        emailinput.input().focus();
      else
        passwordinput.input().focus();
    },
    popupReminderModal : function() {
      var model = this.model;
      //Content
      var content = $("<div class='body'/>");
      var wrapper = $("<div class='wrapper'/>");
      content.append(wrapper);

      
      var emailinput = InfoTextInput.init({
              infotext: localization.loginModal.email,
              value : model.email(),
              onChange : function(v) {model.setEmail(v);} ,
              inputtype : "text",
              name : "email",
              onEnter : function() { model.sendPasswordReminder();}
                                          
      });
      emailinput.input().attr("autocomplete","false");
      wrapper.append($("<span class='txt'/>").text(localization.loginModal.email)).append(emailinput.input()).append("<BR/>");
      wrapper.append("<BR/>");
      //Popup
      var toogleOption = $("<a href='#' class='txt-link message s-log-in'/>").text(localization.loginModal.login).click(function(){ model.toogleView();});

      this.popupReminderModalConfirmation = Confirmation.popup({
        title: localization.loginModal.forgotpassword,
        rejectText: localization.cancel,
        content: content,
        extraOption : toogleOption,
        cssClass : "recovery-container",
        mask: {
            color: standardDialogMask,
            top: standardDialogTop,
            loadSpeed: 0,
            opacity: 0.9
        },
        acceptButton:  Button.init({
                  size  : "small",
                  color : "green",
                  text  : localization.loginModal.sendNewPassword,
                  cssClass : "recovery-password-submit",
                  onClick : function() {
                        model.sendPasswordReminder();
                    }
                }).input()
      });
      emailinput.input().focus();

    },
    render: function () {
       var model = this.model;
       $("#exposeMask").css("display", "none"); // Hack to expose mask. It works wrong.
       if (this.popupLoginModalConfirmation != undefined) {
         this.popupLoginModalConfirmation.view.reject();
         this.popupLoginModalConfirmation = undefined;
       }  
       if (this.popupReminderModalConfirmation != undefined) {
         this.popupReminderModalConfirmation.view.reject();
         this.popupReminderModalConfirmation = undefined;
       }  
       if (model.visible()) {
         if (model.loginView()) 
           this.popupLoginModal();
         else
           this.popupReminderModal();
       }
       return this;
    }
});


window.Login = function(args) {
          var model = new LoginModel(args);
          var view =  new LoginView({model : model, el : $("<div/>")});
          return new Object({
              model : function() {return model;},
              view  : function() {return view;},
              show : function() {
                model.show();
              }
            });
};

})(window);
