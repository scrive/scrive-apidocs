/* Page for accepting API Access  */
(function(window){


window.OAuthConfirationModel = Backbone.Model.extend({
  defaults: {
    logged : false,
    who    : ""   ,
    readPermission   : false,
    createPermission : false,
    sendPermission   : false,
    token : ""
    }
  ,
  logged : function() {
      return this.get("logged");
  },
  who: function() {
      return this.get("who");
  },
  readPermission: function() {
      return this.get("readPermission");
  },
  createPermission: function() {
      return this.get("createPermission");
  },
  sendPermission: function() {
      return this.get("sendPermission");
  },
  token : function() {
      return this.get("token");
  },
  deny : function() {
      new Submit({
          method: "POST",
          url: "/oauth/authorizationdeny",
          oauth_token : this.token()
        }).send();

  },
  accept : function() {
      new Submit({
          method: "POST",
          url: "/oauth/authorizationconfirm",
          oauth_token : this.token()
        }).send();
  },
  login : function(email,password) {
    new Submit({
          method: "POST",
          url: "/login",
          ajax: true,
          email : email,
          password : password,
          ajaxsuccess: function(rs) {
            var resp = JSON.parse(rs);
            if (resp.logged == true)
            {
              window.location = window.location;
            }
            else
            {
              new FlashMessage({ content: localization.loginModal.loginFailed, color: "red"});
            }
          }
        }).send();
  },
  createAccount : function(email) {
    new Submit({
          method: "POST",
          url: "/signup",
          email : email,
          ajax: true,
          ajaxsuccess: function(rs) {
            resp = JSON.parse(rs);
            if (resp.sent === true) {
              mixpanel.track('Create new account from API permissions');
              mixpanel.people.set({
                  '$email'      :  email,
                  'Signup Method' : 'API'
              });
              var content = localization.payments.outside.confirmAccountCreatedUserHeader;
              new FlashMessage({content: content, color: 'green'});
            } else if (resp.sent === false) {
              mixpanel.track('Error',
                            {Message : 'signup failed'});
              new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, color: 'red'});
            }
         }
        }).sendAjax();
  }
});

var OAuthConfirationView = Backbone.View.extend({
    model: OAuthConfirationModel,
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.extrasInTabsRow = args.extrasInTabsRow;
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    top: function() {
      var model = this.model;
      var box = $("<header class='site'/>");
      var nav = $("<nav/>");
      var ul = $("<ul class='ct'/>");
      var content = $("<div class='content'>");
      var deny = $("<li class='page-first' style='float:left;width:200px'/>").append($("<a class='deny page' href='#'></a>").text(localization.apiConfiration.deny));
      deny.click(function() {model.deny();});
      var logo = $("<li id='branding' style='float:right'><a href='/' class='page' id='logo'><img width='120' height='22' src='/img/logo.png'></a></id>");
      return box.append(nav.append(ul.append(deny).append(logo)));
    },
    header : function() {
        return $("<h1/>").text(localization.apiConfiration.title);
    },
    textBox : function() {
      var model = this.model;
      var box = $("<div class='box white oauth-confiration-box'/>");
      var hwho = $("<span/>").text(model.who());
      var hrest = $("<span/>").text(" "+ localization.apiConfiration.header);
      box.append($("<p class=''/>").append(hwho).append(hrest));
      var list = $("<ul/>");
      if (model.createPermission())
        list.append($("<li>").text(localization.apiConfiration.createPermission));
      if (model.readPermission())
        list.append($("<li>").text(localization.apiConfiration.readPermission));
      if (model.sendPermission())
        list.append($("<li>").text(localization.apiConfiration.sendPermission));
      box.append(list);
      if (model.logged())
        box.append($("<p class=''/>").text(localization.apiConfiration.footerLogged));
      else
        box.append($("<p class=''/>").text(localization.apiConfiration.footerNotLogged));

      return box;
    },
    acceptButton : function() {
        var box = $("<div class='button-box'>");
        var model = this.model;
        var button = Button.init({
          color: "green",
          size: "big",
          cssClass : "float-right",
          text: localization.apiConfiration.accept,
          onClick: function() {
              model.accept();
              return false;
          }
        }).input();
        return box.append(button);
    },
    loginBox : function() {
      var model = this.model;
      var box = $("<div class='login-box'/>");
      box.append($("<div/>").append($("<h2/>").text(localization.apiConfiration.loginAndAcceptTitle)));
      var emailinput = InfoTextInput.init({
              infotext: localization.loginModal.email,
              value : "",
              cssClass : "big-input",
              inputtype : "text",
              name : "email"
      });

      emailinput.input().attr("autocomplete","false");
      box.append(emailinput.input());
      var passwordinput = InfoTextInput.init({
              infotext: localization.loginModal.password,
              value : "",
              inputtype : "password",
              cssClass : "big-input",
              name : "password",
              onEnter : function() {  model.login(emailinput.value(),passwordinput.value());}

      });
      passwordinput.input().attr("autocomplete","false");
      box.append(passwordinput.input());

      var button = Button.init({
                  size  : "small",
                  color : "green",
                  cssClass : "login-button",
                  text  : localization.apiConfiration.loginAndAccept,
                  onClick : function() {
                        model.login(emailinput.value(),passwordinput.value());
                    }
                }).input();
      box.append(button);
      return box;
    },
    createAccountBox : function() {
      var model = this.model;
      var box = $("<div class='create-account-box'/>");
      box.append($("<div/>").append($("<h2/>").text(localization.apiConfiration.createAccount)));
      var emailinput = InfoTextInput.init({
              infotext: localization.apiConfiration.enterEmailAdress,
              value : "",
              cssClass : "big-input",
              inputtype : "text",
              name : "email"
      });
      box.append(emailinput.input());
      var button = Button.init({
                  size  : "small",
                  color : "green",
                  cssClass : "create-account-button",
                  text  : localization.apiConfiration.createAccount,
                  onClick : function() {
                        if (new EmailValidation().validateData(emailinput.value()))
                          model.createAccount(emailinput.value());
                        else
                          new FlashMessage({color: "red", content: localization.loginModal.invalidEmail});
                    }
                }).input();
      box.append(button);
      return box;
    },
    body : function() {
      var mainContainer = $("<div class='mainContainer'/>");
      var bodyContainer = $("<div class='body-container'/>");

      mainContainer.append(bodyContainer.append(this.header()).append(this.textBox()));
      if (!this.model.logged())
           mainContainer.append(this.loginBox()).append(this.createAccountBox());
      else
           mainContainer.append(this.acceptButton());
      return mainContainer;
    },
    render: function () {
        var container = $(this.el);
        container.append(this.top()).append(this.body());
        return this;
    }
});


window.OAuthConfiration = function(args){
        var model = new OAuthConfirationModel(args);
        var view = new OAuthConfirationView({
                        model: model,
                        el : $("<div/>")
                    });
        return {
              model    : function()    { return model;}
            , view     : function()    { return view;}
         };
};


})(window);
