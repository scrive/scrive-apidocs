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
              FlashMessages.add({ content: localization.loginModal.loginFailed, color: "red"});
            }
          }
        }).send();
  },
  createAccount : function(email) {
    new Submit({
          method: "POST",
          url: "/signup",
          email : email,
          ajaxsuccess: function(rs) {
              window.location = window.location;
          }
        }).send();
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
      var box = $("<div id='top-slim-container'>");
      var content = $("<div class='content'>");
      var deny = $("<a class='toplink deny' href='#'></a>").text(localization.apiConfiration.deny);
      deny.click(function() {model.deny();});
      var logo = $("<a class='scrive-logo'  href='#'></a>");
      return box.append(content.append(deny).append(logo)).append("<div class='clearboth'></div>");
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
              inputtype : "text",
              name : "email"
      });
 
      emailinput.input().attr("autocomplete","false");
      box.append($("<span class='txt'/>").text(localization.loginModal.email)).append(emailinput.input()).append("<BR/>");
      var passwordinput = InfoTextInput.init({
              infotext: localization.loginModal.password,
              value : "",
              inputtype : "password",
              name : "password",
              onEnter : function() {  model.login(emailinput.value(),passwordinput.value());}

      });
      passwordinput.input().attr("autocomplete","false");
      box.append($("<span class='txt'/>").text(localization.loginModal.password)).append(passwordinput.input()).append("<BR/>");

      var button = Button.init({
                  size  : "small",
                  color : "green",
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
              inputtype : "text",
              name : "email"
      });
      box.append($("<span class='txt'/>").text(localization.loginModal.email)).append(emailinput.input()).append("<BR/>");
      box.append("<BR/>").append("<BR/>");
      var button = Button.init({
                  size  : "small",
                  color : "green",
                  text  : localization.apiConfiration.createAccount,
                  onClick : function() {
                        model.createAccount(emailinput.value());
                    }
                }).input();
      box.append(button);
      return box;
    },
    body : function() {
      var mainContainer = $("<div class='mainContainer'/>");
      var bodyContainer = $("<div class='body-container' />");
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
