var Backbone = require("backbone");
var Submit = require("./submits.js").Submit;
var FlashMessage = require("./flashmessages.js").FlashMessage;
var Language = require("./utils/language.js").Language;
var _ = require("underscore");
var $ = require("jquery");
var Button = require("./buttons.js").Button;
var InfoTextInput = require("./infotextinputs.js").InfoTextInput;
var EmailValidation = require("./validation.js").EmailValidation;
var OAuthConfirationModel = require("./oauth.js").OAuthConfirationModel;
var Track = require("../scripts/common/track");

/* Page for accepting API Access  */

var OAuthConfirationModel = exports.OAuthConfirationModel = Backbone.Model.extend({
  defaults: {
    logged : false,
    who    : ""   ,
    readPermission   : false,
    createPermission : false,
    sendPermission   : false,
    fullAccessPermission   : false,
    token : "",
    askFor2FA: false
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
  fullAccessPermission: function() {
      return this.get("fullAccessPermission");
  },
  token : function() {
      return this.get("token");
  },
  askFor2FA: function() {
      return this.get("askFor2FA");
  },
  startAskingFor2FA: function() {
      this.set({"askFor2FA": true}, {silent: true});
      this.trigger("change:askFor2FA");
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
  login : function(email,password,totp) {
    var self = this;
    new Submit({
          method: "POST",
          url: "/login",
          ajax: true,
          email : email,
          password : password,
          totp: totp,
          ajaxsuccess: function(rs) {
            if (rs.logged == true)
            {
              window.location = window.location;
            }
            else if (!self.askFor2FA() && rs.totp_missing) {
              self.startAskingFor2FA()
            } else {
              new FlashMessage({ content: localization.loginModal.loginFailed, type: 'error'});
            }
          }
        }).send();
  }
});

var OAuthConfirationView = Backbone.View.extend({
    model: OAuthConfirationModel,
    initialize: function (args) {
        _.bindAll(this, 'render', 'renderUpdate2FA');
        this.extrasInTabsRow = args.extrasInTabsRow;
        this.model.bind('change', this.render);
        this.model.bind('change:askFor2FA', this.renderUpdate2FA);
        this.model.view = this;
        this.render();
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
      list.append($("<li>").text(localization.apiConfiration.readUserInfo));
      if (model.createPermission())
        list.append($("<li>").text(localization.apiConfiration.createPermission));
      if (model.readPermission())
        list.append($("<li>").text(localization.apiConfiration.readPermission));
      if (model.sendPermission())
        list.append($("<li>").text(localization.apiConfiration.sendPermission));
      if (model.fullAccessPermission())
        list.append($("<li>").text(localization.apiConfiration.fullAccessPermission));
      box.append(list);
      if (model.logged()) {
        box.append($("<p class=''/>").text(localization.apiConfiration.footerLogged));
      } else {
        var text = $("<p class=''/>").html(localization.apiConfiration.footerNotLogged);
        text.find("a").attr("href", "/get-started");
        box.append(text);
      }

      return box;
    },
    acceptButton : function() {
        var box = $("<div class='button-box'>");
        var model = this.model;
        var button = new Button({
          type : 'action',
          size: "big",
          cssClass : "float-right",
          text: localization.apiConfiration.accept,
          onClick: function() {
              model.accept();
              return false;
          }
        }).el();
        return box.append(button);
    },
    loginBox : function() {
      var model = this.model;
      var self = this;
      var box = $("<div class='login-box'/>");
      box.append($("<div/>").append($("<h2/>").text(localization.apiConfiration.loginAndAcceptTitle)));
      var emailinput = new InfoTextInput({
              infotext: localization.loginModal.email,
              value : "",
              cssClass : "big-input",
              inputtype : "text",
              name : "email"
      });

      emailinput.el().attr("autocomplete","false");
      box.append(emailinput.el());
      this.passwordinput = new InfoTextInput({
              infotext: localization.loginModal.password,
              value : "",
              inputtype : "password",
              cssClass : "big-input",
              name : "password",
              onEnter : function() {  model.login(emailinput.value(),self.passwordinput.value(),self.tokeninput.value());}

      });
      this.passwordinput.el().attr("autocomplete","false");
      box.append(this.passwordinput.el());

      this.tokeninput = new InfoTextInput({
              infotext: localization.loginModal.twoFactor,
              value : "",
              inputtype : "number",
              cssClass : "big-input",
              name : "totp",
              onEnter : function() {  model.login(emailinput.value(),self.passwordinput.value(),self.tokeninput.value());}
      });
      this.tokeninput.el().css("display","none");
      this.tokeninput.el().attr("autocomplete","false");
      box.append(this.tokeninput.el());


      var button = new Button({
                  size  : "small",
                  type : 'action',
                  cssClass : "login-button",
                  text  : localization.apiConfiration.loginAndAccept,
                  onClick : function() {
                        model.login(emailinput.value(),self.passwordinput.value(), self.tokeninput.value());
                    }
                }).el();
      box.append(button);
      return box;
    },
    body : function() {
      var mainContainer = $("<div class='mainContainer'/>");
      var bodyContainer = $("<div class='body-container'/>");

      mainContainer.append(bodyContainer.append(this.header()).append(this.textBox()));
      if (!this.model.logged())
           mainContainer.append(this.loginBox());
      else
           mainContainer.append(this.acceptButton());
      return mainContainer;
    },
    renderUpdate2FA: function() {
      var askFor2FA = this.model.askFor2FA();
      this.tokeninput.el().css("display",askFor2FA ? "block": "none");
    },
    render: function () {
        var container = $(this.el);
        container.append(this.body());
        return this;
    }
});


var OAuthConfirmation = exports.OAuthConfirmation = function(args){
        var model = new OAuthConfirationModel(args);
        var view = new OAuthConfirationView({
                        model: model,
                        el : $("<div/>")
                    });
        return {
              model    : function()    { return model;}
            , view     : function()    { return view;}
            , deny     : function()    { model.deny();}
         };
};
