var Backbone = require("backbone");
var FlashMessage = require("./flashmessages.js").FlashMessage;
var FlashMessageAfterReload = require("./flashmessages.js").FlashMessageAfterReload;
var Submit = require("./submits.js").Submit;
var $ = require("jquery");
var InfoTextInput = require("./infotextinputs.js").InfoTextInput;
var Button = require("./buttons.js").Button;
var PasswordValidation = require("./validation.js").PasswordValidation;

// setting new password (after email reset step)


  var NewPasswordModel = Backbone.Model.extend({
    defaults: {
      password: '',
      password2: '',
      linkchangepassword: ''
    },
    linkchangepassword: function() {
      return this.get('linkchangepassword');
    },
    password: function() {
      return this.get('password');
    },
    setPassword: function(password) {
      this.set('password', password);
    },
    password2: function() {
      return this.get('password2');
    },
    setPassword2: function(password) {
      this.set('password2', password);
    },
    validatePassword: function() {
      var password = this.password();
      var password2 = this.password2();
      if (password !== password2) {
        return localization.newPasswordModal.flashMessagePasswordsDontMatch;
      }

      var error_message;
      var result = password.validate(new PasswordValidation({
        callback: function (text, elem, v) {error_message = v.message();},
        message: localization.validation.passwordLessThanMinLength,
        message_max: localization.validation.passwordExceedsMaxLength,
        message_digits: localization.validation.passwordNeedsLetterAndDigit
      }));

      return result ? null : error_message;
    },
    resetPassword: function() {
      var model = this;
      var validationError = model.validatePassword();
      if (validationError !== null) {
        new FlashMessage({content: validationError, type: 'error'});
        return;
      }

      new Submit({
        method: 'POST',
        url: model.linkchangepassword(),
        ajax: true,
        password: model.password(),
        ajaxsuccess: function(resp) {
          if (resp.logged === true) {
            new FlashMessageAfterReload({
              content: localization.newPasswordModal.flashMessageUserPasswordChanged,
              type: 'success'
            })
            window.location = resp.location;
          } else {
            new FlashMessage({content: localization.newPasswordModal.flashMessagePasswordChangeLinkNotValid,
                              type: 'error'});
          }
        }
      }).send();
    }
  });

  var NewPasswordView = Backbone.View.extend({
    initialize: function(options) {
      this.options = options;
      this.render();
    },

    render: function () {
      var model = this.model;
      var content = $("<div/>");
      var wrapper = $("<div/>");
      var body = $("<div/>");
      var header = $("<div style='margin-bottom:30px;text-align:center;'/>");
      var loginLogoUrl = window.cdnbaseurl + "/login_logo/" + window.brandingdomainid + "/" + window.brandinghash;
      header.append($("<img alt='logo' src='"+ loginLogoUrl +"'/>"));
      header.append($("<div class='divider-line'/>"));

      var poweredLabel = $("<div class='label' style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
      header.append(poweredLabel);

      $(this.el).append(header);

      content.append(wrapper.append(body));

      if (this.options.body)
         body.append($("<label style='cursor: text; padding-bottom: 1em;'/>").text(this.options.body));

      var passwordInput = new InfoTextInput({
        infotext: localization.newPasswordModal.modalNewPasswordViewNewPassword,
        value: model.password(),
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();},
        style : "width : 245px; padding : 7px 14px; font-size: 16px"
      });
      body.append($("<div class='position first'/>").append(passwordInput.el()));

      var password2Input = new InfoTextInput({
        infotext: localization.newPasswordModal.modalNewPasswordViewRepeatPassword,
        value: model.password2(),
        onChange: function(v) {model.setPassword2(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();},
        style : "width : 245px; padding : 7px 14px; font-size: 16px"

      });
      body.append($("<div class='position' style='margin-top:15px;'/>").append(password2Input.el()));

      var changePasswordButton = new Button({
          type  : "main",
          text  : this.options.button,
          style : "width:235px",
          onClick : function() {
            model.resetPassword();
          }
        });

     body.append($("<div class='position' style='text-align:center;margin-top:30px'/>").append(changePasswordButton.el()));
     $(this.el).append(content);
    }
  });


  var NewPassword = exports.NewPassword = function(args) {
    var model = new NewPasswordModel(args);
    var view = new NewPasswordView({
      model: model,
      body: null,
      el : $("<div class='new-password-box'style='width:275px;margin:30px auto'/>"),
      button: localization.newPasswordModal.modalNewPasswordViewFooterSave
    });
    this.el = function() {return $(view.el);};
  };

