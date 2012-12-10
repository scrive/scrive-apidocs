// setting new password (after email reset step)

(function(window) {

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
      var validCharsRegex = /^[a-zA-Z0-9\.,-\/#! +$%\^&\*;:{}=\-_`~()]*$/;
      var passwordLocalization = localization.newPasswordModal;
      if (password !== password2) {
        return passwordLocalization.flashMessagePasswordsDontMatch;
      } else if (password === '') {
        return passwordLocalization.flashMessageMissingRequiredField;
      } else if (password.length < 8) {
        return passwordLocalization.flashMessagePasswordLessThanMinLength;
      } else if (password.length > 250) {
        return passwordLocalization.flashMessagePasswordExceedsMaxLength;
      } else if (password.match(validCharsRegex) === null) {
        return passwordLocalization.flashMessageInvalidCharsInPassword;
      } else if (password.replace(/[^a-zA-Z]/g, '').length < 2) {
        return passwordLocalization.flashMessageNeedsLetterAndDigit;
      } else if (password.replace(/[^0-9]/g, '').length < 2) {
        return passwordLocalization.flashMessageNeedsLetterAndDigit;
      } else {
        return null;
      }
    },
    resetPassword: function() {
      var model = this;
      var validationError = model.validatePassword();
      if (validationError !== null) {
        FlashMessages.add({content: validationError, color: 'red'});
        return;
      }

      new Submit({
        method: 'POST',
        url: model.linkchangepassword(),
        ajax: true,
        password: model.password(),
        ajaxsuccess: function(resp) {
          if (resp.logged === true) {
            window.location = resp.location;
          } else {
            FlashMessages.add({content: localization.newPasswordModal.flashMessagePasswordChangeLinkNotValid,
                               color: 'red'});
          }
        }
      }).send();
    }
  });

  var NewPasswordView = Backbone.View.extend({
    initialize: function() {
      this.render();
    },

    render: function () {
      var model = this.model;
       var header = $("<header/>").append($("<h1 class='big'/>").text(localization.newPasswordModal.modalNewPasswordViewHeader));
       $(this.el).append(header);

      
      var content = $("<div class='short-input-container recovery-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      content.append(wrapper.append(body));

      var passwordInput = InfoTextInput.init({
        infotext: localization.newPasswordModal.modalNewPasswordViewNewPassword,
        value: model.password(),
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      passwordInput.input().attr("autocomplete","false");
      body.append($("<div class='position first'/>").append(passwordInput.input()));
      
      var password2Input = InfoTextInput.init({
        infotext: localization.newPasswordModal.modalNewPasswordViewRepeatPassword,
        value: model.password2(),
        onChange: function(v) {model.setPassword2(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      password2Input.input().attr("autocomplete","false");
      body.append($("<div class='position'/>").append(password2Input.input()));

      var changePasswordButton = Button.init({
          size  : 'small',
          color : 'blue',
          text  : localization.newPasswordModal.modalNewPasswordViewFooterSave,
          onClick : function() {
            model.resetPassword();
          }
        });

     body.append($("<div class='position'/>").append(changePasswordButton.input()));
     $(this.el).append(content);
    }
  });

  window.NewPassword = function(args) {
    var model = new NewPasswordModel(args.linkchangepassword);
    var view =  new NewPasswordView({model: model, el: $("<div class='short-input-section'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
