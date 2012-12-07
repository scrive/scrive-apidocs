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
      // _.bindAll(this, 'render');
      // this.model.bind('change', this.render);
      this.render();
    },

    popupNewPasswordModal : function() {
      var model = this.model;

      var container = $('<div class="recovery-container"/>');
      var content = $('<div class="body"/>');
      container.append(content);
      var wrapper = $('<div class="wrapper" style="text-align: right;"/>');
      content.append(wrapper);

      var passwordLocalization = localization.newPasswordModal;

      var passwordInput = InfoTextInput.init({
        infotext: '',
        value: model.password(),
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        onEnter : function() {model.resetPassword();}
      });
      passwordInput.input().attr("autocomplete","false");
      wrapper.append($('<span class="txt"/>').text(passwordLocalization.modalNewPasswordViewNewPassword)).append(passwordInput.input()).append("<BR/>");
      passwordInput.input().click();

      var password2Input = InfoTextInput.init({
        infotext: '',
        value: model.password2(),
        onChange: function(v) {model.setPassword2(v);},
        inputtype: 'password',
        name: 'password2',
        onEnter : function() {model.resetPassword();}
      });
      password2Input.input().attr("autocomplete","false");
      wrapper.append($('<span class="txt"/>').text(passwordLocalization.modalNewPasswordViewRepeatPassword)).append(password2Input.input()).append("<BR/>");
      password2Input.input().click();

      Confirmation.popup({
        title: passwordLocalization.modalNewPasswordViewHeader,
        rejectText: localization.cancel,
        content: container,
        mask: {
          color: standardDialogMask,
          top: standardDialogTop,
          loadSpeed: 0,
          opacity: 0.9
        },
        acceptButton:  Button.init({
          size  : 'small',
          color : 'blue',
          text  : passwordLocalization.modalNewPasswordViewFooterSave,
          onClick : function() {
            model.resetPassword();
          }
        }).input()
      });
      passwordInput.input().focus();
    },

    render: function () {
      this.popupNewPasswordModal();
    }
  });

  window.NewPassword = function(args) {
    var model = new NewPasswordModel(args.linkchangepassword);
    var view =  new NewPasswordView({model: model, el: $('<div/>')});
  };

})(window);
