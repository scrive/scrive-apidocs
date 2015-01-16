// setting new password (after email reset step)

define(['Backbone', 'legacy_code'], function() {

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
      if (password !== password2) {
        return localization.newPasswordModal.flashMessagePasswordsDontMatch;
      } else if (password === '') {
        return localization.newPasswordModal.flashMessageMissingRequiredField;
      } else if (password.length < 8 ||
                 password.replace(/[^a-zA-Z]/g, '').length < 1 ||
                 password.replace(/[^0-9]/g, '').length < 1) {
        return localization.newPasswordModal.flashMessagePasswordInvalid;
      } else if (password.length > 250) {
        return localization.newPasswordModal.flashMessagePasswordExceedsMaxLength;
      } else if (password.match(validCharsRegex) === null) {
        return localization.newPasswordModal.flashMessageInvalidCharsInPassword;
      } else {
        return null;
      }
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
          if (typeof resp === 'string') {
            resp = JSON.parse(resp);
          }
          if (resp.logged === true) {
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
      var header = $("<div style='margin-bottom:20px;margin-top:50px;text-align:center;'/>");
      header.append($("<img alt='logo' src='/login_logo/"+ window.brandinghash +"'/>"));
      header.append($("<div class='divider-line'/>"));

      var poweredLabel = $("<div class='label' style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
      header.append(poweredLabel);

      $(this.el).append(header);

      content.append(wrapper.append(body));

      if (this.options.body)
         body.append($("<label style='padding-bottom: 1em;'/>").text(this.options.body));

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
      body.append($("<div class='position' style='margin-top:6px;'/>").append(password2Input.el()));

      var changePasswordButton = new Button({
          type  : "main",
          text  : this.options.button,
          style : "width:235px",
          onClick : function() {
            model.resetPassword();
          }
        });

     body.append($("<div class='position' style='text-align:center;margin-top:20px'/>").append(changePasswordButton.el()));
     $(this.el).append(content);
    }
  });


  window.NewPassword = function(args) {
    var model = new NewPasswordModel(args);
    var view;
    var options = { model: model,
                    body: null,
                    button: localization.newPasswordModal.modalNewPasswordViewFooterSave
                  };

    if (args.accessnewaccount) {
      options.body = localization.accessNewAccountModal.body;
      options.button = localization.accessNewAccountModal.button;
    }

    options.el = $("<div class='new-password-box'style='width:275px;margin:20px auto'/>");
    view = new NewPasswordView(options);
    this.el = function() {return $(view.el);};
  };

});
