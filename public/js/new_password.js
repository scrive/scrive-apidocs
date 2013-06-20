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
    logolink : function() {
     return this.get("logolink");
    },
    buttoncolorclass: function() {
     return this.get("buttoncolorclass");
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
        new FlashMessage({content: validationError, color: 'red'});
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
       var header = $("<div/>").addClass('shadowed').append($("<h1 class='big'/>").text(this.options.header));
       $(this.el).append(header);
       if (this.options.body)
         $(this.el).append($("<div style='padding: 0 0 3em;'/>").append($("<h4 style='color: #FFFFFF; text-shadow: 0 2px 3px rgba(0, 0, 0, 0.45);'/>").text(this.options.body)));


      var content = $("<div class='short-input-container recovery-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      content.append(wrapper.append(body));

      var passwordInput = new InfoTextInput({
        infotext: localization.newPasswordModal.modalNewPasswordViewNewPassword,
        value: model.password(),
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      body.append($("<div class='position first'/>").append(passwordInput.el()));

      var password2Input = new InfoTextInput({
        infotext: localization.newPasswordModal.modalNewPasswordViewRepeatPassword,
        value: model.password2(),
        onChange: function(v) {model.setPassword2(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      body.append($("<div class='position'/>").append(password2Input.el()));

      var changePasswordButton = Button.init({
          size  : 'small',
          color : 'blue',
          text  : this.options.button,
          onClick : function() {
            model.resetPassword();
          }
        });

     body.append($("<div class='position'/>").append(changePasswordButton.input()));
     $(this.el).append(content);
    }
  });

  var NewPasswordBrandedView = Backbone.View.extend({
    initialize: function() {
      this.render();
    },

    render: function () {
      var model = this.model;




      var content = $("<div style='width:'/>");
      var wrapper = $("<div/>");
      var body = $("<div/>");
      var header = $("<div style='margin-bottom: 103px'/>");
      header.append($("<img alt='logo'/>").attr('src',model.logolink()));
      header.append($("<div class='divider-line'/>"));
      header.append($("<label style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive));
      $(this.el).append(header);

      content.append(wrapper.append(body));

      body.append($("<div class='position first' style='text-align: left;height: 30px;'/>").append($("<label style='padding-left:10px;'/>").text(this.options.header)));
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
      body.append($("<div class='position'/>").append(passwordInput.el()));

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

      var changePasswordButton = Button.init({
          size  : 'tiny',
          color : model.buttoncolorclass(),
          text  : this.options.button,
            style : "width:245px;",
          onClick : function() {
            model.resetPassword();
          }
        });

     body.append($("<div class='position' style='text-align:center;margin-top:10px'/>").append(changePasswordButton.input()));
     $(this.el).append(content);
    }
  });


  window.NewPassword = function(args) {
    var model = new NewPasswordModel(args);
    var view;
    var options = { model: model,
                    header: localization.newPasswordModal.modalNewPasswordViewHeader,
                    body: null,
                    button: localization.newPasswordModal.modalNewPasswordViewFooterSave
                  };

    if (args.accessnewaccount) {
      options.header = localization.accessNewAccountModal.header;
      options.body = localization.accessNewAccountModal.body;
      options.button = localization.accessNewAccountModal.button;
    }

    if (args.branded) {
      if (!args.accessnewaccount)
        options.header = options.header + ':';
      options.el = $("<div style='width:275px;margin:20px auto'/>");
      view = new NewPasswordBrandedView(options);
    } else {
      options.el = $("<div class='short-input-section'/>");
      view = new NewPasswordView(options);
    }

    this.el = function() {return $(view.el);};
  };

})(window);
