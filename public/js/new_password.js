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
      } else if (password.length < 8) {
        return localization.newPasswordModal.flashMessagePasswordLessThanMinLength;
      } else if (password.length > 250) {
        return localization.newPasswordModal.flashMessagePasswordExceedsMaxLength;
      } else if (password.match(validCharsRegex) === null) {
        return localization.newPasswordModal.flashMessageInvalidCharsInPassword;
      } else if (password.replace(/[^a-zA-Z]/g, '').length < 1) {
        return localization.newPasswordModal.flashMessageNeedsLetterAndDigit;
      } else if (password.replace(/[^0-9]/g, '').length < 1) {
        return localization.newPasswordModal.flashMessageNeedsLetterAndDigit;
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
       var header = $("<div/>").addClass('shadowed').append($("<h1 class='big'/>").text(localization.newPasswordModal.modalNewPasswordViewHeader));
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

      body.append($("<div class='position first' style='text-align: left;height: 30px;'/>").append($("<label style='padding-left:10px;'/>").text(localization.newPasswordModal.modalNewPasswordViewHeader + ":")));


      var passwordInput = InfoTextInput.init({
        infotext: localization.newPasswordModal.modalNewPasswordViewNewPassword,
        value: model.password(),
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      passwordInput.input().attr("autocomplete","false").css("width","245px").css("padding","7px 14px").css("font-size","16px");
      body.append($("<div class='position'/>").append(passwordInput.input()));

      var password2Input = InfoTextInput.init({
        infotext: localization.newPasswordModal.modalNewPasswordViewRepeatPassword,
        value: model.password2(),
        onChange: function(v) {model.setPassword2(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "big-input",
        onEnter : function() {model.resetPassword();}
      });
      password2Input.input().attr("autocomplete","false").css("width","245px").css("padding","7px 14px").css("font-size","16px");
      body.append($("<div class='position' style='margin-top:6px;'/>").append(password2Input.input()));

      var changePasswordButton = Button.init({
          size  : 'tiny',
          color : model.buttoncolorclass(),
          text  : localization.newPasswordModal.modalNewPasswordViewFooterSave,
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

    if (args.branded)
      view = new NewPasswordBrandedView({model: model, el: $("<div style='width:275px;margin:20px auto'/>")});
    else
      view = new NewPasswordView({model: model, el: $("<div class='short-input-section'/>")});

    this.el = function() {return $(view.el);}
  };

})(window);
