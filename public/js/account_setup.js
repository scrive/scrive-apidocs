(function(window) {

  var AccountSetupModel = Backbone.Model.extend({
    defaults: {
      accepted: false,
      password: '',
      callme: false,
      validators: [],
      tosValidator: null,
      phoneValidator: null
    },
    signuplink: function() {
      return this.get('signuplink');
    },
    fstname: function() {
      return this.get('fstname');
    },
    setFstname: function(fstname) {
      this.set('fstname', fstname);
    },
    sndname: function() {
      return this.get('sndname');
    },
    setSndname: function(sndname) {
      this.set('sndname', sndname);
    },
    password: function() {
      return this.get('password');
    },
    setPassword: function(password) {
      this.set('password', password);
    },
    phone: function() {
      return this.get('phone');
    },
    setPhone: function(phone) {
      this.set('phone', phone);
    },
    accepted: function() {
      return this.get('accepted');
    },
    setAccepted: function(accepted) {
      this.set('accepted', accepted);
    },
    validators: function() {
      return this.get('validators');
    },
    addValidator: function(validator) {
      this.validators().push(validator);
    },
    tosValidator: function() {
      return this.get('tosValidator');
    },
    setTosValidator: function(validator) {
      this.set('tosValidator', validator);
    },
    phoneValidator: function() {
      return this.get('phoneValidator');
    },
    setPhoneValidator: function(validator) {
      this.set('phoneValidator', validator);
    },
    callme: function() {
      return this.get('callme');
    },
    setCallme: function(callme) {
      this.set('callme', callme);
    },
    valid: function() {
      if (!this.accepted()) {
        var validator = this.tosValidator();
        if (validator !== null) {
          validator();
        }
        return false;
      }
      var result = true;
      $.map(this.validators(), function(validator) {
        if (!validator()) {
          result = false;
        }
      });
      if (this.callme()) {
        validator = this.phoneValidator();
        if (validator !== null && !validator(this.callme())) {
          result = false;
        }
      }
      return result;
    },
    signup: function() {
      var model = this;

      if (!model.valid()) {
        return;
      }

      new Submit({
        method: 'POST',
        url: model.signuplink(),
        ajax: true,
        tos: 'on',
        fstname: model.fstname(),
        sndname: model.sndname(),
        password: model.password(),
        password2: model.password(), // validated on the client side that they're equal
        phone: model.phone(),
        ajaxsuccess: function(resp) {
          var translation = localization.accountSetupModal;
          if (resp.ok === true) {
            window.location = resp.location;
          } else if (resp.error == 'already_active') {
            FlashMessages.add({content: translation.flashMessageUserAlreadyActivated,
                               color: 'red'});
          } else if (resp.error == 'reload') {
            model.trigger('reload');
          }          
        }
      }).send();
    }
  });

  var AccountSetupView = Backbone.View.extend({
    initialize: function() {
      this.render();
      _.bindAll(this, 'validationCallback');
      _.bindAll(this, 'render');
      this.model.bind('reload', this.render);
    },

    validationCallback: function(t, e, v) {
      e.css('background', 'red');
      $('<div/>').attr('name', 'validate-message').addClass('failed-validation').css({'font-size': 8, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(e.parent());
    },

    termsPageContents: function() {
      var translation = localization.accountSetupModal;
      var container = $('<div class="nicetext"/>');
      container.append($('<h1/>').append(translation.termsPageHeader));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage0));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage1Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1a));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1b));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1c));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1d));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage1e));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage2Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage2));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage2a));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage2b));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage2c));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage3Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage3));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage3a));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage3b));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage3c));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage3d));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage4Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage4));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage5Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage5));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage5a));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage6Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage6));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage7Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage7a));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage7b));
      container.append($('<br/>'));
      container.append($('<h2/>').append(translation.termsPage8Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(translation.termsPage8));
      container.append($('<br/>'));
      return container;
    },

    popupAccountSetupModal: function() {
      var model = this.model;
      var view = this;
      var translation = localization.accountSetupModal;

      var container = $('<div style="width: 100%;"/>');

      var terms = $('<div style="max-height: 300px; overflow: auto"/>');
      terms.append(this.termsPageContents());
      container.append(terms);

      var info = $('<div style="padding: 0 19px;"/>');
      container.append(info);

      var tosAccept = $('<div style="padding-bottom:10px;"/>');
      var tosCBox = $('<input type="checkbox" id="tosCBox" name="tos" />');
      model.setTosValidator(function() {
        tosCBox.validate(new CheckboxReqValidation({callback: view.validationCallback, message: localization.validation.mustAcceptTOS}));
      });
      tosAccept.append(tosCBox);
      tosAccept.append($('<label for="tosCBox"/>').append(translation.modalAccountSetupBodyAcceptTOS));
      tosAccept.append($('<br/>'));
      info.append(tosAccept);

      var signupInfoTable = $('<table id="signupInfo"/>');
      signupInfoTable.hide();
      var signupInfo = $('<tbody/>');
      signupInfoTable.append(signupInfo);
      info.append(signupInfoTable);

      var firstName = $('<tr/>');
      firstName.append($('<td/>').append(localization.account.accountDetails.fstname));
      var firstNameInput = $('<input type="text" name="fstname" value="" autocomplete="off"/>');
      firstNameInput.val(model.fstname());
      model.addValidator(function() {
        return firstNameInput.validate(new NameValidation({callback: view.validationCallback, message: localization.validation.firstNameRequired}));
      });
      firstName.append($('<td/>').append(firstNameInput));
      signupInfo.append(firstName);
      firstNameInput.change(function() {
        model.setFstname($(this).val());
      });

      tosCBox.change(function() {
        if (tosCBox.attr('checked')) {
          model.setAccepted(true);
          signupInfoTable.show();
          firstNameInput.focus();
        } else {
          model.setAccepted(false);
          signupInfoTable.hide();
        }
      });

      var lastName = $('<tr/>');
      lastName.append($('<td/>').append(localization.account.accountDetails.sndname));
      var lastNameInput = $('<input type="text" name="sndname" value="" autocomplete="off"/>');
      model.addValidator(function() {
        return lastNameInput.validate(new NameValidation({callback: view.validationCallback, message: localization.validation.secondNameRequired}));
      });
      lastNameInput.val(model.sndname());
      lastName.append($('<td/>').append(lastNameInput));
      signupInfo.append(lastName);
      lastNameInput.change(function() {
        model.setSndname($(this).val());
      });

      var password = $('<tr/>');
      password.append($('<td style="width: 120px"/>').append(translation.modalAccountSetupChoosePassword));
      var passwordInput = $('<input type="password" name="password" autocomplete="off"/>');
      model.addValidator(function() {
        return passwordInput.validate(new PasswordValidation({callback: view.validationCallback,
                                                              message: localization.validation.passwordLessThanMinLength,
                                                              message_max: localization.validation.passwordExceedsMaxLength,
                                                              message_digits: localization.validation.passwordNeedsLetterAndDigit}));
      });
      password.append($('<td/>').append(passwordInput));
      signupInfo.append(password);
      passwordInput.change(function() {
        model.setPassword($(this).val());
      });

      var password2 = $('<tr/>');
      password2.append($('<td/>').append(translation.modalAccountSetupRepeatPassword));
      var password2Input = $('<input type="password" name="password2" autocomplete="off"/>');
      model.addValidator(function() {
        return password2Input.validate(new PasswordEqValidation({callback: view.validationCallback,
                                                                 message: localization.validation.passwordsDontMatch,
                                                                 'with': passwordInput}));
      });
      password2.append($('<td/>').append(password2Input));
      signupInfo.append(password2);

      var callme = $('<tr style="height:20px;"/>');
      var callmeInput = $('<input type="checkbox" style="padding: 0px; margin: 0px 5px 0px 0px; float: left;" class="callme" id="callme" name="callme"/>');
      callme.append($('<td colspan="2"/>').append(callmeInput).append($('<label for="callme"/>').append(translation.modalAccountSetupCallMe)));
      callme.append($('<td/>'));
      signupInfo.append(callme);

      var phonerow = $('<tr class="phonerow" style="display: none;"/>');
      phonerow.append($('<td/>').append(translation.modalAccountSetupPhone));
      var phoneInput = $('<input type="text" name="phone" autocomplete="off"/>');
      phonerow.append($('<td/>').append(phoneInput));
      signupInfo.append(phonerow);
      phoneInput.change(function() {
        model.setPhone($(this).val());
      });

      model.setPhoneValidator(function(callme) {
        if (callme) {
          return phoneInput.validate(new NotEmptyValidation({callback: view.validationCallback, message: localization.validation.phoneRequired}));
        }
      });

      callmeInput.change(function() {
        if (callmeInput.attr('checked')) {
          model.setCallme(true);
          phonerow.show();
        } else {
          model.setCallme(false);
          phonerow.hide();
        }
      });

      Confirmation.popup({
        title: translation.PleaseAcceptTOS,
        rejectText: null,
        content: container.children(),
        mask: {
          color: standardDialogMask,
          top: standardDialogTop,
          loadSpeed: 0,
          opacity: 0.9
        },
        acceptButton: Button.init({
          icon: $('<div class="btn-symbol green arrow-left"/>'),
          size: 'small',
          color: 'green',
          text: localization.signupModal.modalAccountSetupFooter,
          onClick: function() {
            model.signup();
          }
        }).input()
      });
      $('.modal-body').css('padding', '10px');
      $('.modal-container').wrap($('<div class="modalbox overlay" style="display: block;"/>'));
    },

    render: function () {
      this.popupAccountSetupModal();
    }
  });

  window.AccountSetup = function(args) {
    var model = new AccountSetupModel(args);
    var view =  new AccountSetupView({model: model, el: $('<div/>')});
  };

})(window);
