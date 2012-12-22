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
        ajaxsuccess: function(rs) {
          var resp = JSON.parse(rs);
          if (resp.ok === true) {
            window.location = resp.location;
          } else if (resp.error == 'already_active') {
            new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, color: 'red'});
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
      $("<div class='validate-message failed-validation' />").css({'font-size': 8, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(e.parent());
    },
    clearValidationMessages : function() {
      $(".validate-message",this.el).remove();
    },
    termsPageContents: function() {
      var container = $('<div class="nicetext"/>');
      container.append($('<h2/>').append(localization.accountSetupModal.termsPageHeader));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage0));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage1Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1c));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1d));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage1e));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage2Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage2c));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage3Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3b));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3c));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage3d));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage4Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage4));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage5Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage5));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage5a));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage6Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage6));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage7Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage7a));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage7b));
      container.append($('<br/>'));
      container.append($('<h3/>').append(localization.accountSetupModal.termsPage8Header));
      container.append($('<br/>'));
      container.append($('<p/>').append(localization.accountSetupModal.termsPage8));
      container.append($('<br/>'));
      return container;
    },

    render: function () {
      var model = this.model;
      var view = this;
      //var header = $("<header/>").append($("<h1 class='big'/>").text(localization.accountSetupModal.termsPageHeader));
      //$(this.el).append(header);
       
      var content = $("<div class='short-input-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      var body = $("<div class='short-input-container-body'/>");
      $(this.el).append(content.append(wrapper.append(body)));
      

      var terms = $('<div style="max-height: 400px; overflow: auto"/>');
      terms.append(this.termsPageContents());
      body.append(terms);

      var info = $('<div style="padding: 10px 0px"/>');
      body.append(info);

      var tosAccept = $('<div style="padding-bottom:10px;"/>');
      var tosCBox = $("<input type='checkbox' id='tosCBox' name='tos' style='margin-right:10px;margin-top: -2px'/>");
      model.setTosValidator(function() {
        tosCBox.validate(new CheckboxReqValidation({callback: view.validationCallback, message: localization.validation.mustAcceptTOS}));
      });
      tosAccept.append(tosCBox);
      tosAccept.append($('<label for="tosCBox"/>').append(localization.accountSetupModal.modalAccountSetupBodyAcceptTOS));
      tosAccept.append($('<br/>'));
      var signupInfo = $("<div class='signupInfo' style='display:none;'/>");
      info.append(tosAccept).append(signupInfo);
      var firstNameInput = InfoTextInput.init({
        infotext: localization.account.accountDetails.fstname,
        value: model.fstname(),
        onChange: function(v) {model.setFstname(v);},
        inputtype: 'text',
        name: 'fstname',
        cssClass : "big-input"
      });
      
      model.addValidator(function() {
        return firstNameInput.input().validate(new NameValidation({callback: view.validationCallback, message: localization.validation.firstNameRequired}));
      });
      
      signupInfo.append($("<div class='position first'/>").append(firstNameInput.input()));

      tosCBox.change(function() {
        if (tosCBox.attr('checked')) {
          model.setAccepted(true);
          signupInfo.show();
          firstNameInput.input().focus();
        } else {
          model.setAccepted(false);
          signupInfo.hide();
        }
      });

      
      var lastNameInput = InfoTextInput.init({
        infotext: localization.account.accountDetails.sndname,
        value: model.fstname(),
        onChange: function(v) {model.setSndname(v);},
        inputtype: 'text',
        name: 'sndname',
        cssClass : "big-input"
      });

      model.addValidator(function() {
        return lastNameInput.input().validate(new NameValidation({callback: view.validationCallback, message: localization.validation.secondNameRequired}));
      });

      signupInfo.append($("<div class='position'/>").append(lastNameInput.input()));


      var passwordInput = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupChoosePassword,
        value: "",
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "big-input"
      });

      model.addValidator(function() {
        return passwordInput.input().validate(new PasswordValidation({callback: view.validationCallback,
                                                              message: localization.validation.passwordLessThanMinLength,
                                                              message_max: localization.validation.passwordExceedsMaxLength,
                                                              message_digits: localization.validation.passwordNeedsLetterAndDigit}));
      });

      signupInfo.append($("<div class='position'/>").append(passwordInput.input()));
      

      var password2Input = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupRepeatPassword,
        value: "",
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "big-input"
      });

      model.addValidator(function() {
        return password2Input.input().validate(new PasswordEqValidation({callback: view.validationCallback,
                                                                 message: localization.validation.passwordsDontMatch,
                                                                 'with': passwordInput.input()}));
      });

      signupInfo.append($("<div class='position'/>").append(password2Input.input()));


      var callme = $("<div class='position'/>");
      var callmeInput = $('<input type="checkbox" style="padding: 0px; margin: 0px 5px 0px 0px; float: left;" class="callme" id="callme" name="callme"/>');
      callme.append(callmeInput).append($('<label for="callme"/>').append(localization.accountSetupModal.modalAccountSetupCallMe));
      signupInfo.append(callme);


      var phoneInput = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupPhone,
        value: "",
        onChange: function(v) {model.setPhone(v);},
        inputtype: 'text',
        name: 'phone',
        cssClass : "big-input"
       });

      model.setPhoneValidator(function(callme) {
        if (callme) {
          return phoneInput.input().validate(new NotEmptyValidation({callback: view.validationCallback, message: localization.validation.phoneRequired}));
        }
      });
      var phonerow = $("<div class='position' style='display:none;'/>").append(phoneInput.input());
      signupInfo.append(phonerow);

      
      callmeInput.change(function() {
        if (callmeInput.attr('checked')) {
          model.setCallme(true);
          phonerow.show();
        } else {
          model.setCallme(false);
          phonerow.hide();
        }
      });

      var acceptButton = Button.init({
          size: 'small',
          color: 'green',
          text: localization.signupModal.modalAccountSetupFooter,
          onClick: function() {
            view.clearValidationMessages();
            model.signup();
          }
        });
      
      signupInfo.append($("<div class='position'/>").append(acceptButton.input()));
      
    }
  });

  window.AccountSetup = function(args) {
    var model = new AccountSetupModel(args);
    var view =  new AccountSetupView({model: model, el: $("<div class='short-input-section account-setup'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
