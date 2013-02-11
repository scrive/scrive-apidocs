(function(window) {

  var AccountSetupModel = Backbone.Model.extend({
    defaults: {
      accepted: false,
      password: '',
      callme: false,
      validators: [],
      tosValidator: null
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
    position: function() {
      return this.get('position');
    },
    setPosition: function(position) {
      this.set('position', position);
    },
    company: function() {
      return this.get('company');
    },
    setCompany: function(company) {
      if (!this.companyFilled())
        this.set('company', company);
    },
    userid : function() {
        return this.get('userid');
    },
    companyFilled : function() { // if this is set company should not be altered
      return this.get('companyFilled') == true;
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
        company : model.company(),
        position : model.position(),
        ajaxsuccess: function(rs) {
          var resp = JSON.parse(rs);
          if (resp.ok === true) {
              mixpanel.alias(model.userid());
              mixpanel.people.set({Phone : model.phone(),
                                   'Company name' : model.company(),
                                   'Position' : model.position(),
                                   'First Name' : model.fstname(),
                                   'Last Name' : model.sndname()});
              trackTimeout('Sign TOS', {}, function() {
                  window.location = resp.location;
              });
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
      $("<div class='validate-message failed-validation' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(e.parent());
    },
    clearValidationMessages : function() {
      $(".validate-message",this.el).remove();
    },
    render: function () {
      var model = this.model;
      var view = this;
      //$(this.el).append(header);

      var content = $("<div class='short-input-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper float-left'/>");
      var acceptButtonBox = $("<div class='short-input-container-right float-right'></div>");
      var body = $("<div class='short-input-container-body'/>");
      $(this.el).append(content.append(wrapper.append(body)).append(acceptButtonBox));



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

      body.append($("<div class='position first'/>").append(firstNameInput.input()));

      var lastNameInput = InfoTextInput.init({
        infotext: localization.account.accountDetails.sndname,
        value: model.sndname(),
        onChange: function(v) {model.setSndname(v);},
        inputtype: 'text',
        name: 'sndname',
        cssClass : "big-input"
      });

      model.addValidator(function() {
        return lastNameInput.input().validate(new NameValidation({callback: view.validationCallback, message: localization.validation.secondNameRequired}));
      });

      body.append($("<div class='position'/>").append(lastNameInput.input()));


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

      body.append($("<div class='position'/>")
                    .append($("<label style='text-align:left;margin-left: 5px;width:100%'></label>").text(localization.accountSetupModal.modalAccountPasswordRequirements))
                    .append(passwordInput.input()));


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

      body.append($("<div class='position'/>").append(password2Input.input()));

      var tosAccept = $("<div class='position checkbox-box' style='text-align: left;'/>");
      var tosCBox = $("<div class='checkbox' name='tos' style='margin-left:3px'/>");
      if (model.accepted()) tosCBox.addClass('checked');
      tosCBox.click(function() { tosCBox.toggleClass('checked'); model.setAccepted(tosCBox.hasClass('checked'));});


      model.setTosValidator(function() {
        tosCBox.validate(new CheckboxReqValidation({callback: view.validationCallback, message: localization.validation.mustAcceptTOS}));
      });
      tosAccept.append(tosCBox);
      var thref = "http://" + location.host + location.pathname.substring(0, 3) + "/terms";
      tosAccept.append($('<label/>')
                  .append($("<span/>").text(localization.accountSetupModal.modalAccountSetupBodyAccept))
                  .append($("<a class='clickable' target='_blank'/>").attr('href',thref).text(" " + localization.accountSetupModal.modalAccountSetupBodyTOS))
                );
      tosAccept.append($('<br/>'));

      body.append(tosAccept);


      var optionaldescriptionrow = $("<div class='position' style='text-align:left;'/>")
        .append($("<label style='text-align:left;margin-left: 5px'></label>").text(localization.accountSetupModal.modalAccountOptionalPositions));
      body.append(optionaldescriptionrow);

      var companyInput = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupCompany,
        value: model.company(),
        onChange: function(v) {model.setCompany(v);},
        inputtype: 'text',
        name: 'position',
        cssClass : "big-input"
       });
      if (this.model.companyFilled())
        companyInput.input().attr("readonly","true");
      var companyrow = $("<div class='position'/>").append(companyInput.input());

      body.append(companyrow);

      var positionInput = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupPosition,
        value: "",
        onChange: function(v) {model.setPosition(v);},
        inputtype: 'text',
        name: 'position',
        cssClass : "big-input"
       });

      var positionrow = $("<div class='position'/>").append(positionInput.input());

      body.append(positionrow);

      var phoneInput = InfoTextInput.init({
        infotext: localization.accountSetupModal.modalAccountSetupPhone,
        value: "",
        onChange: function(v) {model.setPhone(v);},
        inputtype: 'text',
        name: 'phone',
        cssClass : "big-input"
       });

      var phonerow = $("<div class='position'/>").append(phoneInput.input());
      body.append(phonerow);




      var acceptButton = Button.init({
          size: 'small',
          color: 'blue',
          text: localization.signupModal.modalAccountSetupFooter,
          onClick: function() {
            view.clearValidationMessages();
            model.signup();
          }
        });

      acceptButtonBox.append($("<h3></h3>").append(localization.signupModal.mainHeader)).append(acceptButton.input());

    }
  });

  window.AccountSetup = function(args) {
    var model = new AccountSetupModel(args);
    var view =  new AccountSetupView({model: model, el: $("<div class='short-input-section account-setup'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
