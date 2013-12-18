(function(window) {

  var AccountSetupModel = Backbone.Model.extend({
    defaults: {
      accepted: false,
      password: '',
      callme: false,
      validators: [],
      tosValidator: null
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
      if (this.companyAdmin())
        this.set('company', company);
    },
    userid : function() {
        return this.get('userid');
    },
    companyAdmin : function() {
      return this.get('companyAdmin') == true;
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
    cleanValidators : function() {
      this.set({'vaidators' : [] });
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
    buttoncolorclass : function() {
      return this.get("buttoncolorclass");
    },
    branded : function() {
      return this.get("branded");
    },
    servicelinkcolour : function() {
      return this.get("servicelinkcolour");
    },
    callme: function() {
      return this.get('callme');
    },
    setCallme: function(callme) {
      this.set('callme', callme);
    },
      signupMethod : function() {
          return this.get('signupmethod');
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
              mixpanel.alias(resp.userid);
              mixpanel.identify(resp.userid);
              var ps = {'Phone' : model.phone(),
                        'Company Name' : model.company(),
                        'Position' : model.position(),
                        '$first_name' : model.fstname(),
                        '$last_name' : model.sndname(),
                        'TOS Date' : new Date(),
                        'Signup Method' : model.signupMethod()};
              mixpanel.register(ps);
              mixpanel.people.set(ps);
              trackTimeout('Sign TOS', {}, function() {
                  window.location = resp.location;
              }, 1000);
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
      _.bindAll(this, 'render');
      this.model.bind('reload', this.render);
    },

    clearValidationMessages : function() {
      $(".validate-message",this.el).remove();
    },
    render: function () {
      var model = this.model;
      var view = this;
      model.cleanValidators();
      $(this.el).empty();

      var content = $("<div class='short-input-container'/>");
      var wrapper = $("<div class='short-input-container-body-wrapper'/>");
      wrapper.css('margin','auto');

      var body = $("<div class='short-input-container-body'/>");
      $(this.el).append(content.append(wrapper.append(body)));



      var nameInput = new InfoTextInput({
        infotext: localization.account.accountDetails.fullname,
        value: model.sndname() ? model.fstname() + ' ' + model.sndname() : model.fstname(),
        onChange: function(v) {
          var words = _.filter(v.split(' '), function(x) { return x.trim() != '';});
          var firstName = words[0];
          if (firstName === undefined) {
            firstName = '';
          }
          var lastName = _.rest(words).join(' ');
          model.setFstname(firstName);
          model.setSndname(lastName);
        },
        inputtype: 'text',
        name: 'fullname',
        cssClass : "big-input"
      });

      model.addValidator(function() {
        return nameInput.value().validate(new UserNameValidation({
          callback: function(t, e, v) {
            $("<div class='validate-message failed-validation' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(nameInput.el().parent());
          },
          firstName: localization.validation.firstNameField,
          lastName: localization.validation.lastNameField}));
      });

      body.append($("<div class='position first'/>").append(nameInput.el()));

      var companyInput = new InfoTextInput({
        infotext: localization.accountSetupModal.modalAccountSetupCompany,
        value: model.company(),
        onChange: function(v) {model.setCompany(v);},
        inputtype: 'text',
        name: 'company',
        cssClass : "med-input med-input-left",
        readonly : !this.model.companyAdmin()
       });

      var companyrow = $("<div class='position'/>").append(companyInput.el());

      var positionInput = new InfoTextInput({
        infotext: localization.accountSetupModal.modalAccountSetupPosition,
        value: "",
        onChange: function(v) {model.setPosition(v);},
        inputtype: 'text',
        name: 'position',
        cssClass : "med-input"
       });

      companyrow.append(positionInput.el());

      body.append(companyrow);

      var phoneInput = new InfoTextInput({
        infotext: localization.accountSetupModal.modalAccountSetupPhone,
        value: "",
        onChange: function(v) {model.setPhone(v);},
        inputtype: 'text',
        name: 'phone',
        cssClass : "big-input"
       });

      var phonerow = $("<div class='position'/>").append(phoneInput.el());
      body.append(phonerow);

      var passwordInput = new InfoTextInput({
        infotext: localization.accountSetupModal.modalAccountSetupChoosePassword,
        value: "",
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password',
        cssClass : "med-input med-input-left"
      });

      model.addValidator(function() {
        return passwordInput.value().validate(new PasswordValidation({
                    callback: function(t, e, v) {
                      $("<div class='validate-message failed-validation' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(passwordInput.el().parent());
                    },
                    message: localization.validation.passwordLessThanMinLength,
                    message_max: localization.validation.passwordExceedsMaxLength,
                    message_digits: localization.validation.passwordNeedsLetterAndDigit}));
      });

      var password2Input = new InfoTextInput({
        infotext: localization.accountSetupModal.modalAccountSetupRepeatPassword,
        value: "",
        onChange: function(v) {model.setPassword(v);},
        inputtype: 'password',
        name: 'password2',
        cssClass : "med-input"
      });

      model.addValidator(function() {
        return password2Input.value().validate(new PasswordEqValidation({
                  callback: function(t, e, v) {
                      $("<div class='validate-message failed-validation' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(password2Input.el().parent());
                  },
                  message: localization.validation.passwordsDontMatch,
                  'with': function() {return passwordInput.value();}
        }));
      });

      body.append($("<div class='position'/>")
                    .append($("<label style='text-align:left;margin-left: 32px;width:100%'></label>").text(localization.accountSetupModal.modalAccountPasswordRequirements))
                    .append(passwordInput.el())
                    .append(password2Input.el()));

      var tosAccept = $("<div class='checkbox-box' style='text-align: left;'/>");
      var tosCBox = $("<div class='checkbox' name='tos' style='margin-left:3px'/>");
      if (model.accepted()) tosCBox.addClass('checked');
      tosCBox.click(function() { tosCBox.toggleClass('checked'); model.setAccepted(tosCBox.hasClass('checked'));});


      model.setTosValidator(function() {
        tosCBox.validate(new CheckboxReqValidation({
          callback: function(t, e, v) {
                      $("<div class='validate-message failed-validation' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(tosCBox.parent());
          },
          message: localization.validation.mustAcceptTOS
        }));
      });
      tosAccept.append(tosCBox);
      var thref = "http://" + location.host + location.pathname.substring(0, 3) + "/terms";
      var toslink = $("<a class='clickable' target='_blank'/>").attr('href',thref).text(" " + localization.accountSetupModal.modalAccountSetupBodyTOS);
      if (model.servicelinkcolour()) {
        toslink.css('color', model.servicelinkcolour());
      }
      tosAccept.append($('<label/>')
                  .append($("<span/>").text(localization.accountSetupModal.modalAccountSetupBodyAccept))
                  .append(toslink)
                );
      tosAccept.append($('<br/>'));

      body.append(tosAccept);

      var acceptButton = new Button({
          size: 'small',
          color: this.model.buttoncolorclass(),
          text: localization.signupModal.modalAccountSetupFooter,
          onClick: function() {
            view.clearValidationMessages();
            model.signup();
          }
        });

      body.append($('<div class="position"/>').append(acceptButton.el()));
    }
  });

  window.AccountSetup = function(args) {
      mixpanel.track('Visit account setup');
    var model = new AccountSetupModel(args);
    var view =  new AccountSetupView({model: model, el: $("<div class='short-input-section account-setup'/>")});
    this.el = function() {return $(view.el);};
  };

})(window);
