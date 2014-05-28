define(['Backbone', 'legacy_code'], function() {

  var SignupModel = Backbone.Model.extend({
    defaults: {
        logolink : "",
        servicelinkcolour : "",
        textscolour : ""
    },
    autofocus: function() {
      return this.get('autofocus');
    },
    email: function() {
      return this.get('email');
    },
    setEmail: function(email) {
      this.set('email', email);
    },
    logolink : function() {
     return this.get("logolink");
    },
    servicelinkcolour : function() {
      return this.get("servicelinkcolour");
    },
    textscolour : function() {
     return this.get("textscolour");
    },
    buttoncolorclass: function() {
     return this.get("buttoncolorclass");
    },
    clear: function() {
      this.setEmail('');
      this.trigger('clear');
    },
    signup: function() {
      var model = this;
      mixpanel.track('Submit signup');
      new Submit({
        method: 'POST',
        url: "/api/frontend/signup",
        ajax: true,
        lang : Language.current(),
        email: model.email(),
        ajaxsuccess: function(rs) {
          try {
            resp = JSON.parse(rs);
          } catch (e) {
            resp = JSON.parse($(rs).text());
          }
          if (resp.sent === true) {
            mixpanel.track('Create new account', {
                'Email' : model.email()
            });
            mixpanel.people.set({
                '$email'        : model.email()
            });
            var content = localization.payments.outside.confirmAccountCreatedUserHeader;
            new FlashMessage({content: content, color: 'green'});
            model.clear();
          } else if (resp.sent === false) {
            mixpanel.track('Error',
                           {Message : 'signup failed'});
            new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, color: 'red'});
          }
        }
      }).send();
    }
  });

  var SignupView = Backbone.View.extend({
    initialize: function() {
      _.bindAll(this, 'render');
      this.model.bind('clear', this.render);
      this.render();
    },
    validationCallback: function(t, _e , v) {
      $("<div class='validation-failed-msg' />").append(v.message()).appendTo($('.position.withEmail',this.el));
    },
    clearValidationMessages : function() {
      $(".validation-failed-msg",this.el).remove();
    },
    render: function () {
        $(this.el).html('');
        var self = this;
        var model = this.model;
        var header = $("<div class='shadowed'/>");
        header.append($("<h1/>").append(localization.getStartedInstantly));
        header.append($("<h2/>").append(localization.freeDocumentPerMonth));
        $(this.el).append(header);

        var content = $("<div class='short-input-container'/>");
        var wrapper = $("<div class='short-input-container-body-wrapper'/>");
        var body = $("<div class='short-input-container-body'/>");
        content.append(wrapper.append(body));

        var emailInput = new InfoTextInput({
          infotext: localization.email,
          value: model.email(),
          onChange: function(v) {self.clearValidationMessages(); model.setEmail(v);},
          onEnter: function() {
              signupButton.el().click();
          },
          cssClass : "big-input",
          inputtype: 'text',
          name: 'email'
        });

        // Automatically focus the appropriate login field.
        if(model.autofocus()) {
            $(document).ready(function() {
                emailInput.focus();
            });
        }

        var signupButton = new Button({
            size: 'big',
            color : 'green',
            text: localization.signup,
            onClick: function() {
              self.clearValidationMessages();
              if (emailInput.value().validate(new EmailValidation({callback: self.validationCallback, message: localization.validation.wrongEmail})))
                model.signup();
            }
          });

        body.append($("<div class='position first withEmail'/>").append(emailInput.el()).append(signupButton.el()));
        $(this.el).append(content);
      }
  });

  var SignupBrandedView = Backbone.View.extend({
    initialize: function() {
      _.bindAll(this, 'render');
      this.model.bind('clear', this.render);
      this.render();
    },
    validationCallback: function(t, e, v) {
      $("<div class='validation-failed-msg' />").append(v.message()).appendTo($('.position.withEmail',this.el));
    },
    clearValidationMessages : function() {
      $(".validation-failed-msg",this.el).remove();
    },
    render: function () {
        $(this.el).html('');
        $("#page-signup").addClass("button-gray");
        var self = this;
        var model = this.model;

        var content = $("<div style='width:'/>");
        var wrapper = $("<div/>");
        var body = $("<div/>");
        var header = $("<div style='margin-bottom: 103px;text-align: center;'/>");

        header.append($("<img alt='logo'/>").attr('src',model.logolink()));
        header.append($("<div class='divider-line'/>"));

        var poweredLabel = $("<label style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
        if (model.textscolour() != undefined) poweredLabel.css("color",model.textscolour());
        header.append(poweredLabel);

        $(this.el).append(header);

        content.append(wrapper.append(body));

        var signUpLabel = $("<label style='padding-left:10px;'/>").text(localization.signup + ":");
        if (model.textscolour() != undefined) signUpLabel.css("color",model.textscolour());
        body.append($("<div class='position first' style='text-align: left;height:30px;'/>").append(signUpLabel));


        var emailInput = new InfoTextInput({
          infotext: localization.email,
          value: model.email(),
          onChange: function(v) {self.clearValidationMessages(); model.setEmail(v);},
          onEnter: function() {
              signupButton.el().click();
          },
          inputtype: 'text',
          name: 'email'
        });

        // Automatically focus the appropriate login field.
        if(model.autofocus()) {
            $(document).ready(function() {
                emailInput.focus();
            });
        }

        var signupButton = new Button({
            size  : 'tiny',
            color : model.buttoncolorclass(),
            text: localization.signup,
            style : "width:245px;",
            onClick: function() {
              self.clearValidationMessages();
              if (emailInput.value().validate(new EmailValidation({callback: self.validationCallback, message: localization.validation.wrongEmail})))
                model.signup();
            }
          });

        body.append($("<div class='position withEmail'/>").append(emailInput.el().attr("autocomplete","false").css("width","245px").css("padding","7px 14px").css("font-size","16px")));
        body.append($("<div class='position' style='text-align:center;margin-top:10px;'/>").append(signupButton.el()));

        var dontHaveAccount = $("<label class='label-with-link'/>").html(localization.signupModal.alreadyHaveAnAccount);
        dontHaveAccount.find('a').attr('href', '/login');

        var paymentsPage = $("<label class='label-with-link'/>").html(localization.visitOurPricingPage);
        paymentsPage.find('a').attr('href', '/pricing');

        if (model.textscolour() != undefined)  {
          dontHaveAccount.css('color', model.textscolour());
          paymentsPage.css('color', model.textscolour());
        }

        if (model.servicelinkcolour()) {
          dontHaveAccount.find('a').css('color', model.servicelinkcolour());
          paymentsPage.find('a').css('color', model.servicelinkcolour());
        }

        body.append($("<div class='position' style='text-align:center;margin-top:20px;'/>").append(dontHaveAccount).append(paymentsPage));

        $(this.el).append(content);
      }
  });


  window.Signup = function(args) {
    var model = new SignupModel(args);
    var view;
    if (args.branded)
            view = new SignupBrandedView({model : model, el : $("<div style='width:275px;margin:20px auto' />") });
          else
            view = new SignupView({model : model, el : $("<div class='signup short-input-section'/>")});
    this.el = function() {return $(view.el);};
  };

});
