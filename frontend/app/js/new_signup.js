define(['common/hubspot_service', 'common/adwords_conversion_service', 'Backbone', 'legacy_code'], function(HubSpot, AdwordsConversionService) {

  var SignupModel = Backbone.Model.extend({
    defaults: {
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
            _gaq.push(['_trackEvent', 'Signup', 'Clicked']);
            AdwordsConversionService.markAsSignupConversion();
            mixpanel.track('Create new account', {
                'Email' : model.email()
            });
            mixpanel.people.set({
                '$email'        : model.email()
            });
            var content = localization.payments.outside.confirmAccountCreatedUserHeader;
            new FlashMessage({content: content, type: 'success'});
            model.clear();
          } else if (resp.sent === false) {
            mixpanel.track('Error',
                           {Message : 'signup failed'});
            new FlashMessage({content: localization.accountSetupModal.flashMessageUserAlreadyActivated, type: 'error'});
          }
        }
      }).send();
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
        var header = $("<div style='margin-bottom:20px;margin-top:50px;text-align: center;'/>");

        header.append($("<img alt='logo' src='/login_logo/"+ window.brandinghash +"'/>"));
        header.append($("<div class='divider-line'/>"));

        var poweredLabel = $("<span class='label' style='text-align:center;width:275px;'/>").text(localization.esigningpoweredbyscrive);
        header.append(poweredLabel);

        $(this.el).append(header);

        content.append(wrapper.append(body));

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
            size  : 'small',
            type : 'main',
            text: localization.signup,
            style : "width:235px;",
            onClick: function() {
              self.clearValidationMessages();
              if (emailInput.value().validate(new EmailValidation({callback: self.validationCallback, message: localization.validation.wrongEmail})))
                {
                  model.signup();
                  HubSpot.track(HubSpot.FORM_SIGNUP,
                                {
                                    email : emailInput.value(),
                                    language : Language.current(),
                                    scrive_domain : location.hostname,
                                    signup_method : "AccountRequest"
                                });
                }
            }
          });

        body.append($("<div class='position first withEmail'/>").append(emailInput.el().attr("autocomplete","false").css("width","245px").css("padding","7px 14px").css("font-size","16px")));
        body.append($("<div class='position' style='text-align:center;margin-top:20px;'/>").append(signupButton.el()));

        var alreadyHaveAccount = $("<span class='label-with-link'/>").html(localization.signupModal.alreadyHaveAnAccount);
        alreadyHaveAccount.find('.put-link-to-login-here').attr('href', "/" + localization.code + '/login');


        body.append($("<div class='position' style='text-align:center;margin-top:20px;'/>").append(alreadyHaveAccount));

        $(this.el).append(content);
      }
  });


  window.Signup = function(args) {
    var model = new SignupModel(args);
    var view = new SignupBrandedView({model : model, el : $("<div style='width:275px;margin:20px auto' class='signup-box'/>") });
    this.el = function() {return $(view.el);};
  };

});
