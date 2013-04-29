(function(window) {

  var SignupModel = Backbone.Model.extend({
    defaults: {
        logolink : ""
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
          resp = JSON.parse(rs);
          if (resp.sent === true) {
            mixpanel.track('Create new account', {
                'Email' : model.email()
            });
            mixpanel.people.set({
                '$email'        : model.email()
            });
            var content = localization.payments.outside.confirmAccountCreatedUserHeader;
            new FlashMessage({content: content, color: 'green'});
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
      this.render();
    },
    validationCallback: function(t, e, v) {
      $("<div class='validate-message failed-validation float-left' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(e.parent());
    },
    clearValidationMessages : function() {
      $(".validate-message",this.el).remove();
    },
    render: function () {
        $("#page-signup").removeClass("button-red").addClass("button-gray");
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

        var emailInput = InfoTextInput.init({
          infotext: localization.email,
          value: model.email(),
          onChange: function(v) {self.clearValidationMessages(); model.setEmail(v);},
          onEnter: function() {
              signupButton.input().click();
          },
          cssClass : "big-input",
          inputtype: 'text',
          name: 'email'
        });

        // Automatically focus the appropriate login field.
        if(model.autofocus()) {
            $(document).ready(function() {
                emailInput.input().focus();
            });
        }

        var signupButton = Button.init({
            size  : 'small',
            color : 'blue',
            text: localization.signup + " ›",
            onClick: function() {
              self.clearValidationMessages();
              if (emailInput.input().validate(new EmailValidation({callback: self.validationCallback, message: localization.validation.wrongEmail})))
                model.signup();
            }
          });

        body.append($("<div class='position first'/>").append(emailInput.input()).append(signupButton.input()));
        $(this.el).append(content);
      }
  });

  var SignupBrandedView = Backbone.View.extend({
    initialize: function() {
      this.render();
    },
    validationCallback: function(t, e, v) {
      $("<div class='validate-message failed-validation float-left' />").css({'font-size': 12, 'font-weight': 'bold', color: 'red'}).append(v.message()).appendTo(e.parent());
    },
    clearValidationMessages : function() {
      $(".validate-message",this.el).remove();
    },
    render: function () {
        $("#page-signup").removeClass("button-red").addClass("button-gray");
        var self = this;
        var model = this.model;

        var header = $("<div class='shadowed'/>");
        header.append($("<img alt='logo'/>").attr('src',model.logolink()));
        header.append($("<div class='divider-line'/>"));
        header.append($("<label/>").text(localization.esigningpoweredbyscrive));

        $(this.el).append(header);



        var content = $("<div class='short-input-container login' style='border:none;background:none;'/>");
        var wrapper = $("<div class='short-input-container-body-wrapper' style='border:none;background:none;'/>");
        var body = $("<div class='short-input-container-body' style='border:none;background:none;'/>");
        content.append(wrapper.append(body));

        body.append($("<div class='position first' style='text-align: left;'/>").append($("<label style='padding-left:10px;'/>").text(localization.signup + ":")));


        var emailInput = InfoTextInput.init({
          infotext: localization.email,
          value: model.email(),
          onChange: function(v) {self.clearValidationMessages(); model.setEmail(v);},
          onEnter: function() {
              signupButton.input().click();
          },
          cssClass : "big-input",
          inputtype: 'text',
          name: 'email'
        });

        // Automatically focus the appropriate login field.
        if(model.autofocus()) {
            $(document).ready(function() {
                emailInput.input().focus();
            });
        }

        var signupButton = Button.init({
            size  : 'tiny',
            color : 'blue',
            text: localization.signup + " ›",
            style : "width:80px;",
            onClick: function() {
              self.clearValidationMessages();
              if (emailInput.input().validate(new EmailValidation({callback: self.validationCallback, message: localization.validation.wrongEmail})))
                model.signup();
            }
          });

        body.append($("<div class='position'/>").append(emailInput.input()));
        body.append($("<div class='position' style='text-align:right'/>").append(signupButton.input()));
        $(this.el).append(content);
      }
  });


  window.Signup = function(args) {
    var model = new SignupModel(args);
    var view;
    if (args.branded)
            view = new SignupBrandedView({model : model, el : $("<div class='signup short-input-section'/>") });
          else
            view = new SignupView({model : model, el : $("<div class='signup short-input-section'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
