(function(window) {

  var SignupModel = Backbone.Model.extend({
    email: function() {
      return this.get('email');
    },
    setEmail: function(email) {
      this.set('email', email);
    },
    signuplink: function() {
      return this.get('signuplink');
    },
    signup: function() {
      var model = this;

      new Submit({
        method: 'POST',
        url: model.signuplink(),
        ajax: true,
        email: model.email(),
        ajaxsuccess: function(resp) {
          if (resp.sent === true) {
            var content = localization.payments.outside.confirmAccountCreatedUserHeader;
            FlashMessages.add({content: content, color: 'green'});
          }
        }
      }).send();
    }
  });

  var SignupView = Backbone.View.extend({
    initialize: function() {
      this.render();
    },

    popupSignupModal : function() {
      var model = this.model;

      var container = $('<div class="recovery-container"/>');
      var content = $('<div class="body"/>');
      container.append(content);
      var wrapper = $('<div class="wrapper" style="text-align: right;"/>');
      content.append(wrapper);

      var signupLocalization = localization.signupModal;

      var emailInput = InfoTextInput.init({
        infotext: signupLocalization.fillinemail,
        value: model.email(),
        onChange: function(v) {model.setEmail(v);},
        inputtype: 'text',
        name: 'email'
      });
      wrapper.append($('<span class="txt"/>').text(signupLocalization.emailAdress)).append(emailInput.input()).append('<BR/>');
      emailInput.input().click();

      Confirmation.popup({
        title: signupLocalization.modalAccountSetupFooter,
        rejectText: localization.cancel,
        content: container,
        mask: {
          color: standardDialogMask,
          top: standardDialogTop,
          loadSpeed: 0,
          opacity: 0.9
        },
        acceptButton: Button.init({
          size: 'small',
          color: 'blue',
          text: signupLocalization.modalAccountSetupFooter,
          onClick: function() {
            model.signup();
          }
        }).input()
      });
      emailInput.input().focus();
    },

    render: function () {
      this.popupSignupModal();
    }
  });

  window.Signup = function(args) {
    var model = new SignupModel(args);
    var view =  new SignupView({model: model, el: $('<div/>')});
  };

})(window);
