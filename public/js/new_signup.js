(function(window) {

  var SignupModel = Backbone.Model.extend({
    email: function() {
      return this.get('email');
    },
    setEmail: function(email) {
      this.set('email', email);
    },
    signup: function() {
      var model = this;

      new Submit({
        method: 'POST',
        url: "/signup",
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
    render: function () {
        var model = this.model;
        var header = $("<header/>").append($("<h1 class='big'/>").text(localization.signupModal.modalAccountSetupFooter));
        $(this.el).append(header);

        var content = $("<div class='short-input-container recovery-container'/>");
        var wrapper = $("<div class='short-input-container-body-wrapper'/>");
        var body = $("<div class='short-input-container-body'/>");
        content.append(wrapper.append(body));

        var emailInput = InfoTextInput.init({
          infotext: localization.signupModal.fillinemail,
          value: model.email(),
          onChange: function(v) {model.setEmail(v);},
          cssClass : "big-input",
          inputtype: 'text',
          name: 'email'
        });

        var signupButton = Button.init({
            size  : 'small',
            color : 'blue',
            text: localization.signupModal.modalAccountSetupFooter,
            onClick: function() {
              model.signup();
            }
          });
        body.append($("<div class='position first'/>").append($("<h2>").text(localization.signupModal.startNow)));
        body.append($("<div class='position'/>").append(emailInput.input()).append(signupButton.input()));
        $(this.el).append(content);
      }
  });

  window.Signup = function(args) {
    var model = new SignupModel(args);
    var view =  new SignupView({model: model, el: $("<div class='signup short-input-section'/>")});
    this.el = function() {return $(view.el);}
  };

})(window);
