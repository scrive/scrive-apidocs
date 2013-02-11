/* All things that are shown after person had signed */


(function(window) {

window.CreateAccountAfterSignView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  clearPasswordValidationErrors: function() {
    $(".errormsg",this.password1box).remove();
    $(".errormsg",this.password2box).remove();
  },
  clearTOSValidationErrors: function() {
    $(this.tos).removeClass("invalid");
    $(".errormsg",this.tos).remove();
  },
  filledAndValid: function(passwordinput) {
    var view = this;
    var p1 = this.passwordinput.validate(new PasswordValidation({
          callback: function(t, e, v) {
            $(view.password1box).append($("<div class='errormsg' />").text(v.message()));
          },
          message: localization.validation.passwordLessThanMinLength,
          message_max: localization.validation.passwordExceedsMaxLength,
          message_digits: localization.validation.passwordNeedsLetterAndDigit
      }));
    var p2 = this.password2input.validate(new PasswordEqValidation({
          callback: function(t, e, v) {
            $(view.password2box).append($("<div class='errormsg' />").text(v.message()));
          },
          message: localization.validation.passwordsDontMatch,
          "with": this.passwordinput
      }));
    var p3 = this.checkbox.validate(new CheckboxReqValidation({
          callback: function(t, e, v) {
            $(view.tos).addClass('invalid');
            $(view.tos).append($("<div class='errormsg' />").text(v.message()));
          },
          message: localization.validation.mustAcceptTOS
      }));
    return p1 && p2 && p3;

  },
  render: function() {
    var view = this;
    var container = $("<div class='save'>")
    $(this.el).children().detach();
    $(this.el).append(container);

    if (this.model.document().currentSignatory().saved() || this.model.justSaved()) {
      container.append($("<div class='headline'/>").text(localization.docsignview.createdAccountTitle));
      container.append($("<div class='subheadline'/>").text(localization.docsignview.createdAccountSubtitle));
      container.addClass('done');
      return;
    }
    
    container.addClass('newaccount');
    container.append($("<div class='title' />").text(localization.docsignview.newAccountTitle));
    container.append($("<div class='subtitle' />").text(localization.docsignview.newAccountSubTitle));
    var form = $("<div class='inner' />");
    container.append($("<div class='highlight'/>").append(form));

    var emailrow = $("<div class='item' />");
    emailrow.append($("<div />").append($("<div class='label' />").text(localization.docsignview.emailLabel)));
    emailrow.append($("<div />").append($("<input type='text' class='email' disabled='true'/>").val(this.model.document().currentSignatory().email())));
    form.append(emailrow);
    if (this.passwordinput == undefined) {
        this.passwordinput = $("<input type='password' name='password' autocomplete='off' />");
        this.passwordinput.change(function() {view.clearPasswordValidationErrors();});
        this.passwordinput.keypress(function() {view.clearPasswordValidationErrors();});
    };
    var password1row = $("<div class='item' />");
    password1row.append($("<div />").append($("<div class='label' />").text(localization.docsignview.passwordLabel)));
    this.password1box = $("<div / >");
    password1row.append(this.password1box.append(this.passwordinput));
    form.append(password1row);
                                                        
    if (this.password2input == undefined) {
        this.password2input = $("<input type='password' name='password2' autocomplete='off' />");
        this.password2input.change(function() {view.clearPasswordValidationErrors();});
        this.password2input.keypress(function() {view.clearPasswordValidationErrors();});
    };
    var password2row = $("<div class='item' />");
    password2row.append($("<div />").append($("<div class='label' />").text(localization.docsignview.password2Label)));
    this.password2box = $("<div / >");
    password2row.append(this.password2box.append(this.password2input));
    form.append(password2row);
                                                        
    form.append("<div class='clearfix' />");

    this.tos = $("<div class='checkbox-box'/>");
    if (this.checkbox == undefined) {
        this.checkbox = $("<div class='checkbox'/>");
        this.checkbox.click(function() {view.checkbox.toggleClass('checked'); view.clearTOSValidationErrors();});
    };
    this.tos.append(this.checkbox).append($("<label/>").append(localization.docsignview.acceptTOSLabel));
    this.tos.append($("<div class='clearfix'/>"));
    form.append($("<div class='row'>").append(this.tos));


    var view = this;
    var model = this.model;
    var newAccountButton = Button.init({
      color: "green",
      size: "tiny",
      text: localization.docsignview.newAccountButton,
      onClick: function() {
        view.clearPasswordValidationErrors();
        view.clearTOSValidationErrors();
        if (view.filledAndValid()) {
            mixpanel.track('Create new account');
            mixpanel.people.set({
                'TOS Date'    : new Date(),
                'Full Name'   : model.document().currentSignatory().name(),
                '$first_name' : model.document().currentSignatory().fstname(),
                '$last_name'  : model.document().currentSignatory().sndname(),
                '$email'      : model.document().currentSignatory().email(),
                'Language'    : (model.document().lang() === "gb" ? "en" : "sv"),
                'Signup Method' : 'BySigning'
            });
         new Submit({
           url: model.document().currentSignatory().saveurl(),
           method: "POST",
           acceptaccount: true,
           password: view.passwordinput.val(),
           password2: view.password2input.val(),
           tos: view.checkbox.hasClass("checked") ? "on" : "",
           ajax: true,
           onSend: function() {
             container.empty().append("<div class='saving'/>");
           },
           ajaxsuccess: function(d) {
               d = JSON.parse(d);
             if(d.userid)
               mixpanel.alias(d.userid);
             model.setJustSaved();
           }
         }).send();
        }
      }
    });
    form.append($("<div class='acceptbutton' />").append(newAccountButton.input()));
    form.append("<div class='clearfix' />");

    return this;
  }
});

})(window);
