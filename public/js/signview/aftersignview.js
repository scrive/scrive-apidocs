/* All things that are shown after person had signed */


(function(window) {

window.DocumentSaveAfterSignView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  clearValidationErrors: function(elem) {
    elem.removeClass("invalid");
    if (elem.parent() != undefined) {
      elem.parent().find(".errormsg").hide();
    }
  },
  clearValidationErrorsOnChange: function(elem) {
    var view = this;
    var clear = function() {
      view.clearValidationErrors(elem);
    };
    elem.change(clear);
    elem.keydown(clear);
  },
  addValidationError: function(elem, msg) {
    elem.addClass("invalid");
    elem.parent().append($("<div class='errormsg' />").text(msg));
  },
  createValidationFunction: function(inputelem, displayelem, createValidation) {
    var view = this;
    view.clearValidationErrorsOnChange(displayelem);
    return function() {
      view.clearValidationErrors(displayelem);
      var callback = function(t, e, v) {
        view.addValidationError(displayelem, v.message());
      }
      var validation = createValidation(callback);
      return inputelem.validate(validation);
    };
  },
  createPasswordValidationFunction: function(passwordinput) {
    return this.createValidationFunction(passwordinput, passwordinput, function(callback) {
      return new PasswordValidation({
          callback: callback,
          message: localization.validation.passwordLessThanMinLength,
          message_max: localization.validation.passwordExceedsMaxLength,
          message_digits: localization.validation.passwordNeedsLetterAndDigit
      });
    });
  },
  createPassword2ValidationFunction: function(passwordinput, password2input) {
    return this.createValidationFunction(password2input, password2input, function(callback) {
      return new PasswordEqValidation({
          callback: callback,
          message: localization.validation.passwordsDontMatch,
          "with": passwordinput
      });
    });
  },
  createTOSValidationFunction: function(checkbox, toswrapper) {
    return this.createValidationFunction(checkbox, toswrapper, function(callback) {
      return new CheckboxReqValidation({
          callback: callback,
          message: localization.validation.mustAcceptTOS
      });
    });
  },
  createNewAccountElems: function() {
    var container = $("<div class='newaccount'/>");
    container.append($("<div class='title' />").text(localization.docsignview.newAccountTitle));
    container.append($("<div class='subtitle' />").text(localization.docsignview.newAccountSubTitle));

    var form = $("<div class='highlight'/>");
    var inner = $("<div class='inner' />");
      form.append(inner);
      container.append(form);
      form = inner;

    var appendFormInput = function(labeltext, input) {
      var row = $("<div class='item' />");
      row.append($("<div />").append($("<div class='label' />").text(labeltext)));
      row.append($("<div />").append(input));
      form.append(row);
    };

    appendFormInput(localization.docsignview.emailLabel, ($("<input type='text' class='email'/>").attr("disabled", "true")).val(this.model.email()));

    var passwordinput = $("<input type='password' name='password' autocomplete='off' />");
    appendFormInput(localization.docsignview.passwordLabel, passwordinput);

    var password2input = $("<input type='password' name='password2' autocomplete='off' />");
    appendFormInput(localization.docsignview.password2Label, password2input);

    form.append("<div class='clearfix' />");

    var tos = $("<div class='tos'/>");
    var checkbox = $("<input type='checkbox'  id='tosCBox' autocomplete='off'/>");
    tos.append($("<div class='check'/>").append(checkbox));

    var toslabel = $("<label for='tosCBox'/>");
    tos.append($("<div class='label'/>").append(localization.docsignview.acceptTOSLabel));
    tos.append($("<div class='clearfix'/>"));
    form.append($("<div class='row'>").append(tos));

    var validatePassword = this.createPasswordValidationFunction(passwordinput);
    var validatePassword2 = this.createPassword2ValidationFunction(passwordinput, password2input);
    var validateTOS = this.createTOSValidationFunction(checkbox, tos);

    var view = this;
    var model = this.model;
    var newAccountButton = Button.init({
      color: "green",
      size: "small",
      text: localization.docsignview.newAccountButton,
      onClick: function() {

        var passwordvalid = validatePassword();
        var password2valid = validatePassword2();
        var tosvalid = validateTOS();

        if (passwordvalid &&
              password2valid &&
              tosvalid) {
         new Submit({
           url: model.saveurl(),
           method: "POST",
           acceptaccount: true,
           password: passwordinput.val(),
           password2: password2input.val(),
           tos: checkbox.val(),
           ajax: true,
           onSend: function() {
             console.log("creating account");
             model.setSaving(true);
           },
           ajaxerror: function(d, a) {
             console.error("failed to create an account");
             model.setSaving(false);
           },
           ajaxsuccess: function(d) {
             console.log("successfully created account");
             model.setSaved();
             console.log(
               model.document().currentSignatory());
           }
         }).send();
        }
      }
    }).input();
    form.append($("<div class='acceptbutton' />").append(newAccountButton));
    form.append("<div class='clearfix' />");



    return container;
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.hasSigned() || this.model.document().isWhiteLabeled()) {
      console.log("not rendering save view");
      return this;
    }

    var container = $("<div class='save' />");

    if (this.model.saving()) {
      container.addClass("saving");
    } else if (this.model.saved()) {
      container.append($("<div class='headline'/>").text(localization.docsignview.createdAccountTitle));
      container.append($("<div class='subheadline'/>").text(localization.docsignview.createdAccountSubtitle));
      container.addClass('done');
    } else {
      container.append(this.createNewAccountElems());
    }

    $(this.el).append(container);
    return this;
  }
});


window.DocumentShareAfterSignView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  createFacebookLikeBox: function() {
    return $('<iframe src="//www.facebook.com/plugins/likebox.php?href=http%3A%2F%2Fwww.facebook.com%2Fpages%2FScrive%2F237391196280189&amp;width=292&amp;height=62&amp;colorscheme=light&amp;show_faces=false&amp;border_color&amp;stream=false&amp;header=false" scrolling="no" frameborder="0" style="border:none; overflow:hidden; width:292px; height:62px;" allowTransparency="true"></iframe>');
  },
  createFacebookLikeElems: function() {
    var container = $("<div />");

    var button = $("<div class='facebook btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.facebookButtonLabel));
    container.append(button);

    var dropdown = $("<div class='facebook dropdown'/>");
    dropdown.append($("<div class='content' />").append(this.createFacebookLikeBox()));
    dropdown.hide();
    container.append(dropdown);

    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  createTweetLink: function() {
    var container = $("<a />");
    container.attr("href", "https://twitter.com/intent/tweet?screen_name=scrive");
    var twitterscript = $("<script type='text/javascript' />");
    twitterscript.attr("src", "//platform.twitter.com/widgets.js");
    console.log("adding twitter script to head");
    $("head").append(twitterscript);
    return container;
  },
  createTweetThisElems: function() {
    var container = this.createTweetLink();
    var button = $("<div class='twitter btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.tweetButtonLabel));
    container.append(button);
    return container;
  },
  createPhoneMeElems: function() {
    var model = this.model;

    var container = $("<div />");

    var button = $("<div class='phone btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.phoneButtonLabel));
    container.append(button);

    var loading = $("<div class='loading' />");
    loading.hide();

    var form = $("<div />");
    form.append($("<div />").text(localization.docsignview.phoneFormDescription));
    var numberinput = $("<input type='text' />");
    form.append(numberinput);
    var submitForm = function() {
      var phone = numberinput.val();
      if (phone.trim().length == 0) {
        return;
      }
      (new Submit({
        url: model.phoneurl,
        method: "POST",
        email: model.email(),
        phone: phone,
        ajax: true,
        onSend: function() {
          console.log("requesting phone call");
          form.hide();
          loading.show();
        },
        ajaxerror: function(d, a) {
          console.error("failed to request a phone call");
          loading.hide();
          form.show();
        },
        ajaxsuccess: function(d) {
          console.log("successfully requested a phone call");
          loading.hide();
          form.empty();
          form.text(localization.docsignview.phoneConfirmationText);
          form.show();
        }
      })).send();
    };

    form.append(Button.init({
      color: "green",
      size: "tiny",
      text: localization.docsignview.phoneSubmitButtonLabel,
      onClick: submitForm
    }).input());

    numberinput.keypress(function(e) {
      if (e.which == 13) {
        submitForm();
      }
    });

    form.append($("<div class='clearfix' />"));

    var content = $("<div class='content' />");
    content.append(loading);
    content.append(form);

    var dropdown = $("<div class='phone dropdown'/>");
    dropdown.append(content);
    dropdown.hide();
    container.append(dropdown);

    button.mouseover(function() {
      dropdown.addClass("over");
    });

    button.mouseleave(function() {
      dropdown.removeClass("over");
    });

    button.click(function() {
      dropdown.toggle();
    });
    return container;
  },
  createStartLink: function() {
    var container = $("<a />");
    container.attr("href", "/upload");
    return container;
  },
  createGetStartedElems: function() {
    var container = this.createStartLink();
    var button = $("<div class='start btn' />");
    button.append($("<div class='label' />").text(localization.docsignview.startButtonLabel));
    container.append(button);
    return container;
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.hasSigned() || !this.model.saved() || this.model.document().isWhiteLabeled() || this.model.document().currentSignatory().hasUser()) {
      return this;
    }

    var container = $("<div class='share' />");

    container.append($("<div class='title'/>").text(localization.docsignview.shareTitle));
    var panel = $("<div class='panel' />");
    panel.append($("<div class='item' />").append(this.createFacebookLikeElems()));
    panel.append($("<div class='item' />").append(this.createTweetThisElems()));
    panel.append($("<div class='item' />").append(this.createPhoneMeElems()));
    panel.append($("<div class='item' />").append(this.createGetStartedElems()));
    panel.append($("<div class='clearfix' />"));
    container.append(panel);

    $(this.el).append(container);
    return this;
  }
});
})(window);
