/* Signatory view of document
 */


(function(window) {

window.DocumentSignInstructionsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  isSigning: function() {
    var signatory = this.model.document.currentSignatory();
    return this.model.document.signingInProcess() && signatory.signs() && !signatory.hasSigned();
  },
  isReviewing: function() {
    var signatory = this.model.document.currentSignatory();
    return (this.model.document.signingInProcess() || this.model.document.closed()) && !signatory.signs();
  },
  isSignedNotClosed: function() {
    var signatory = this.model.document.currentSignatory();
    return this.model.document.signingInProcess() && signatory.hasSigned() && !this.model.document.closed();
  },
  isSignedAndClosed: function() {
    var signatory = this.model.document.currentSignatory();
    return signatory.hasSigned() && this.model.document.closed();
  },
  isUnavailableForSign: function() {
    return !this.model.document.signingInProcess() && !this.model.document.closed();
  },
  text: function() {
    if (this.isSigning()) {
      return localization.docsignview.followArrowToSign;
    } else if (this.isReviewing()) {
      return localization.docsignview.followArrowToReview;
    } else if (this.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosed;
    } else if (this.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosed;
    } else if (this.isUnavailableForSign()) {
      return localization.docsignview.unavailableForSign;
    } else {
      console.error("Unsure what state we're in");
      return localization.docsignview.unavailableForSign;
    }
  },
  subtext: function() {
    if (this.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosedSubText;
    } else if (this.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosedSubText;
    } else {
      return "";
    }
  },
  createMenuElems: function() {
    if (this.model.document.padAuthorization()) return $("<div>");  
    return $(new DocumentActionMenuView({
      model: this.model.document,
      el: $("<div class='menuwrapper'/>")
    }).el);
  },
  render: function() {
    $(this.el).empty();

    if(this.model.justSaved())
      return this;

    var container = $("<div class='instructions' />");
    container.append($("<div class='headline' />").text(this.text()));
    container.append($("<div class='subheadline' />").text(this.subtext()));
    if (this.model.document.padAuthorization() && this.isSignedNotClosed() && BrowserInfo.isPadDevice())
    {    var padGiveToNextSignatoryModel = new PadGiveToNextSignatoryModel({document : this.model.document});
         container.append(new PadGiveToNextSignatoryView({model : padGiveToNextSignatoryModel}).el);
    }
    var smallerbit = $("<div />");
    var timeout = this.model.document.timeouttime();
    if (timeout != undefined && this.model.document.signingInProcess()) {
      smallerbit.append($("<div class='duedate' />").text(localization.docsignview.dueDate + " " + timeout.getFullYear() + "-" + timeout.getMonth() + "-" + timeout.getDate()));
    }
    smallerbit.append(this.createMenuElems());
    container.append($("<div class='subheadline' />").append(smallerbit));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentSignSignatoryBox = Backbone.Model.extend({
    defaults: {
        index: 0
    },
    initialize: function(args) {
        var me = this;
        this.document().bind('change', function(){ me.trigger('change:document'); });
    },
    currentSignatoryIndex: function() {
        return this.get('index');
    },
    setSignatoryIndex: function(i) {
        this.set({index:i});
        return this;
    },
    document: function() {
        return this.get('document');
    },
    signatories: function() {
        var signatories = this.document().signatories();
        var current = _.find  (signatories, function(s) { return  s.current(); });
        var others  = _.filter(signatories, function(s) { return !s.current(); });
        return _.filter([current].concat(others), function(s) { return s.signs(); });
    },
    currentSignatory: function() {
        return this.signatories()[this.currentSignatoryIndex()];
    }
});

window.DocumentSignSignatoriesView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  signatorySummary: function(signatory) {
      var document = signatory.document();
      if (signatory.signdate() != undefined)
        return localization.signatoryMessage.signed;
      else if (signatory.datamismatch() == true ||
               document.timedout() ||
               document.canceled() ||
               document.datamismatch())
          return localization.docsignview.unavailableForSign;
      else if (signatory.rejecteddate() != undefined)
          return localization.signatoryMessage.rejected;
      else if (signatory.status() == 'opened')
          return localization.signatoryMessage.seen;
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.other;
      else
          return localization.signatoryMessage[signatory.status()];
  },
  siglist: function(signatories) {
      var sigbox = this.model;
      var list = $("<div class='list spacing' />");
      _.each(signatories, function(signatory, index) {
          var sigdiv     = $("<div class='sig' />");
          if(index === 0)
              sigdiv.addClass('first');
          var name       = $("<div class='name' />").text(signatory.name);
          var line       = $("<div class='line' />");
          var middle1    = $("<div class='middle' />");
          var middle2    = $("<div class='middle' />");
          var middle3    = $("<div class='middle' />");
          var statusicon = $("<div class='icon status' />").addClass(signatory.statusicon);
          var status     = $("<span class='statustext' />").addClass(signatory.statusicon).text(signatory.status);
          var details    = $('<a class="details" href="#" />').text(localization.docsignview.showDetails);

          middle1.append(statusicon);
          middle2.append(status);
          middle3.append(details);
          line.append(middle1).append(middle2).append(middle3);
          details.click(function() {
              sigbox.setSignatoryIndex(index);
              return false;
          });

          sigdiv.append(name).append(line);
          list.append(sigdiv);
      });
      return list;
  },
  statusbox: function(signatory) {
      var statusbox  = $('<div  class="statusbox" />');
      var space = $('<div class="spacing butt" />');
      var statusicon = $("<span class='icon status' />").addClass(signatory.status());
      var status     = $("<span class='status statustext' />").text(this.signatorySummary(signatory)).addClass(signatory.status());
      space.append(statusicon).append(status).addClass(signatory.status());
      statusbox.append(space);
      return statusbox;
  },
  sigbox: function(signatory) {
      var box     = $('<div class="sigbox" />');

      var titleinfo = $('<div class="titleinfo spacing" />');
      var name      = $('<div class="name" />').text(signatory.name());
      var company   = $('<div class="company" />').text(signatory.company());
      titleinfo.append(name).append(company);
      box.append(titleinfo);

      var inner   = $('<div class="inner spacing" />');

      var face    = $('<div class="face" />');
      
      var numspace = $('<div class="spacing numspace" />');
      var orgnum  = $('<div class="orgnum field" />').text(localization.docsignview.companyNumberLabel + ": " 
                                                           + (signatory.companynumber().trim() || localization.docsignview.notEntered))
          .attr('title', signatory.companynumber());
      var persnum = $('<div class="persnum field" />').text(localization.docsignview.personalNumberLabel + ": " 
                                                            + (signatory.personalnumber().trim() || localization.docsignview.notEntered))
        .attr('title', signatory.personalnumber());
      var contactspace = $('<div class="spacing contactspace" />');
      var email   = $('<div class="email field" />').text(signatory.email()).attr('title', signatory.email());

      numspace.append(orgnum);
      numspace.append(persnum);

      numspace.append(email);

      inner.append(face);

      inner.append(numspace);
      inner.append(contactspace);
      box.append(inner);

      box.append(this.statusbox(signatory));

      return box;
  },
  render: function() {
      var view = this;
      var $el = $(this.el);
      $el.empty();

      $el.append($("<h2 />").text(localization.docsignview.signatoriesTitle));
      var box1 = $('<div class="column spacing" />');
      var box2 = $('<div class="column spacing" />');

      $el.append(box1).append(box2);

      var sigbox = view.model;

      var signatories = sigbox.signatories();

      if(signatories.length > 2) {
          var signatories_min = _.map(signatories, function(s) { return {name:s.nameOrEmail(),statusicon:s.status(),status:view.signatorySummary(s)} });
          box1.append(view.siglist(signatories_min));
          box2.append(view.sigbox(sigbox.currentSignatory()));
      } else if (signatories.length === 2) {
          box1.append(view.sigbox(signatories[0]));
          box2.append(view.sigbox(signatories[1]));
      } else if (signatories.length === 1) {
          box1.css("border-color","#ffffff");
          box2.append(view.sigbox(signatories[0]));
      }
      return this;
  }
});

window.DocumentSaveAfterSignModel = Backbone.Model.extend({
  defaults: {
    saved: false,
    saving: false,
    justsaved: false
  },
  initialize: function(args) {
    this.document = args.document;
  },
  email: function() {
    return this.document.currentSignatory().email();
  },
  hasSigned: function() {
    return this.document.currentSignatory().hasSigned();
  },
  justSaved: function() {
    return this.get('justsaved');
  },
  saved: function() {
    return this.document.currentSignatory().saved() || this.get("saved");
  },
  saving: function() {
    return this.get("saving");
  },
  setSaving: function(saving) {
    this.set({ saving: saving });
  },
  setSaved: function() {
    this.set({ saved: true,
               saving: false,
               justsaved: true});
    this.document.trigger('change');
  },
  saveurl: function() {
    return this.document.currentSignatory().saveurl();
  },
  phoneurl: function() {
    return "/account/phoneme";
  }
});

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
               model.document.currentSignatory());
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

    if (!this.model.hasSigned() || this.model.document.isWhiteLabeled()) {
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

    if (!this.model.hasSigned() || !this.model.saved() || this.model.document.isWhiteLabeled() || this.model.document.currentSignatory().hasUser()) {
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
