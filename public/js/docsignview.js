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

window.DocumentSignView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        var view = this;
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        //kind of icky.  this is required because the of the timeouts in FieldPlacement
        this.model.bind('file:change', function() { window.setTimeout(view.render, 500); });
        this.model.view = this;
        this.saveAfterSignModel = new DocumentSaveAfterSignModel({
          document: this.model
        });
        this.prerender();
        this.render();
    },
    prerender: function() {
      this.container = $("<div class='mainContainer signview' />");
      $(this.el).append(this.container);
      $(this.el).addClass("body-container");
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");
      return this;
    },
    createSignInstructionElems: function() {
      return $(new DocumentSignInstructionsView({
        model: this.saveAfterSignModel,
        el: $("<div />")
      }).el);
    },
    createSaveAfterSignViewElems: function() {
      if (this.model.padAuthorization()) return $("<div/>"); 
      return $(new DocumentSaveAfterSignView({
       model: this.saveAfterSignModel,
       el: $("<div />")
      }).el);
    },
    createShareAfterSignViewElems: function() {
      if (this.model.padAuthorization()) return $("<div/>");
      return $(new DocumentShareAfterSignView({
        model: this.saveAfterSignModel,
        el: $("<div />")
      }).el);
    },
    getOrCreateMainFileView: function() {
      if (this.mainfileview == undefined) {
        var file = KontraFile.init({
          file: this.model.mainfile(),
          document: this.model
        });
        this.mainfileview = file.view;
      }
      return this.mainfileview;
    },
    getRenderedPlacements: function() {
      var renderedPlacements = [];
      _.each(this.getOrCreateMainFileView().pageviews, function(pageview) {
        _.each(pageview.renderedPlacements, function(placement) {
          renderedPlacements.push(placement);
        });
      });
      renderedPlacements = renderedPlacements.sort(function(a, b) {
        var pdiff = a.placement.page() - b.placement.page();
        if (pdiff!=0) {
          return pdiff;
        }
        //put a fudge into the y, so it doesnt matter if people don't line things up
        var ydiff = a.placement.y() - b.placement.y();
        if (ydiff>10 || ydiff<(-10)) {
          return ydiff;
        }
        return a.placement.x() - b.placement.x();
      });
      return renderedPlacements;
    },
    authorAttachmentsTitle: function() {
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.authorattachments().length > 1) {
        return localization.docsignview.authorAttachmentsTitleForLots;
      } else {
        return localization.docsignview.authorAttachmentsTitleForOne;
      }
    },
    unPlacedFieldTasks: function(fieldels) {
      var allfields = this.model.currentSignatory().customFields();
      //calling .filter will fail in IE7.  so have to do this instead
      var fields = [];
      _.each(allfields, function(field) {
        if (!field.isPlaced()) {
          fields.push(field);
        }
      });
      if (fields.length != fieldels.length) {
        console.error("expected to find an element per custom field");
        console.log("****info*****");
        console.log("fields");
        console.log(fields);
        console.log("fieldels");
        console.log(fieldels);
        console.log("*****");
        return;
      }
      var tasks = [];
      for (var i = 0; i < fields.length; i++) {
        tasks.push(this.unPlacedFieldTask(fields[i], fieldels[i]));
      }
      return tasks;
    },
    unPlacedFieldTask: function(field, el) {
      /**
       * this stuff is kind of disgusting
       * the idea is to delay completing a field for
       * a little while to give the person a change
       * to type it all in before moving the arrow
       */
      var completiontime = undefined;
      var lastvalue = undefined;
      var queueChange = function(update) {
        console.log("Delaying field completion...");
        window.setTimeout(function() {
          update();
        }, 500);
      };
      return new DocumentSignViewTask({
        model: field,
        isComplete: function() {
          var newvalue = field.value() != "";
          var returnvalue = lastvalue;
          if (lastvalue == undefined ||
                !newvalue ||
                (lastvalue && newvalue)) {
            returnvalue = newvalue;
          } else if (!lastvalue &&
                       newvalue &&
                       completiontime == undefined) {
            completiontime = new Date();
            queueChange(this.update);
          } else if (!lastvalue &&
                       newvalue &&
                       completiontime != undefined) {
            var elapsedtime = (new Date()).getTime() - completiontime.getTime();
            if (elapsedtime >= 2000) {
              completiontime = undefined;
              returnvalue = newvalue;
            } else {
              queueChange(this.update);
            }
          }

          lastvalue = returnvalue;
          return returnvalue;
        },
        el: el
      });
    },
    createAuthorAttachmentsElems: function() {
      return $(new DocumentAuthorAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing'/>"),
        title: this.authorAttachmentsTitle()
      }).el);
    },
    signatoryAttachmentsTitle: function() {
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.currentSignatory().attachments().length > 1) {
        return localization.docsignview.signatoryAttachmentsTitleForLots;
      } else {
        return localization.docsignview.signatoryAttachmentsTitleForOne;
      }
    },
    createSignatoryAttachmentsView: function() {
      return new DocumentSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing'/>"),
        title: this.signatoryAttachmentsTitle()
      });
    },
    signatoryAttachmentTasks: function(attachmentels) {
      var attachments = this.model.currentSignatory().attachments();
      if (attachments.length != attachmentels.length) {
        console.error("expected to find an element per attachment");
        return;
      }
      var tasks = [];
      for (var i = 0; i < attachments.length; i++)
          tasks.push(this.signatoryAttachmentTask(attachments[i], attachmentels[i]));
      return tasks;
    },
    signatoryAttachmentTask: function(attachment, el) {
      return new DocumentSignViewTask({
        model: attachment,
        isComplete: function() {
          return attachment.hasFile() && attachment.isReviewed();
        },
        el: el
      });
    },
    createInlineFieldTask: function(renderedPlacement) {
      var placement = renderedPlacement.placement;
      var elem = renderedPlacement.elem;
      return new DocumentSignViewTask({
        model: placement.field(),
        isComplete: function() {
          return placement.field().readyForSign();
        },
        el: elem,
        beforePointing: function() {
          elem.trigger("click");
        },
        label: placement.field().isSignature() ? "" : placement.field().nicename()
      });
    },
    createUploadedAttachmentsElems: function() {
      return $(new DocumentUploadedSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing' />"),
        title: localization.docsignview.uploadedAttachmentsTitle
      }).el);
    },
    createSignatoriesView: function(triggerArrowChange) {
      return new DocumentSignSignatoriesView({
          // I apologize for putting this model in the view, but 
          // the model/view split was already messed up when I got
          // here. --Eric
        model: new DocumentSignSignatoryBox({document:this.model}),
        el: $("<div class='section signatories spacing'/>")
      });
    },
    createRejectButtonElems: function() {
      var document = this.model;
      var signatory = document.currentSignatory();
      return $("<div class='rejectwrapper'>").append(Button.init({
        size: "big",
        color: "red",
        text: document.process().rejectbuttontext(),
        onClick: function() {
          ConfirmationWithEmail.popup({
            title: document.process().signatorycancelmodaltitle(),
            mail: signatory.rejectMail(),
            acceptText: localization.reject.send,
            editText: localization.reject.editMessage,
            rejectText: localization.cancel,
            acceptColor: "red",
            onAccept: function(customtext) {
              signatory.reject(customtext).send();
            }
          });
        }
      }).input());
    },
    createSignButtonElems: function(othertasks) {
      return $(new DocumentSignButtonView({
        model: this.model,
        validate: function() {
          var complete = true;
          _.each(othertasks, function(task) {
            if (!task.complete()) {
              complete = false;
            }
          });
          return complete;
        },
        el: $("<div class='signwrapper'/>")
      }).el);
    },
    signButtonTask: function(el) {
      var document = this.model;
      return new DocumentSignViewTask({
        model: document,
        isComplete: function() {
          return !document.currentSignatoryCanSign();
        },
        el: el
      });
    },
    createArrowsElems: function(tasks) {
      var model = new DocumentSignViewTasks({
        tasks: tasks
      });
      return $(new DocumentSignViewArrowView({
        model: model,
        mainview : this,
        el: $("<div />")
      }).el);
    },
    isDisplaySignatories: function() {
      return !this.model.closed();
    },
    isBottomStuff: function() {
      return this.model.isAuthorAttachments() ||
               this.model.isSignatoryAttachments() ||
               this.model.isUploadedAttachments() ||
               this.isDisplaySignatories();
    },
    render: function() {
      var view = this;
      var document = this.model;
      if (!document.ready() || document.mainfile()==undefined) {
          this.mainfileview = undefined;
          return this;
      }

      var mainfileelems = $(this.getOrCreateMainFileView().el);
      mainfileelems.detach();

      this.container.empty();

      this.container.append(this.createSignInstructionElems());
      if (document.currentSignatory().hasSigned()) {
        if (!document.currentSignatory().saved() || view.saveAfterSignModel.justSaved()) {
          this.container.append(this.createSaveAfterSignViewElems());
        }
        this.container.append(this.createShareAfterSignViewElems());
      }

      if(view.saveAfterSignModel.justSaved() && !document.isWhiteLabeled()) {
          var sbox = $('<div class="sbox" />');
          var video = $('<div class="video" />');
          sbox.append(video);
          video.append('<iframe src="https://player.vimeo.com/video/41846881" width="620" height="330" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>');
          this.container.find(".share").append(sbox);
          this.container.addClass("just-signed");
      } else {
          var subcontainer = $("<div class='subcontainer'/>");
          
          var mainfileelems = $(this.getOrCreateMainFileView().el);
          subcontainer.append(mainfileelems);
          
          if (!document.mainfile().ready()) {
              this.container.append(subcontainer);
              return this;
          }
          
          mainfileelems.css("min-height", "1352px");
          
          var tasks = [];
          
          var triggerTask = undefined;
          var triggerArrowChange = function() {
              if (triggerTask != undefined) {
                  triggerTask.trigger("change");
              }
          };

      _.each(this.getRenderedPlacements(), function(renderedPlacement) {
        //the signatory only needs to fill in their own tasks
        if (renderedPlacement.placement.field().signatory().current()) {
          tasks.push(view.createInlineFieldTask(renderedPlacement));
        }
      });

      if (this.isBottomStuff()) {
        var bottomstuff = $("<div class='bottomstuff' />");

        if (this.model.isAuthorAttachments()) {
          bottomstuff.append(this.createAuthorAttachmentsElems());
        }

        if (this.model.isSignatoryAttachments()) {
          var attachmentsview = this.createSignatoryAttachmentsView();
          if (this.model.currentSignatoryCanSign())
            _.each(this.signatoryAttachmentTasks(attachmentsview.uploadElems), function(task) { tasks.push(task); });
          bottomstuff.append($(attachmentsview.el));
        }

        if (this.model.isUploadedAttachments()) {
          bottomstuff.append(this.createUploadedAttachmentsElems());
        }

        if (this.isDisplaySignatories()) {
          //triggerArrowChange will cause the arrow to repaint if the signatories
          //are expanded or compressed, because this means any sign arrow may need to move
          var signatoriesview = this.createSignatoriesView(triggerArrowChange);
          if (this.model.currentSignatoryCanSign()) {
            //_.each(this.unPlacedFieldTasks(signatoriesview.customfieldelems), function(task) {
            //  tasks.push(task);
            //});
          }
          bottomstuff.append($(signatoriesview.el));
        }

        if (this.model.currentSignatoryCanSign() && (!this.model.currentSignatory().canPadSignQuickSign())) {
          var signsection = $("<div class='section spacing signbuttons' />");
          signsection.append(this.createRejectButtonElems());
          var signButton = this.createSignButtonElems(jQuery.extend({}, tasks));
          var signButtonTask = this.signButtonTask(signButton);
          triggerTask = signButtonTask;
          tasks.push(signButtonTask);
          signsection.append(signButton);
          signsection.append($("<div class='clearfix' />"));
          bottomstuff.append(signsection);
        }

        subcontainer.append(bottomstuff);

        subcontainer.append($("<div class='cleafix' />"));
      }
      this.container.append(subcontainer);
      }
      if ((this.model.signingInProcess() &&
           !this.model.currentSignatory().hasSigned()) ||
             !this.model.currentSignatory().signs()) {
        this.container.prepend(this.createArrowsElems(tasks));
      }

      return this;
    }
});

window.DocumentSignViewTask = Backbone.Model.extend({
  defaults: {
    complete: false
  },
  initialize: function(args) {
    _.bindAll(this, 'update');
    this.model = args.model;
    this.model.bind('reset', this.update);
    this.model.bind('change', this.update);
    this.update();
  },
  isComplete: function() {
    var f = this.get("isComplete");
    if (f==undefined) {
      return undefined;
    }
    return f();
  },
  el: function() {
    return this.get("el");
  },
  label: function() {
    return this.get("label");
  },
  beforePointing: function() {
    var f = this.get("beforePointing");
    if (f==undefined) {
      return undefined;
    }
    return f();
  },
  update: function() {
    var complete = this.isComplete();
    this.set({ complete: complete });
  },
  complete: function() {
    return this.get("complete");
  },
  setComplete: function(complete) {
    if (complete != this.complete) {
      this.set({ complete: complete });
    }
  }
});

window.DocumentSignViewTasks = Backbone.Model.extend({
  defaults: {
    tasks: []
  },
  tasks: function() {
    return this.get("tasks");
  },
  isIncompleteTask: function() {
    return this.nextIncompleteTask() != undefined;
  },
  nextIncompleteTask: function() {
      return _.find(this.tasks(), function(t) { return !t.complete(); });
  }
});

window.DocumentSignViewArrowView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    var render = this.render;
    _.each(this.model.tasks(), function(task) {
      task.bind("change", render);
    });
    this.mainview = args.mainview;
    this.render();
  },
  render: function() {
    var view = this;
    var document = this.document;
    console.log("rendering arrows");
    $(this.el).empty();

    var taskmodel = this.model;

    var container = $("<div class='arrows' />");

    var downarrow = $("<div class='down arrow' style='display:none'/>");
    container.append(downarrow);

    var uparrow = $("<div class='up arrow' style='display:none'/>");
    container.append(uparrow);

    if (taskmodel.isIncompleteTask()) {
      downarrow.css("cursor", "pointer");
      downarrow.click(function() {
        var el = taskmodel.nextIncompleteTask().el();
        var scrollbottom = el.offset().top + el.height() + 100;
        $('html,body').animate({
          scrollTop: scrollbottom - $(window).height()
        }, 2000);
      });
      uparrow.css("cursor", "pointer");
      uparrow.click(function() {
        var el = taskmodel.nextIncompleteTask().el();
        $('html,body').animate({
          scrollTop: el.offset().top - 100
        }, 1000);
      });
    }

    var actionarrow = $("<div class='action arrow'/>");
    var setActionArrowText = function(txt) {
      actionarrow.empty();
      var front = $("<div class='front' />");
      if (txt!=undefined) {
        front.append($("<div class='label' />").text(txt));
      }
      actionarrow.append(front);
      actionarrow.append($("<div class='back' />"));
      actionarrow.append($("<div class='clearfix' />"));
    };
    container.append(actionarrow);

    var updateRightMargin = function() {
      var space = $(window).width() - 941;
      var margin = 0;
      var bigarrowmargin = 0;
      if (space > 0) {
        margin = space / 2;
        bigarrowmargin = (space + 941 - 112) / 2;
      } else {
        bigarrowmargin = (941 - 112) / 2;
      }
      // I'm keeping this in case we want to revert it. -- Eric
      //      downarrow.css("right", margin + "px");
      downarrow.css("right", bigarrowmargin + "px");
      uparrow.css("right", bigarrowmargin + "px");
    };
    $(window).resize(updateRightMargin);
    updateRightMargin();

    var updateActionArrowPosition = function() {
      if (taskmodel.isIncompleteTask()) {
        var el = taskmodel.nextIncompleteTask().el();
        actionarrow.css("top", (el.offset().top + (el.height() / 2) - 14) + "px");
        actionarrow.css("left", (el.offset().left + el.width() + 10) + "px");
      }
    };
    updateActionArrowPosition();

    var checkIfDownArrowInFooter = function() {
      if ($(".pagefooter").size() == 0) return;
      var footertop = $(".pagefooter").offset().top;
      var downarrowbottom = downarrow.offset().top + downarrow.height();
      if (downarrowbottom + 100 > footertop) {
        downarrow.addClass("infooter");
      } else {
        downarrow.removeClass("infooter");
      }
    };
    $(window).resize(checkIfDownArrowInFooter);
    $(window).scroll(checkIfDownArrowInFooter);
    checkIfDownArrowInFooter();

      var scrollpoint = 0;
    var updateVisibility = function() {

      if (!taskmodel.isIncompleteTask()) {
        downarrow.show();
        uparrow.hide();
        actionarrow.hide();
        view.mainview.trigger("change:task");
      } else {
        var scrolltop = $(window).scrollTop();
        var scrollbottom = scrolltop + $(window).height();
        var eltop = taskmodel.nextIncompleteTask().el().offset().top;
        var elbottom = eltop + taskmodel.nextIncompleteTask().el().height();

        var bottommargin = 0;
        var topmargin = 0;

        if ((scrolltop >= 0) && (elbottom <= eltop)) {
          console.log("waiting to show arrow ....");
          window.setTimeout(updateVisibility, 500);
          view.pointingAt = undefined;
        } else if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop)) {
          var nextTask = taskmodel.nextIncompleteTask();
            $(".signview .section").removeClass("highlight");
          nextTask.el().parents(".signview .section").addClass("highlight");
          if (view.pointingAt==undefined || view.pointingAt!=nextTask) {
            nextTask.beforePointing();
          }
          setActionArrowText(nextTask.label());
          updateActionArrowPosition();
          actionarrow.show();
          uparrow.hide();
          downarrow.hide();
          view.pointingAt = nextTask;
          view.mainview.trigger("change:task");
        } else if ((elbottom + bottommargin) > scrollbottom) {
            if(scrollpoint !== 16) {
                scrollpoint = 6;
                console.log("6");
                downarrow.show();
                uparrow.hide();
                actionarrow.hide();
                view.pointingAt = undefined;
                view.mainview.trigger("change:task");
            }
        } else {
            if(scrollpoint !== 17) {
                scrollpoint = 7;
                console.log("7");
                uparrow.show();
                downarrow.hide();
                actionarrow.hide();
                view.pointingAt = undefined;
                view.mainview.trigger("change:task");
            }
        }
      }
    };
    $(window).resize(updateVisibility);
    $(window).scroll(updateVisibility);
    updateVisibility();

    $(this.el).append(container);
  }
});

window.KontraSignDocument = {
  init: function(args) {
    this.model = new Document({
                    id: args.id,
                    viewer: args.viewer
                });
    this.view = new DocumentSignView({
                    model: this.model,
                    el: $("<div/>")
                });
    this.recall();
    return this;
  },
  recall: function() {
    this.model.recall();
  }
};
})(window);
