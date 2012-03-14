/* Signatory view of document
 */


(function(window){

window.DocumentSignInstructionsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  isSigning: function() {
    var signatory = this.model.currentSignatory();
    return this.model.signingInProcess() && signatory.signs() && !signatory.hasSigned();
  },
  isReviewing: function() {
    var signatory = this.model.currentSignatory();
    return (this.model.signingInProcess() || this.model.closed()) && !signatory.signs();
  },
  isSignedNotClosed: function() {
    var signatory = this.model.currentSignatory();
    return this.model.signingInProcess() && signatory.hasSigned() && !this.model.closed();
  },
  isSignedAndClosed: function() {
    var signatory = this.model.currentSignatory();
    return signatory.hasSigned() && this.model.closed();
  },
  isUnavailableForSign: function() {
    var signatory = this.model.currentSignatory();
    return !this.model.signingInProcess() && !this.model.closed();
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
    return new DocumentActionMenuView({
      model: this.model,
      el: $("<div class='menuwrapper'/>")
    }).el;
  },
  render: function() {
    this.el.empty();

    var container = $("<div class='instructions' />");
    container.append($("<div class='headline' />").append(this.text()));
    container.append($("<div class='subheadline' />").append(this.subtext()));

    var smallerbit = $("<div class='subheadline' />");
    var timeout = this.model.timeouttime();
    if (timeout!=undefined && this.model.signingInProcess()) {
      smallerbit.append($("<div class='duedate' />").append(localization.docsignview.dueDate + " " + timeout.getFullYear() + "-" + timeout.getMonth() + "-" + timeout.getDate()));
    }
    smallerbit.append(this.createMenuElems());
    smallerbit.append($("<div class='clearfix' />"));
    container.append(smallerbit);

    this.el.append(container);

    return this;
  }
});

window.DocumentSignSignatoryView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    if (args.summary) {
      this.summary = args.summary;
    } else {
      this.summary = new SignatoryStandardSummary({
        model: this.model
      });
    }
    this.render();
  },
  //TODO: neaten up this junk
  signatorySummary : function(){
    var signatory = this.model;
    var document = signatory.document();
    if (signatory.signdate() != undefined)
          return this.summary.signed();
    else if (signatory.datamismatch() == true)
          return this.summary.datamismatch()
    else if (document.timedout())
          return this.summary.timedout();
    else if(document.canceled())
          return this.summary.cancelled();
    else if (document.datamismatch())
          return " "
    else if (signatory.rejecteddate()!= undefined)
          return this.summary.rejected();
    else if (signatory.seendate()!= undefined)
          return this.summary.seen();
    else if (signatory.readdate()!= undefined)
          return this.summary.read();
    else if (signatory.deliveredEmail())
          return this.summary.delivered();
    else
        return this.summary.other();
  },
  isInputField: function(field) {
    return (field.name() != "fstname" &&
              field.name() != "sndname" &&
              field.name() != "email" &&
              field.name() != "sigco" &&
              field.name() != "sigpersnr" &&
              field.name() != "sigcompnr" &&
              field.name() != "signature");
  },
  createCompactInfoElements: function() {
    var view = this;

    var container = $("<div />");
    container.append($("<div class='grouping' />").append($("<div class='name' />").append(this.model.name())));
    var expandlink = $("<a />");
    expandlink.click(function() {
      view.expanded = true;
      view.render();
    });
    expandlink.text(localization.docsignview.showDetails);
    container.append($("<div class='grouping' />").append(expandlink));

    return container;
  },
  createDetailedInfoElements: function() {
    var signatory = this.model;
    var view = this;

    var container = $("<div />");

    var topbit = $("<div class='grouping' />");
    topbit.append($("<div class='name' />").append(signatory.name()));
    topbit.append($("<div class='company' />").append(signatory.company()));
    container.append(topbit);

    if (signatory.companynumber()!="" || signatory.personalnumber()!="") {
      var middlebit = $("<div class='grouping' />");
      if (signatory.companynumber()!="") {
        middlebit.append($("<div class='companynumber'/>").append(localization.docsignview.companyNumberLabel + ": " + signatory.companynumber()));
      }
      if (signatory.personalnumber()!="") {
        middlebit.append($("<div class='personalnumber'/>").append(localization.docsignview.personalNumberLabel + ": " + signatory.personalnumber()));
      }
      container.append(middlebit);
    }

    var bottombit = $("<div class='grouping' />");
    bottombit.append($("<div class='email' />").append(signatory.email()));
    container.append(bottombit);

    var inputbits = $("<div class='grouping' />");
    var isinputs = false;
    var isInputField = this.isInputField;
    _.each(signatory.fields(), function(field) {
      if (isInputField(field)) {
        /**don't show other people's custom fields that
        still need filling**/
        if (field.value()=="" && !signatory.current()) {
          return;
        }
        isinputs = true;
        var fieldwrapper = $("<div class='fieldwrapper'/>");
        if (signatory.current()) {
          fieldwrapper.append($("<div class='label' />").append(field.nicename() + ":"));
        }
        fieldwrapper.append(new FieldStandardView({
          model: field,
          el: $("<div class='field' />")
        }).el);
        inputbits.append(fieldwrapper);
      }
    });
    if (isinputs) {
      container.append(inputbits);
    }

    if (!signatory.current()) {
      var compactlink = $("<a />");
      compactlink.click(function() {
        view.expanded = false;
        view.render();
      });
      compactlink.text(localization.docsignview.hideDetails);
      container.append($("<div class='grouping' />").append(compactlink));
    }

    return container;
  },
  createSummaryElements: function() {
    var signatory = this.model;

    var container = $("<div class='summary' />");
    if (signatory.signs()) {
      container.addClass(signatory.status());
      if (signatory.status()=="signed") {
        container.append($("<div class='icon status signed' />"));
      }
      container.append($("<div class='label' />").text(this.signatorySummary()));
    } else {
      container.append($("<div class='label' />").text(signatory.document().process().authorissecretarytext()));
    }
    container.append($("<div class='clearfix' />"));
    return container;
  },
  render: function() {
    this.el.empty();

    var signatory = this.model;

    var container = $("<div class='signatory' />");
    if (signatory.current()) {
      container.addClass("current");
    } else {
      container.addClass("other");
    }

    if (!signatory.current() && !this.expanded) {
      container.append(this.createCompactInfoElements());
    } else {
      container.append(this.createDetailedInfoElements());
    }

    container.append(this.createSummaryElements());

    this.el.append(container);
    return this;
  }
});

window.DocumentSignSignatoriesView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  orderedOtherSignatories: function() {
    var document = this.model;
    var othersignatories = new Array();

    if (!document.currentSignatory().author()) {
      othersignatories.push(document.author());
    }

    var pushOthersWhereHasSignedIs = function(hassigned) {
      _.each(document.otherSignatories(), function(signatory) {
        if (!signatory.author() && signatory.signs() && signatory.hasSigned()==hassigned) {
          othersignatories.push(signatory);
        }
      });
    };

    pushOthersWhereHasSignedIs(true);
    pushOthersWhereHasSignedIs(false);

    return othersignatories;
  },
  createSignatoryElems: function(signatory) {
    return new DocumentSignSignatoryView({
      model: signatory,
      summary: new SignatoryReducedSummary({
        model: signatory
      }),
      el: $("<div />")
    }).el;
  },
  render: function() {
    this.el.empty();

    var container = $("<div class='signatories' />");
    container.append($("<h2 />").append(localization.docsignview.signatoriesTitle));

    var signatoriesview = this;
    var list = $("<div class='list' />");
    var otheritem = undefined;
    var row = undefined;
    _.each(this.orderedOtherSignatories(), function (signatory) {
      var item = $("<div class='column' />").append(signatoriesview.createSignatoryElems(signatory));
      if (row==undefined) {
        row = $("<div class='row' />").append(item);
        otheritem = item;
      } else {
        row.append(item);
        row.append($("<div class='clearfix' />"));
        list.append(row);
        row = undefined;
      }
    });

    var currentitem = $("<div class='column' />").append(signatoriesview.createSignatoryElems(this.model.currentSignatory()));
    if (row==undefined) {
      row = $("<div class='row' />").append($("<div class='column' />"));
    }
    row.append(currentitem);
    row.append($("<div class='clearfix' />"));
    list.append(row);

    list.append($("<div class='clearfix' />"));

    container.append(list);
    container.append($("<div class='clearfix' />"));

    this.el.append(container);

    return this;
  }
});

window.DocumentSaveAfterSignView = Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  clearValidationErrors: function(elem) {
    elem.removeClass("invalid");
    if (elem.parent()!=undefined) {
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
    elem.parent().append($("<div class='errormsg' />").append(msg));
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
          callback: callback
        , message: localization.validation.passwordLessThanMinLength
        , message_max: localization.validation.passwordExceedsMaxLength
        , message_digits: localization.validation.passwordNeedsLetterAndDigit
      });
    });
  },
  createPassword2ValidationFunction: function(passwordinput, password2input) {
    return this.createValidationFunction(password2input, password2input, function(callback) {
      return new PasswordValidation({
          callback: callback
        , message: localization.validation.passwordsDontMatch
        , "with": passwordinput
      });
    });
  },
  createTOSValidationFunction: function(checkbox, toswrapper) {
    return this.createValidationFunction(checkbox, toswrapper, function(callback) {
      return new CheckboxReqValidation({
          callback: callback
        , message: localization.validation.mustAcceptTOS
      });
    });
  },
  accountFromSignURL: function() {
    return "/s/" + this.model.id + "/" + this.model.currentSignatory().signatoryid() + "/" + this.model.viewer().magichash();
  },
  createNewAccountElems: function() {
    var container = $("<div class='newaccount'/>");
    container.append($("<div class='title' />").append(localization.docsignview.newAccountTitle));
    container.append($("<div class='subtitle' />").append(localization.docsignview.newAccountSubTitle));

    var form = $("<div class='form'/>");

    var appendFormInput = function(labeltext, input) {
      var row = $("<div class='item' />");
      row.append($("<div />").append($("<div class='label' />").append(labeltext)));
      row.append($("<div />").append(input));
      form.append(row);
    };

    appendFormInput(localization.docsignview.emailLabel, $("<div class='email'/>").append(this.model.currentSignatory().email()));

    var passwordinput = $("<input type='password' autocomplete='off' />");
    appendFormInput(localization.docsignview.passwordLabel, passwordinput);

    var password2input = $("<input type='password' autocomplete='off' />");
    appendFormInput(localization.docsignview.password2Label, password2input);

    form.append("<div class='clearfix' />");

    var tos = $("<div class='tos'/>");
    var checkbox =  $("<input type='checkbox'  id='tosCBox' autocomplete='off'/>");
    tos.append($("<div class='check'/>").append(checkbox));

    var toslabel = $("<label for='tosCBox'/>");
    tos.append($("<div class='label'/>").append(localization.docsignview.acceptTOSLabel));
    tos.append($("<div class='clearfix'/>"));
    form.append($("<div class='row'>").append(tos));

    var validatePassword = this.createPasswordValidationFunction(passwordinput);
    var validatePassword2 = this.createPassword2ValidationFunction(passwordinput, password2input);
    var validateTOS = this.createTOSValidationFunction(checkbox, tos);

    var view = this;
    var newAccountButton = Button.init({
      color: "green",
      text: localization.docsignview.newAccountButton,
      onClick: function() {

        var passwordvalid = validatePassword();
        var password2valid = validatePassword2();
        var tosvalid = validateTOS();

        if (passwordvalid &&
              password2valid &&
              tosvalid) {
         new Submit({
           url: view.accountFromSignURL(),
           method: "POST",
           acceptaccount: true,
           password: passwordinput.val(),
           password2: password2input.val(),
           tos: checkbox.val()
         }).send();
        }
      }
    }).input();
    form.append($("<div class='acceptbutton' />").append(newAccountButton));
    form.append("<div class='clearfix' />");

    container.append(form);

    return container;
  },
  createFindOutMoreElems: function() {
  },
  render: function() {
    this.el.empty();

    if (!this.model.currentSignatory().hasSigned()) {
      return this;
    }

    var container = $("<div class='save' />");

    if (this.model.currentSignatory().saved()) {
      container.addClass("done");
      container.append($("<div class='title'/>").append("Document Saved!"));
      container.append($("<div class='subtitle'/>").append("Your document is now securely saved with Scrive.  To retrieve in the future login with your email and password at www.scrive.com."));
    } else {
      container.append(this.createNewAccountElems());
    }

    this.el.append(container);
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
    button.append($("<div class='label' />").append("Like us on Facebook"));
    container.append(button);

    var dropdown = $("<div class='facebook dropdown'/>");
    dropdown.append(this.createFacebookLikeBox());
    dropdown.hide();
    container.append(dropdown);

    button.click(function() {
      console.log("facebook button clicked");
      dropdown.toggle();
    });
    return container;
  },
  createTweetLink: function() {
    var container = $("<a />");
    container.attr("href", "https://twitter.com/intent/tweet?screen_name=scrive");
    var f = function (d, s, id) {
      var js, fjs = d.getElementsByTagName(s)[0];
      if (!d.getElementById(id)){
        js = d.createElement(s);
        js.id = id;
        js.src = "//platform.twitter.com/widgets.js";
        fjs.parentNode.insertBefore(js, fjs);
      }
    }(document,"script","twitter-wjs");
    return container;
  },
  createTweetThisElems: function() {
    var container = this.createTweetLink();
    var button = $("<div class='twitter btn' />");
    button.append($("<div class='label' />").append("Tweet about Scrive"));
    container.append(button);
    return container;
  },
  createPhoneMeElems: function() {
    var container = $("<div class='phone btn' />");
    container.append($("<div class='label' />").append("Please call me"));
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
    button.append($("<div class='label' />").append("Just take me to the service!"));
    container.append(button);
    return container;
  },
  render: function() {
    this.el.empty();

    if (!this.model.currentSignatory().hasSigned()) {
      return this;
    }

    var container = $("<div class='share' />");

    container.append($("<div class='title'/>").append("Did you like this signing experience?"));
    var panel = $("<div class='panel' />");
    panel.append($("<div class='item' />").append(this.createFacebookLikeElems()));
    panel.append($("<div class='item' />").append(this.createTweetThisElems()));
    panel.append($("<div class='item' />").append(this.createPhoneMeElems()));
    panel.append($("<div class='item' />").append(this.createGetStartedElems()));
    panel.append($("<div class='clearfix' />"));
    container.append(panel);

    this.el.append(container);
    return this;
  }
});

window.DocumentSignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        this.model.view = this;
        this.prerender();
        this.render();
    },
    prerender: function(){
        this.container = $("<div class='mainContainer signview' />");
        this.el.append(this.container);
        this.el.addClass("body-container");
        this.el.append("<div class='clearfix'/>");
        this.el.append("<div class='spacer40'/>");
        return this;
    },
    createSignInstructionElems: function() {
      return new DocumentSignInstructionsView({
        model: this.model,
        el: $("<div/>")
      }).el;
    },
    createSaveAfterSignViewElems: function() {
      return new DocumentSaveAfterSignView({
       model: this.model,
       el: $("<div />")
      }).el;
    },
    createShareAfterSignViewElems: function() {
      return new DocumentShareAfterSignView({
        model: this.model,
        el: $("<div />")
      }).el;
    },
    createMainFileElems: function() {
      var file = KontraFile.init({
        file: this.model.mainfile(),
        document: this.model
      });
      return file.view.el;
    },
    authorAttachmentsTitle: function() {
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.authorattachments().length>1) {
        return localization.docsignview.authorAttachmentsTitleForLots;
      } else {
        return localization.docsignview.authorAttachmentsTitleForOne;
      }
    },
    createAuthorAttachmentsElems: function() {
      return new DocumentAuthorAttachmentsView({
        model: this.model,
        el: $("<div class='section'/>"),
        title: this.authorAttachmentsTitle()
      }).el;
    },
    signatoryAttachmentsTitle: function() {
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.currentSignatory().attachments().length>1) {
        return localization.docsignview.signatoryAttachmentsTitleForLots;
      } else {
        return localization.docsignview.signatoryAttachmentsTitleForOne;
      }
    },
    createSignatoryAttachmentsElems: function() {
      return new DocumentSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section'/>"),
        title: this.signatoryAttachmentsTitle()
      }).el;
    },
    signatoryAttachmentTasks: function(el) {
      var attachmentels = el.find(".list .item .second.column");
      var attachments = this.model.currentSignatory().attachments();
      if (attachments.length!=attachmentels.length) {
        console.error("expected to find an element per attachment");
        return;
      }
      var tasks = [];
      for (var i=0; i<attachments.length; i++) {
        tasks.push(this.signatoryAttachmentTask(attachments[i], attachmentels.eq(i)));
      }
      return tasks;
    },
    signatoryAttachmentTask: function(attachment, el) {
      return new DocumentSignViewTask({
        model: attachment,
        label: localization.docsignview.uploadPDFFile,
        isComplete: function() {
          return attachment.hasFile();
        },
        el: el
      });
    },
    customFieldTasks: function(el) {
      var fields = this.model.currentSignatory().customFields();
      var fieldels = el.find(".signatory.current .dragfield input");
      if (fields.length!=fieldels.length) {
        console.error("expected to find an element per custom field");
        return;
      }
      var tasks = [];
      for (var i=0; i<fields.length; i++) {
        tasks.push(this.customFieldTask(fields[i], fieldels.eq(i)));
      }
      return tasks;
    },
    customFieldTask: function(field, el) {
      /**
       * this stuff is kind of disgusting
       * the idea is to delay completing a field for
       * a little while to give the person a change
       * to type it all in before moving the arrow
       */
      var completiontime = undefined;
      var lastvalue = undefined;
      var queueChange = function() {
        console.log("Delaying field completion...");
        window.setTimeout(function() {
          field.change();
        }, 500);
      };
      return new DocumentSignViewTask({
        model: field,
        label: localization.docsignview.fillInYourDetails,
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
            queueChange();
          } else if (!lastvalue &&
                       newvalue &&
                       completiontime != undefined) {
            var elapsedtime = (new Date()).getTime() - completiontime.getTime();
            if (elapsedtime >= 2000) {
              completiontime = undefined;
              returnvalue = newvalue;
            } else {
              queueChange();
            }
          }

          lastvalue = returnvalue;
          return returnvalue;
        },
        el: el
      });
    },
    createUploadedAttachmentsElems: function() {
      return new DocumentUploadedSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section' />"),
        title: localization.docsignview.uploadedAttachmentsTitle
      }).el;
    },
    createSignatoriesElems: function() {
      return new DocumentSignSignatoriesView({
        model: this.model,
        el: $("<div class='section'/>")
      }).el;
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
            editText:  localization.reject.editMessage,
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
      return new DocumentSignButtonView({
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
      }).el;
    },
    signButtonTask: function(el) {
      var document = this.model;
      return new DocumentSignViewTask({
        model: document,
        label: localization.docsignview.clickToSign,
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
      return new DocumentSignViewArrowView({
        model: model,
        el: $("<div />")
      }).el;
    },
    makeVisibleWhenMainFileReady: function(elems) {
      if (this.mainfilebound==undefined) {
        var mainfile = this.model.mainfile();
        var updateElemsVisibility = function() {
          _.each(elems, function(elem) {
            if (mainfile.ready()) {
              elem.show();
            } else {
              elem.hide();
            }
          });
        };
        this.model.mainfile().bind('reset', updateElemsVisibility);
        this.model.mainfile().bind('change', updateElemsVisibility);
        updateElemsVisibility();
        this.mainfilebound = true;
      }
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
    render: function () {
      var document = this.model;
      if (!document.ready())
          return this;

      this.container.empty();

      var tasks = [];

      this.container.append(this.createSignInstructionElems());
      if (document.currentSignatory().hasSigned()) {
        this.container.append(this.createSaveAfterSignViewElems());
        this.container.append(this.createShareAfterSignViewElems());
      }

      var subcontainer = $("<div class='subcontainer'/>");

      subcontainer.append(this.createMainFileElems());

      if (this.isBottomStuff()) {
        var bottomstuff = $("<div class='bottomstuff' />");

        if (this.model.isAuthorAttachments()) {
          bottomstuff.append(this.createAuthorAttachmentsElems());
        }

        if (this.model.isSignatoryAttachments()) {
          var signatoryattachments = this.createSignatoryAttachmentsElems();
          if (this.model.currentSignatoryCanSign()) {
            _.each(this.signatoryAttachmentTasks(signatoryattachments), function(task) {
              tasks.push(task);
            });
          }
          bottomstuff.append(signatoryattachments);
        }

        if (this.model.isUploadedAttachments()) {
          bottomstuff.append(this.createUploadedAttachmentsElems());
        }

        if (this.isDisplaySignatories()) {
          var signatories = this.createSignatoriesElems();
          if (this.model.currentSignatoryCanSign()) {
            _.each(this.customFieldTasks(signatories), function(task) {
              tasks.push(task);
            });
          }
          bottomstuff.append(signatories);
        }

        if (this.model.currentSignatoryCanSign()) {
          var signsection = $("<div class='section' />");
          signsection.append(this.createRejectButtonElems());
          var signButton = this.createSignButtonElems(jQuery.extend({}, tasks));
          tasks.push(this.signButtonTask(signButton));
          signsection.append(signButton);
          signsection.append($("<div class='clearfix' />"));
          bottomstuff.append(signsection);
        }

        subcontainer.append(bottomstuff);

        subcontainer.append($("<div class='end' />"));
        subcontainer.append($("<div class='cleafix' />"));

        this.makeVisibleWhenMainFileReady([bottomstuff]);
      }
      this.container.append(subcontainer);

      if (!this.model.currentSignatory().hasSigned()) {
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
    this.isComplete = args.isComplete;
    this.el = args.el;
    this.model.bind('reset', this.update);
    this.model.bind('change', this.update);
    this.update();
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
  },
  label: function() {
    return this.get("label");
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
    return this.nextIncompleteTask()!=undefined;
  },
  nextIncompleteTask: function() {
    var tasks = this.tasks();
    for (var i=0; i<tasks.length; i++) {
      if (!tasks[i].complete()) {
        return tasks[i];
      }
    }
    return undefined;
  }
});

window.DocumentSignViewArrowView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    var render = this.render;
    _.each(this.model.tasks(), function(task) {
      task.bind("change", render);
    });
    this.render();
  },
  render: function() {
    this.el.empty();

    var taskmodel = this.model;

    var container = $("<div class='arrows' />");

    var downarrow = $("<div class='down arrow' />");
    container.append(downarrow);

    var uparrow = $("<div class='up arrow' />");
    container.append(uparrow);

    if (taskmodel.isIncompleteTask()) {
      downarrow.css("cursor", "pointer");
      downarrow.click(function() {
        var el = taskmodel.nextIncompleteTask().el;
        var scrollbottom = el.offset().top + el.height() + 100;
        $('html,body').animate({
          scrollTop: scrollbottom - $(window).height()
        }, 2000);
      });
      uparrow.css("cursor", "pointer");
      uparrow.click(function() {
        var el = taskmodel.nextIncompleteTask().el;
        $('html,body').animate({
          scrollTop: el.offset().top - 100
        }, 1000);
      });
    }

    var actionarrow = $("<div class='action arrow'/>");
    if (taskmodel.isIncompleteTask()) {
      actionarrow.append($("<div class='label' />").append(taskmodel.nextIncompleteTask().label()));
    }
    container.append(actionarrow);

    var updateRightMargin = function() {
      var space = $(window).width() - 941;
      var margin = 0;
      if (space>0) {
        margin = space / 2;
      }
      downarrow.css("right", margin + "px");
      uparrow.css("right", margin + "px");
      actionarrow.css("right", (margin - 20) + "px");
    };
    $(window).resize(updateRightMargin);
    updateRightMargin();

    var updateActionArrowPosition = function() {
      if (taskmodel.isIncompleteTask()) {
        var el = taskmodel.nextIncompleteTask().el;
        actionarrow.css("top", (el.offset().top + (el.height() / 2) - 14)+ "px");
        actionarrow.css("right", ($(window).width() - (el.offset().left + el.width() + 102)) + "px");
      }
    };
    $(window).resize(updateActionArrowPosition);
    updateActionArrowPosition();

    var updateVisibility = function() {

      downarrow.hide();
      uparrow.hide();
      actionarrow.hide();
      $(".signview .section").removeClass("highlight");

      if (!taskmodel.isIncompleteTask()) {
        downarrow.show();
      } else {
        var scrolltop = $(window).scrollTop();
        var scrollbottom = scrolltop + $(window).height();
        var eltop = taskmodel.nextIncompleteTask().el.offset().top;
        var elbottom = eltop + taskmodel.nextIncompleteTask().el.height();

        var bottommargin = 0;
        var topmargin = 0;

        if ((scrolltop >= 0) && (elbottom <= eltop)) {
          console.log("waiting to show arrow ....");
          window.setTimeout(updateVisibility, 500);
        } else if (((elbottom + bottommargin) <= scrollbottom) && ((eltop - topmargin) >= scrolltop)) {
          taskmodel.nextIncompleteTask().el.parents(".signview .section").addClass("highlight");
          updateActionArrowPosition();
          actionarrow.show();
        } else if ((elbottom + bottommargin) > scrollbottom) {
          downarrow.show();
        } else {
          uparrow.show();
        }
      }
    };
    $(window).resize(updateVisibility);
    $(window).scroll(updateVisibility);
    updateVisibility();

    this.el.append(container);
  }
});

window.KontraSignDocument = {
  init: function(args){
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
