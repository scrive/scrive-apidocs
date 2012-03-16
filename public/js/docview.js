/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){


window.DocumentDownloadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  render: function() {
    $(this.el).empty();

    if (this.model.mainfile()==undefined) {
      return this;
    }

    var link = $("<a target='_blank' />");
    link.attr("href", this.model.mainfile().downloadLink());
    link.append($("<div class='float-left icon' />"));
    link.append($("<div class='float-left label' />").text(localization.downloadPDF));
    link.append($("<div class='float-left docname' />").text(this.model.mainfile().name() + ".pdf"));
    link.append($("<div class='clearfix' />"));

    $(this.el).append($("<div class='download' />").append(link));

    return this;
  }
});

window.DocumentActionMenuView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  createDownloadElems: function() {
    return $(new DocumentDownloadView({
                  model: this.model,
                  el: $("<div/>")
    }).el);
  },
  render: function() {
    $(this.el).empty();
    $(this.el).append($("<div class='menu' />").append(this.createDownloadElems()));

    return this;
  }
});

window.DocumentAuthorAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.title = args.title;
    this.render();
  },
  createAuthorAttachmentElems: function(attachment) {
    var container = $("<div class='item' />");
    container.append($("<div class='icon' />"));
    var label = $("<div class='label' />");
    label.append($("<div class='name' />").text(attachment.name()));
    var link = $("<a target='_blank' />");
    link.text(localization.reviewPDF);
    link.attr("href", attachment.downloadLink());
    label.append(link);
    container.append(label);
    container.append($("<div class='clearfix' />"));
    return container;
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.isAuthorAttachments()) {
      return this;
    }

    var container = $("<div class='authorattachments' />");
    container.append($("<h2/>").text(this.title==undefined ? localization.authorAttachmentBoxHeader : this.title));
    var list = $("<div class='list' />");
    var createAuthorAttachmentElems = this.createAuthorAttachmentElems;
    _.each(this.model.authorattachments(), function(attachment) {
      list.append(createAuthorAttachmentElems(attachment));
    });
    list.append($("<div class='clearfix' />"));
    container.append(list);

    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentSignatoryAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.title = args.title;
    this.uploadElems = [];
    this.render();
  },
  createSignatoryAttachmentView: function(attachment) {
    return new SignatoryAttachmentView({
      model: attachment,
      el : $("<div/>")
    });
  },
  render: function() {
    $(this.el).empty();
    var view = this;

    if (!this.model.isSignatoryAttachments()) {
      return this;
    }

    var container = $("<div class='signatoryattachments' />");
    container.append($("<h2/>").text(this.title==undefined ? localization.requestedAttachments : this.title));

    var list = $("<div class='list'/>");
    _.each(this.model.currentSignatory().attachments(), function (attachment) {
      var attachmentview = view.createSignatoryAttachmentView(attachment);
      view.uploadElems.push(attachmentview.uploadElems);
      list.append($(attachmentview.el));
    });
    list.append($("<div class='clearfix' />"));
    container.append(list);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentUploadedSignatoryAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.title = args.title;
    this.render();
  },
  createUploadedAttachmentElems: function(attachment) {
    return $(new UploadedSignatoryAttachmentView({
      model: attachment,
      el : $("<div/>")
    }).el);
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.isUploadedAttachments()) {
      return this;
    }

    var container = $("<div class='uploadedsignatoryattachments' />");
    container.append($("<h2/>").text(this.title==undefined ? localization.uploadedAttachments : this.title));

    var createUploadedAttachmentElems = this.createUploadedAttachmentElems;
    var list = $("<div class='list'/>");
    _.each(this.model.signatoryattachments(), function (attachment) {
      list.append(createUploadedAttachmentElems(attachment));
    });
    container.append(list);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

window.GuardModel = Backbone.Model.extend({
  defaults: {
    ischecked: false,
    ishighlighted: false,
    isrequired: true
  },
  initialize: function(args) {
    this.onAlert = args.onAlert;
  },
  isChecked: function() {
    return this.get("ischecked");
  },
  setChecked: function(checked) {
    if (checked) {
      this.set({
        ischecked: true,
        ishighlighted: false,
      });
    } else {
      this.set({ ischecked: false });
    }
  },
  isHighlighted: function() {
    return this.get("ishighlighted");
  },
  isRequired: function() {
    return this.get("isrequired");
  },
  ensureChecked: function() {
    if (this.isRequired() && !this.isChecked()) {
      this.set({ ishighlighted: true });
      if (this.onAlert) {
        this.onAlert();
      }
      return false;
    } else {
      return true;
    }
  }
});


window.DocumentSignGuardView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender: function() {
    $(this.el).empty();

    if (!this.model.isRequired()) {
      return this;
    }

    var guard = this.model;

    var checkbox =  $("<input type='checkbox'  id='signGuardCBox' autocomplete='off'/>");
    checkbox.click(function() {
      guard.setChecked(checkbox.is(":checked"));
    });
    this.checkbox = checkbox;

    this.container = $("<div class='signguard' />");
    this.container.append($("<div class='check' />").append(this.checkbox));
    this.container.append($("<div class='label' />").append($("<label for='signGuardCBox' />").append(localization.sign.guardCBox)));

    $(this.el).append(this.container);

    return this;
  },
  render: function() {
    if (!this.model.isRequired()) {
      return this;
    }

    if (this.model.isChecked() &&
          !this.checkbox.is(":checked")) {
      this.checkbox.attr("checked", "true");
    } else if (!this.model.isChecked() &&
                  this.checkbox.is(":checked")) {
      this.checkbox.removeAttr("checked");
    }

    if (this.model.isHighlighted()) {
      this.container.css("border", "solid red 1px");
    } else {
      this.container.css("border", "none");
    }
  }
});

window.DocumentSignButtonView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.validate = args.validate;
    this.render();
  },
  confirm: function() {
    return new DocumentSignConfirmation({
      model: this.model
    }).popup();
  },
  render: function() {
    if (!this.model.currentSignatoryCanSign()) {
      return this;
    }

    var sign = this;
    $(this.el).append($("<div class='sign' />").append(Button.init({
      size: "big",
      color: "blue",
      text: this.model.process().signbuttontext(),
      icon: $("<span class='icon cross'></span>"),
      onClick: function() {
        if (sign.validate()) {
          sign.confirm();
        }
      }
    }).input()));

    return this;
  }
});

window.DocumentSignConfirmation = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'popup');
    _.bindAll(this, 'createContentElems');
    var guardWarnText = this.model.process().signguardwarntext();
    this.guardModel = new GuardModel({
      onAlert: function() {
        FlashMessages.add({
          content: guardWarnText,
          color: "red"});
        return this;
      },
      isrequired: this.model.process().requiressignguard()
    });
  },
  createElegButtonElems: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
    var telia = $("<a href='#' class='author2 telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
    var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
    bankid.click(function() {
      Eleg.bankidSign(document,signatory, document.sign());
      return false;
    });
    telia.click(function() {
      Eleg.teliaSign(document,signatory, document.sign());
      return false;
    });
    nordea.click(function() {
      Eleg.nordeaSign(document,signatory, document.sign());
      return false;
    });
    return $("<span />").append(bankid).append(telia).append(nordea);
  },
  createSignButtonElems: function() {
    var document = this.model;
    var guardModel = this.guardModel;
    return Button.init({
      size: "small",
      color: "blue",
      icon: $("<span class='btn-symbol cross' style='margin-left: 10px'/>"),
      text: document.process().signbuttontext(),
      onClick: function() {
        if (guardModel.ensureChecked()) {
          document.sign().send();
        }
      }
    }).input();
  },
  createPreambleElems: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    if (signatory.author) {
      var content = $("<div />").append(document.lastSignatoryLeft() ? $(document.process().signatorysignmodalcontentauthorlast()) : $(document.process().signatorysignmodalcontentnotlast()));
      if (document.elegAuthorization()) {
        var subhead = $("<h3/>").text(localization.signByAuthor.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.signByAuthor.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.signByAuthor.eleg.body1).append(a).append(localization.signByAuthor.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    } else {
      var content = $("<div />").append(document.lastSignatoryLeft() ? $(document.process().signatorysignmodalcontentlast()) : $(document.process().signatorysignmodalcontentnotlast()));
      if (document.elegAuthorization()) {
        var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    }
  },
  createSignGuardElems: function() {
    return $(new DocumentSignGuardView({
      model: this.guardModel,
      el: $("<div />")
    }).el);
  },
  createContentElems: function() {
    var content = $("<div />");
    content.append(this.createPreambleElems());
    content.append(this.createSignGuardElems());
    return content;
  },
  popup: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    Confirmation.popup({
      title: signatory.author ? localization.signByAuthor.modalTitle : document.process().signatorysignmodaltitle(),
      acceptButton: document.elegAuthorization() ? this.createElegButtonElems() : this.createSignButtonElems(),
      rejectText: localization.cancel,
      content: this.createContentElems
    });
  }
});

window.DocumentStandardView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    _.bindAll(this, 'validateSign');
    this.model.bind('reset', this.render);
    this.model.bind('change', this.render);
    this.model.view = this;
    this.prerender();
    this.render();
  },
  prerender: function() {
    this.container = $("<div class='mainContainer docview' />");
    $(this.el).append(this.container);
    $(this.el).addClass("body-container");
    $(this.el).append("<div class='clearfix'/>");
    $(this.el).append("<div class='spacer40'/>");
  },
  createAttachmentsTabElems: function() {
    var attachmenttab = $("<span id='attachmenttabview' class='attachmentstab' />");
    var body = $("<div class='signStepsBody ericfix forauthor'/>");
    body.append($("<div class='section'/>").append(this.createAuthorAttachmentsElems()));
    body.append($("<div class='section' />").append(this.createUploadedAttachmentsElems()));
    attachmenttab.append(body);
    return attachmenttab;
  },
  createSignatoriesTabElems: function() {
    var document = this.model;
    var signatoriestabview = $("<span id='documenttabview' />");
    var body = $("<div class='signStepsBody ericfix forauthor'/>");
    var firstbox = $("<div id='signViewBodyLeft' />");
    firstbox.append(document.infotext());
    firstbox.append("<BR/>");
    // Making restart button
    if (document.canberestarted()) {
      firstbox.append("<BR/>");
      firstbox.append(this.createRestartButtonElems());
    }
    // Making cancel button
    if (document.canbecanceled()) {
      firstbox.append("<BR/>");
      firstbox.append(this.createCancelButtonElems());
    }

    var middlebox = $("<div class='float-left signViewBodyBox'/>");
    if (document.currentSignatory() != undefined) {
        if (document.currentSignatory().author() || document.currentSignatory().signs()) {
          var currentsignatoryview = new SignatoryStandardView({model : document.currentSignatory(), el : $("<div/>")});
          middlebox.append($(currentsignatoryview.el));
        }
    }

    var lastbox = $("<div class='float-right signViewBodyBox'/>");
    var othersignatories = document.otherSignatories();
    _.each(document.otherSignatories(), function(signatory) {
        if (signatory.author() || signatory.signs()) {
          var signatoryview = new SignatoryStandardView({model : signatory, el : $("<div/>")});
          lastbox.append($(signatoryview.el));
        }
    });


    body.append(firstbox);
    body.append(middlebox);
    body.append(lastbox);
    signatoriestabview.append(body);
    return signatoriestabview;

  },
  createMenuElems: function() {
    return $(new DocumentActionMenuView({
      model: this.model,
      el: $("<div/>")
    }).el);
  },
  createRestartButtonElems: function() {
    var document = this.model;
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().restartbuttontext(),
      onClick: function(){ document.restart().send(); }
    }).input();
  },
  createCancelButtonElems: function() {
    var document = this.model;
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().cancelbuttontext(),
      onClick: function(){
        Confirmation.popup({
          title: document.process().cancelmodaltitle(),
          content: document.process().cancelmodaltext(),
          acceptText: document.process().cancelbuttontext(),
          rejectText: localization.cancel,
          acceptColor: "red",
          onAccept: function() {
              document.cancel().send();
              return true;
            }
          });
        }
    }).input();
  },
  createAuthorAttachmentsElems : function() {
    return $(new DocumentAuthorAttachmentsView({
      model: this.model,
      el: $("<div />")
    }).el);
  },
  createSignatoryAttachmentsElems : function() {
    return $(new DocumentSignatoryAttachmentsView({
      model: this.model,
      el: $("<div />")
    }).el);
  },
  createUploadedAttachmentsElems: function() {
    return $(new DocumentUploadedSignatoryAttachmentsView({
      model: this.model,
      el: $("<div />")
    }).el);
  },
  createAcceptButtonElems: function() {
    var validateSign = this.validateSign;
    return $(new DocumentSignButtonView({
      model: this.model,
      validate: function() {
        return validateSign();
      },
      el: $("<div />")
    }).el);
  },
  validateSign: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    if (!signatory.signatureReadyForSign()) {
      signatory.signature().trigger('empty');
      FlashMessages.add({content: localization.signature.missing, color: "red"});
      return false;
    }

    if (!signatory.allFieldsReadyForSign()) {
      _.each(signatory.fields(),function(field) {
        if (!field.readyForSign() && field.view != undefined)
          field.view.redborder();
      });
      FlashMessages.add({content: localization.mustFillFieldsBeforeSigning, color: "red"});
      return false;
    }

    if (!signatory.allAttachemntHaveFile()) {
      _.each(signatory.attachments(),function(attachment) {
        if (!attachment.hasFile())
          $(attachment.view.el).addClass("redborder");
      });
      FlashMessages.add({content: localization.addRequiredAttachments, color: "red"});
      return false;
    }

    return true;
  },
  createRejectButtonElems: function() {
    var document = this.model;
    var signatory = document.currentSignatory();
    return Button.init({
      size:"small",
      color: "red",
      text: document.process().rejectbuttontext(),
      style: "width: 150px;margin-right: 12px;",
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
    }).input();
  },
  createSignBoxElems: function() {
    var leftbox = $("<div id='signViewBottomBoxContainerLeft'/>");
    if (this.model.currentSignatory().author()) {
      leftbox.append(this.createCancelButtonElems());
    } else {
      leftbox.append(this.createRejectButtonElems());
    }

    var rightbox = $("<div id='signViewBottomBoxContainerRight'/>");
    rightbox.append(this.createAcceptButtonElems());

    var box = $("<div id='signViewBottomBox'/>");
    box.append(leftbox);
    box.append(rightbox);

    return box;
  },
  render: function () {
    var document = this.model;
    if (!document.ready())
        return this;

    this.container.empty();

    var titlepart = $("<span id='signStepsTitleRowTextContainer'/>");
    titlepart.append($("<span class='title'/>").text(document.process().title() + " #" + document.documentid() + ": "));
    titlepart.append($("<span class='name'/>").text(document.title()));

    var bottomparts = $("<div/>");
    // Author attachment box
    if (document.currentSignatory() != undefined &&
          document.currentSignatory().signs() &&
          !document.currentSignatory().hasSigned() &&
          !document.currentSignatory().author() &&
          document.authorattachments().length > 0 &&
          document.signingInProcess()) {
      bottomparts.append(this.authorAttachments()) ;
    }

    // Signatory attachment box
    if (document.currentSignatory() != undefined &&
            document.currentSignatory().signs() &&
            !document.currentSignatory().hasSigned() &&
            document.currentSignatory().attachments().length > 0 &&
            document.signingInProcess()) {
      bottomparts.append(this.createSignatoryAttachmentsElems()) ;
    }

    if (document.currentSignatoryCanSign()) {
      bottomparts.append(this.createSignBoxElems());
    }

    var file = KontraFile.init({
      file: document.mainfile(),
      document: document
    });
    var tabs = KontraTabs.init({
    title: jQuery.merge(titlepart, this.createMenuElems()),
    tabs: [
      new Tab({
        name : localization.document,
        elems: [ this.createSignatoriesTabElems(),
                 $(file.view.el),
                 bottomparts
               ]
        }),
      new Tab({
          name: localization.attachments,
          elems: [ this.createAttachmentsTabElems(),
                   $(file.view.el),
                   bottomparts
                 ],
          disabled: !this.model.hasAnyAttachments() ||
                      ( document.currentSignatory() != undefined &&
                        document.currentSignatory().signs() &&
                        !document.currentSignatory().hasSigned() &&
                        document.signingInProcess() &&
                        !document.currentSignatory().author()
                      )
        })
      ]
    });
    this.container.append($(tabs.view.el));

    return this;
  }
});

window.KontraStandardDocument = {
  init: function(args) {
    this.model = new Document({
      id: args.id,
      viewer: args.viewer
    });
    this.view = new DocumentStandardView({
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
