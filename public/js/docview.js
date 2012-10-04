/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window) {


window.DocumentDownloadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.render();
  },
  render: function() {
    $(this.el).empty();

    if (this.model.mainfile() == undefined) {
      return this;
    }

    var link = $("<a target='_blank' />");
    link.attr("href", this.model.mainfile().downloadLinkForMainFile(this.model.title()));
    link.append($("<div class='float-left icon' />"));
    link.append($("<div class='float-left label' />").text(localization.downloadPDF));
    link.append($("<div class='float-left docname' />").text(this.model.title() + ".pdf"));
    link.append($("<div class='clearfix' />"));

    $(this.el).append($("<div class='download' />").append(link));

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

    if (!this.model.authorattachments().length > 0) {
      return this;
    }

    var container = $("<div class='authorattachments' />");
    container.append($("<h2/>").text(this.title == undefined ? localization.authorAttachmentBoxHeader : this.title));
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
      el: $("<div/>")
    });
  },
  render: function() {
    $(this.el).empty();
    var view = this;

    if (!this.model.currentSignatory().attachments().length > 0) {
      return this;
    }

    var container = $("<div class='signatoryattachments' />");
    container.append($("<h2/>").text(this.title == undefined ? localization.requestedAttachments : this.title));

    var list = $("<div class='list'/>");
    _.each(this.model.currentSignatory().attachments(), function(attachment) {
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
      el: $("<div/>")
    }).el);
  },
  render: function() {
    $(this.el).empty();

    if (!this.model.canseeallattachments() && this.model.signatoryattachments().length > 0) {
      return this;
    }

    var container = $("<div class='uploadedsignatoryattachments' />");
    container.append($("<h2/>").text(this.title == undefined ? localization.uploadedAttachments : this.title));

    var createUploadedAttachmentElems = this.createUploadedAttachmentElems;
    var list = $("<div class='list'/>");
    _.each(this.model.signatoryattachments(), function(attachment) {
      list.append(createUploadedAttachmentElems(attachment));
    });
    container.append(list);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

window.DocumentSignConfirmation = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'popup');
    _.bindAll(this, 'createContentElems');
  },
  createElegButtonElems: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
    var telia = $("<a href='#' class='author2 telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
    var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
    var mbi = $("<a href='#' class='mbi'><img src='/img/mobilebankid.png' alt='Mobilt BankID' /></a>");      
    bankid.click(function() {
      Eleg.bankidSign(document, signatory, document.sign());
      return false;
    });
    telia.click(function() {
      Eleg.teliaSign(document, signatory, document.sign());
      return false;
    });
    nordea.click(function() {
      Eleg.nordeaSign(document, signatory, document.sign());
      return false;
    });
      mbi.click(function() {
          Eleg.mobileBankIDSign(document,signatory,document.sign());
          return false;
      });
    return $("<span />").append(bankid).append(telia).append(nordea).append(mbi);
  },
  createSignButtonElems: function() {
    var document = this.model;
    var guardModel = this.guardModel;
    return Button.init({
      size: "small",
      color: "blue",
      icon: $("<span class='btn-symbol cross' />"),
      text: document.process().signbuttontext(),
      onClick: function() {
        if (alreadyClicked(this))
          return false;
        document.sign().send();
      }
    }).input();
  },
  createPreambleElems: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    if (signatory.author) {
     var content = $("<div />");
     if (document.authorIsOnlySignatory())
            content = $(document.process().authorIsOnlySignatory());
     else if (document.elegAuthentication())
          content.append(document.process().signatorysignmodalcontentsignvieweleg());
     else 
          content.append(document.process().signatorysignmodalcontent());

     if (document.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.signByAuthor.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.signByAuthor.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.signByAuthor.eleg.body1).append(a).append(localization.signByAuthor.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    } else {
      var content = $("<div />");
      if (document.elegAuthentication())
          content.append(document.process().signatorysignmodalcontentsignvieweleg());
      else
          content.append(document.process().signatorysignmodalcontent());

      if (document.elegAuthentication()) {
        var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
        var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href", "http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
        var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
        content.add($("<span/>").append(subhead).append(p));
      }
      return content;
    }
  },
  createContentElems: function() {
    var content = $("<div />");
    content.append(this.createPreambleElems());
    return content;
  },
  popup: function() {
    var document = this.model;
    var signatory = document.currentSignatory();

    Confirmation.popup({
      title: signatory.author ? localization.signByAuthor.modalTitle : document.process().signatorysignmodaltitle(),
      acceptButton: document.elegAuthentication() ? this.createElegButtonElems() : this.createSignButtonElems(),
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
          var currentsignatoryview = new SignatoryStandardView({model: document.currentSignatory(), el: $("<div/>")});
          middlebox.append($(currentsignatoryview.el));
        }
    }

    var lastbox = $("<div class='float-right signViewBodyBox'/>");
    var othersignatories = document.otherSignatories();
    _.each(document.otherSignatories(), function(signatory) {
        if (signatory.author() || signatory.signs()) {
          var signatoryview = new SignatoryStandardView({model: signatory, el: $("<div/>")});
          lastbox.append($(signatoryview.el));
        }
    });


    body.append(firstbox);
    body.append(middlebox);
    body.append(lastbox);
    signatoriestabview.append(body);
    return signatoriestabview;

  },
  createRestartButtonElems: function() {
    var document = this.model;
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().restartbuttontext(),
      onClick: function() {
        if (alreadyClicked(this))
          return;
        document.restart().send();
      }
    }).input();
  },
  createCancelButtonElems: function() {
    var document = this.model;
    return Button.init({
      color: "red",
      size: "small",
      text: document.process().cancelbuttontext(),
      cssClass: "s-withdraw-button",
      onClick: function() {
        Confirmation.popup({
          title: document.process().cancelmodaltitle(),
          content: document.process().cancelmodaltext(),
          acceptText: document.process().cancelbuttontext(),
          rejectText: localization.cancel,
          acceptColor: "red",
          extraClass : "s-withdraw-confirmation",
          onAccept: function() {
              if (alreadyClicked(this))
                return;
              document.cancel().sendAjax(function() {window.location = window.location;});
              return true;
            }
          });
        }
    }).input();
  },
  createAuthorAttachmentsElems: function() {
    return $(new DocumentAuthorAttachmentsView({
      model: this.model,
      el: $("<div />")
    }).el);
  },
  createSignatoryAttachmentsElems: function() {
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
    var document = this.model;
    return $( Button.init({
                            size: "big",
                            color: "blue",
                            text: document.process().signbuttontext(),
                            icon: $("<span class='icon cross'></span>"),
                            onClick: function() {
                                if (validateSign())
                                    new DocumentSignConfirmation({
                                        model: document
                                        }).popup();
                            }        
                            }).input()
            );
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
      _.each(signatory.fields(), function(field) {
        if (!field.readyForSign() && field.view != undefined)
          field.view.redborder();
      });
      FlashMessages.add({content: localization.mustFillFieldsBeforeSigning, color: "red"});
      return false;
    }

    if (!signatory.allAttachemntHaveFile()) {
      _.each(signatory.attachments(), function(attachment) {
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
      size: "small",
      color: "red",
      text: document.process().rejectbuttontext(),
      style: "width: 150px;margin-right: 12px;",
      onClick: function() {
        ConfirmationWithEmail.popup({
          title: document.process().signatorycancelmodaltitle(),
          mail: signatory.rejectMail(),
          acceptText: localization.reject.send,
          editText: localization.reject.editMessage,
          rejectText: localization.cancel,
          acceptColor: "red",
          onAccept: function(customtext) {
            if (alreadyClicked(this))
              return;
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
  render: function() {
    var document = this.model;
    if (!document.ready())
        return this;

    this.container.empty();

    var titlepart = $("<span class='title'/>");
    titlepart.text(document.process().title() + " #" + document.documentid() + ": ");
    var namepart = $("<span class='name'/>");
    namepart.text(document.title());

    var bottomparts = $("<div/>");
    // Author attachment box
    if (document.currentSignatory() != undefined &&
          document.currentSignatory().signs() &&
          !document.currentSignatory().hasSigned() &&
          !document.currentSignatory().author() &&
          document.authorattachments().length > 0 &&
          document.signingInProcess()) {
      bottomparts.append(this.authorAttachments());
    }

    // Signatory attachment box
    if (document.currentSignatory() != undefined &&
            document.currentSignatory().signs() &&
            !document.currentSignatory().hasSigned() &&
            document.currentSignatory().attachments().length > 0 &&
            document.signingInProcess()) {
      bottomparts.append(this.createSignatoryAttachmentsElems());
    }

    if (document.currentSignatoryCanSign() && !(document.currentSignatory() != undefined && document.currentSignatory().canPadSignQuickSign())) {
      bottomparts.append(this.createSignBoxElems());
    }

    // Download link
    var downloadpart = $("<span class='download'/>");
    if (document.mainfile() != undefined)
      downloadpart.append($("<a target='_blank'/>").attr("href",document.mainfile().downloadLinkForMainFile()).text(localization.downloadPDF));
    var fileview = $("<div id ='documentBox'><div class='waiting4page'/></div>");
    if (document.mainfile() != undefined) {
      var file = KontraFile.init({
        file: document.mainfile()
      });
      fileview = $(file.view.el);
    }
    var tabs = new KontraTabs({
    numbers : false,
    title: titlepart.add(namepart).add(downloadpart),
    tabs: [
      new Tab({
        name: localization.document,
        elems: [this.createSignatoriesTabElems(),
                $(fileview),
                bottomparts
               ]
        }),
      new Tab({
          name: localization.attachmentsWord,
          elems: [this.createAttachmentsTabElems(),
                  $(fileview),
                  bottomparts
                 ],
          disabled: !this.model.hasAnyAttachments() ||
                      (document.currentSignatory() != undefined &&
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
