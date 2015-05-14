/* Signatory view of document
 */

define(['Backbone', 'legacy_code'], function() {

var SignatoryAttachmentUploadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
    this.signview = args.signview;
    this.uploadButton = this.newUploadButton();
    this.render();
  },
  setattachmentURL: function() {
    return "/api/frontend/setsignatoryattachment/" + this.model.document().documentid() + "/" + this.model.document().viewer().signatoryid() + "/" + encodeURIComponent(this.model.name()) + "" + this.model.document().viewer().urlPart();
  },
  removeButton: function() {
    var self = this;
    var attachment = this.model;
    var button = new Button({type: "cancel", text: localization.deletePDF, size:'small', onClick: function() {
            attachment.loading();
            self.uploadButton = self.newUploadButton();
            new Submit({
                    method: "POST",
                    expectedType : "json",
                    url: self.setattachmentURL(),
                    ajax: true,
                    ajaxerror: function(d, a) {
                      attachment.notLoading();
                    },
                    ajaxsuccess: function(d) {
                      attachment.unset('file');
                      attachment.notLoading();
                    }
                  }).send();
            return false;
            }});
    return button;
  },
  newUploadButton: function() {
    var self = this;
    var attachment = this.model;
    return new UploadButton({
      width : 230,
      size : 'small',
      name: "file",
      type: "action",
      cssClass: "attachment-upload-button",
      text: localization.signatoryAttachmentUploadButton,
      submitOnUpload: true,
      showLoadingDialog: false,
      onError: function() {
        attachment.notLoading();
        attachment.trigger('change');
      },
      submit: new Submit({
        method: "POST",
        url: self.setattachmentURL(),
        attachname: attachment.name(),
        sigattachment: "YES",
        ajax: true,
        expectedType : "json",
        onSend: function() {
          attachment.loading();
        },
        ajaxerror: function(d, a) {
            if (d != undefined && d.status == 409) {
              var button =  new Button({ type: 'optional',
                                         size: 'small',
                                         text: localization.reloadPage,
                                         onClick: function() {
                                           document.location.reload(true);
                                         }
                                        });
              var content = $('<div/>');
              content.text(localization.signviewAttachmentUploadedInOtherWindow);
              content.append($('<div style="margin-top: 40px;" />'));
              content.append(button.el());
              ScreenBlockingDialog.open({header: content});
          }
          else
            new FlashMessage({content: localization.couldNotUpload, type: 'error'});
          attachment.notLoading();
        },
        ajaxsuccess: function(docdata) {
              var olddocument = attachment.signatory().document();

              var newdoc = new Document(new Document({id: olddocument.documentid(), viewer : olddocument.viewer()}).parse(docdata));
              _.each(newdoc.currentSignatory().attachments(), function(a) {
                if (a.name() == attachment.name())
                    attachment.setFile(a.file());
              });
              attachment.notLoading();
        }
      })
    });
  },
  getButton: function () {
    if (this.uploadButton != undefined)
        return this.uploadButton;
  },
  reviewButton: function() {
      var model = this.model;
      var button = new Button({text: localization.reviewPDF, size:'small', cssClass : 's-review-sigattachment', onClick: function() {
          window.open(model.file().downloadLink(), '_blank');
      }});
      return button;
  },
  render: function() {
      var self = this;
      var attachment = self.model;
      var container = $("<div class='item' />");
      if (attachment.get('loading')) {
          container.append($("<img class='loading'>").attr('src', "/img/wait30trans.gif"));
      } else if (attachment.hasFile()) {
          container.append(this.reviewButton().el());
          if (attachment.signatory().document().currentSignatoryCanSign())
            container.append(this.removeButton().el());

      } else if (attachment.signatory().document().pending() || attachment.signatory().document().currentSignatoryCanSign()){
        container.append(self.newUploadButton().el().addClass('float-right').css("overflow","hidden"));
      }
      container.append($("<div class='clearfix' />"));

      $(this.el).empty();
      $(this.el).append(container);
      this.signview.updateArrowPosition();
      return this;
  }
});

window.DocumentSignatoryAttachmentsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.title = args.title;
    this.subtitle = args.subtitle;
    this.uploadViews = [];
    this.render();
  },
  signatoryAttachmentDescription: function(attachment) {
    var self = this;
    var container = $("<div class='item' />");
    var filename = $("<div class='label' />");
    var name = $("<div class='name' />");
    var desc = $("<div class='description' />");
    if (attachment.file() != undefined)
      container.append($("<div class='filename'/>").append($("<div class='icon'/>")).append(filename.text(attachment.file().name())));
    container.append(name.text(attachment.name()));
    container.append(desc.text(attachment.description()));
    return container;
  },
  signatoryAttachmentFile: function(attachment) {
    var upl = new SignatoryAttachmentUploadView({
      model: attachment,
      el: $("<div/>"),
      signview: this.model
    });
    this.uploadViews.push(upl);
    return upl.el;
  },
  render: function() {
    $(this.el).empty();
    var self = this;

    if (!this.model.document().currentSignatory().attachments().length > 0) {
      return this;
    }

    var document = this.model.document().signatories()[0].document();


    var header = $("<h2/>");

    var container = $("<div class='signatoryattachments' />");
    var header = $("<div class='header'/>");
    container.append(header);
    header.append($("<h2/>").text(this.title == undefined ? localization.requestedAttachments : this.title));
    if (this.subtitle != undefined)
      header.append($("<div class='subtitle'/>").text(this.subtitle));
    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.document().currentSignatory().attachments(), function(attachment) {
      var desc = $("<td class='desc'>").append(self.signatoryAttachmentDescription(attachment));
      var file = $("<td class='file'>").append(self.signatoryAttachmentFile(attachment));
      self.listenTo(attachment, 'change', function() {desc.empty().append(self.signatoryAttachmentDescription(attachment));});
      tbody.append($("<tr/>").append(desc).append(file));
    });

    container.append(table);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

});
