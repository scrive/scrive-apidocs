/* Signatory view of document
 */


(function(window) {

var SignatoryAttachmentUploadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
    this.signview = args.signview;
    this.labelstyle = '';
    for (var key in args.labelCss) {
      this.labelstyle += key + ': ' + args.labelCss[key] + ' !important; ';
    }
    this.render();
  },
  setattachmentURL: function() {
    return "/api/frontend/setsignatoryattachment/" + this.model.document().documentid() + "/" + this.model.document().viewer().signatoryid() + "/" + encodeURIComponent(this.model.name()) + "" + this.model.document().viewer().urlPart();
  },
  removeButton: function() {
    var self = this;
    var attachment = this.model;
    var button = new Button({color: "red", text: localization.deletePDF, size:'small', onClick: function() {
            attachment.loading();
            new Submit({
                    method: "POST",
                    expectedType : "json",
                    url: self.setattachmentURL(),
                    ajax: true,
                    expectedType : "text",
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
  uploadButton: function() {
    var self = this;
    var attachment = this.model;
    return new UploadButton({
      width: 200,
      size : 'small',
      name: "file",
      text: localization.signatoryAttachmentUploadButton,
      submitOnUpload: true,
      showLoadingDialog: false,
      onClick: function() {
        return true;
      },
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
              var button =  new Button({color: 'blue',
                                         size: 'small',
                                         text: 'Reload page',
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
          else if (a === 'parsererror') // file too large
            new FlashMessage({content: localization.fileTooLarge, color: "red"});
          else
            new FlashMessage({content: localization.couldNotUpload, color: "red"});
          attachment.notLoading();
        },
        ajaxsuccess: function(docdata) {
              var newdoc = new Document(new Document({}).parse(docdata));
              _.each(newdoc.currentSignatory().attachments(), function(a) {
                if (a.name() == attachment.name())
                    attachment.setFile(a.file());
              });
              attachment.notLoading();
        }
      })
    });
  },
  reviewButton: function() {
      var model = this.model;
      var button = new Button({color: "signview-blue", text: localization.reviewPDF, size:'small', cssClass : 's-review-sigattachment', onClick: function() {
          window.open(model.file().downloadLink(), '_blank');
          }});
      return button;
  },
  render: function() {
      var attachment = this.model;
      var container = $("<div class='item' />");
      if (attachment.get('loading')) {
          container.append($("<img class='loading'>").attr('src', "/img/wait30trans.gif"));
      } else if (attachment.hasFile()) {
          container.append(this.reviewButton().el());
          if (attachment.signatory().document().currentSignatoryCanSign())
            container.append(this.removeButton().el());

      } else if (attachment.signatory().document().pending() || attachment.signatory().document().currentSignatoryCanSign()){
          container.append(this.uploadButton().el().addClass('float-right').css("overflow","hidden"));
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
    this.textcolour = args.textcolour;
    this.textfont = args.textfont;
    this.uploadElems = [];
    this.render();
  },
  signatoryAttachmentDescription: function(attachment, labelCss) {
    var self = this;
    var container = $("<div class='item' />");
    var filename = $("<div class='label' />").css(labelCss);
    var name = $("<div class='name' />").css(labelCss);
    var desc = $("<div class='description' />").css(labelCss);
    if (attachment.file() != undefined)
      container.append($("<div class='filename'/>").append($("<div class='icon'/>")).append(filename.text(attachment.file().name())));
    container.append(name.text(attachment.name()));
    container.append(desc.text(attachment.description()));
    return container;
  },
  signatoryAttachmentFile: function(attachment, labelCss) {
    var upl = new SignatoryAttachmentUploadView({
      model: attachment,
      el: $("<div/>"),
      labelCss: labelCss,
      signview: this.model
    });
    this.uploadElems.push($(upl.el));
    return upl.el;


  },
  render: function() {
    $(this.el).empty();
    var self = this;

    if (!this.model.document().currentSignatory().attachments().length > 0) {
      return this;
    }

    var document = this.model.document().signatories()[0].document();

    var labelCss = {};

    var header = $("<h2/>");
    if (this.textcolour) {
      labelCss['color'] = this.textcolour;
    }
    if (this.textfont) {
      labelCss['font-family'] = this.textfont;
    }

    var container = $("<div class='signatoryattachments' />");
    var header = $("<div class='header'/>");
    container.append(header);
    header.append($("<h2/>").text(this.title == undefined ? localization.requestedAttachments : this.title).css(labelCss));
    if (this.subtitle != undefined)
      header.append($("<div class='subtitle'/>").text(this.subtitle).css(labelCss));
    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.document().currentSignatory().attachments(), function(attachment) {
      var desc = $("<td class='desc'>").append(self.signatoryAttachmentDescription(attachment, labelCss));
      var file = $("<td class='file'>").append(self.signatoryAttachmentFile(attachment, labelCss));
      self.listenTo(attachment, 'change', function() {desc.empty().append(self.signatoryAttachmentDescription(attachment, labelCss));});
      tbody.append($("<tr/>").append(desc).append(file));
    });

    container.append(table);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

})(window);
