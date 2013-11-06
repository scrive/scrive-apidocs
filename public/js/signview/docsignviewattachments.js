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
  uploadURL: function() {
    return "/api/frontend/addsignatoryattachment/" + this.model.document().documentid() + "/" + this.model.document().viewer().signatoryid() + "/" + encodeURIComponent(this.model.name()) + "" + this.model.document().viewer().urlPart();
  },
  deleteURL: function() {
    return "/api/frontend/deletesignatoryattachment/" + this.model.document().documentid() + "/" + this.model.document().viewer().signatoryid() + "/" + encodeURIComponent(this.model.name()) + "" + this.model.document().viewer().urlPart();
  },
  removeButton: function() {
    var attachment = this.model;
    var deleteurl = this.deleteURL();
    var button = new Button({color: "red", text: localization.deletePDF, size:'small', onClick: function() {
            attachment.loading();
            $.ajax(deleteurl, {
              type: 'DELETE',
              success: function(d) {
                attachment.unset('file');
                attachment.notLoading();
              },
              error: function() {
                attachment.notLoading();
                console.log("error");
              }
            });
            return false;
          }});
    return button;
  },
  uploadButton: function() {
    var attachment = this.model;
    var uploadurl = this.uploadURL();
    return new UploadButton({
      width: 160,
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
        url: uploadurl,
        attachname: attachment.name(),
        sigattachment: "YES",
        ajax: true,
        expectedType : "text",
        onSend: function() {
          attachment.loading();
        },
        ajaxerror: function(d, a) {
          try {
            var response = JSON.parse(d.responseText);
            if (response.message == 'There is already a file attached for that attachment request.') {
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
              return;
            }
          } catch (e) {
          }
          if (a === 'parsererror') // file too large
            new FlashMessage({content: localization.fileTooLarge, color: "red"});
          else
            new FlashMessage({content: localization.couldNotUpload, color: "red"});
          attachment.notLoading();
        },
        ajaxsuccess: function(d) {
             try {
              var doc =  attachment.signatory().document();
              var file = new File(_.extend(JSON.parse(d).file, {document: doc }));
              attachment.setFile(file);
              attachment.notLoading();
             } catch(e) { attachment.notLoading(); }

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
    var header = $("<h2/>");
    header.css(labelCss);
    container.append(header.text(this.title == undefined ? localization.requestedAttachments : this.title));
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
