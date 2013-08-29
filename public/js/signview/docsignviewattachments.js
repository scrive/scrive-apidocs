/* Signatory view of document
 */


(function(window) {

var SignatoryAttachmentUploadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
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
    var button = new Button({color: "red", text: localization.deletePDF, size:'tiny', onClick: function() {
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
      width: 120,
      size : 'tiny',
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
      var button = new Button({color: "green", text: localization.reviewPDF, size:'tiny', cssClass : 's-review-sigattachment', onClick: function() {
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
          container.append($("<div class='icon' />"));
          var label = $("<div class='file' />");
          label.append($("<div class='name' />").text(this.model.file().name() + ".pdf"));
          label.append($("<div class='clearfix' />"));
          container.append(label);
          container.append(this.reviewButton().el());
          if (attachment.signatory().document().currentSignatoryCanSign())
            container.append(this.removeButton().el());

      } else if (attachment.signatory().document().pending() || attachment.signatory().document().currentSignatoryCanSign()){
          container.append(this.uploadButton().el().addClass('float-right').css("height","40px").css("overflow","hidden"));
      }
      container.append($("<div class='clearfix' />"));

      $(this.el).empty();
      $(this.el).append(container);

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
    var container = $("<div class='item' />");
    var name = $("<div class='name' />");
    var desc = $("<div class='description' />");
    name.css(labelCss);
    desc.css(labelCss);
    container.append(name.text(attachment.name()));
    container.append(desc.text(attachment.description()));
    return container;
  },
  signatoryAttachmentFile: function(attachment, labelCss) {
    var upl = new SignatoryAttachmentUploadView({
      model: attachment,
      el: $("<div/>"),
      labelCss: labelCss
    });
    this.uploadElems.push($(upl.el));
    return upl.el;


  },
  render: function() {
    $(this.el).empty();
    var self = this;

    if (!this.model.currentSignatory().attachments().length > 0) {
      return this;
    }

    var document = this.model.signatories()[0].document();

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
    _.each(this.model.currentSignatory().attachments(), function(attachment) {
      var tr = $("<tr/>");
      tr.append($("<td class='desc'>").append(self.signatoryAttachmentDescription(attachment, labelCss)));
      tr.append($("<td class='file'>").append(self.signatoryAttachmentFile(attachment, labelCss)));
      tbody.append(tr);
    });

    container.append(table);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

})(window);
