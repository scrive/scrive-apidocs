/* Signatory view of document
 */


(function(window) {

var SignatoryAttachmentUploadView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.model.view = this;
    this.render();
  },
  apiURL: function() {
    var path = document.location.pathname.split("/");
    return "/api/frontend/document/" + path[2] + "/signatory/" + path[3] + "/attachment/" + encodeURIComponent(this.model.name()) + "/file" + this.model.document().viewer().urlPart();
  },
  removeButton: function() {
    var attachment = this.model;
    var deleteurl = this.apiURL();
    var button = Button.init({color: "red", text: localization.deletePDF, size:'tiny', onClick: function() {
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
    var uploadurl = this.apiURL();
    return UploadButton.init({
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
      var button = Button.init({color: "green", text: localization.reviewPDF, size:'tiny', cssClass : 's-review-sigattachment', onClick: function() {
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
          container.append(this.reviewButton().input());
          if (attachment.signatory().document().currentSignatoryCanSign())
            container.append(this.removeButton().input());

      } else if (attachment.signatory().document().pending() || attachment.signatory().document().currentSignatoryCanSign()){
          container.append(this.uploadButton().input().addClass('float-right').css("height","40px").css("overflow","hidden"));
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
    this.uploadElems = [];
    this.render();
  },
  signatoryAttachmentDescription: function(attachment) {
    var container = $("<div class='item' />");
    container.append($("<div class='name' />").text(attachment.name()));
    container.append($("<div class='description' />").text(attachment.description()));
    return container;
  },
  signatoryAttachmentFile: function(attachment) {
    var upl = new SignatoryAttachmentUploadView({
      model: attachment,
      el: $("<div/>")
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


    var container = $("<div class='signatoryattachments' />");
    container.append($("<h2/>").text(this.title == undefined ? localization.requestedAttachments : this.title));
    var table = $("<table class='list'/>");
    var tbody = $("<tbody/>");
    table.append(tbody);
    _.each(this.model.currentSignatory().attachments(), function(attachment) {
      var tr = $("<tr/>")
      tr.append($("<td class='desc'>").append(self.signatoryAttachmentDescription(attachment)));
      tr.append($("<td class='file'>").append(self.signatoryAttachmentFile(attachment)));
      tbody.append(tr);
    });

    container.append(table);
    container.append($("<div class='clearfix' />"));

    $(this.el).append(container);

    return this;
  }
});

})(window);
