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
    return "/api/frontend/document/" + path[2] + "/signatory/" + path[3] + "/attachment/" + this.model.name() + "/file" + this.model.document().viewer().urlPart();
  },
  removeLink: function() {
    var attachment = this.model;
    var deleteurl = this.apiURL();
    var removelink = $("<a href='' />").text(localization.deletePDF);
    removelink.click(function() {
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
      });
    return removelink;
  },
  uploadButton: function() {
    var attachment = this.model;
    var uploadurl = this.apiURL();
    return UploadButton.init({
      width: 200,
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
            FlashMessages.add({content: localization.fileTooLarge, color: "red"});
          else
            FlashMessages.add({content: localization.couldNotUpload, color: "red"});
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
      var button = Button.init({color: "green", text: localization.reviewPDF, width: 90, size:'small', onClick: function() {
          window.open(model.file().downloadLink(), '_blank');
          }});
      return button;
  },
  render: function() {
      var attachment = this.model;
      var container = $("<div class='upload' />");
      if (attachment.get('loading')) {
          container.append($("<img class='loading'>").attr('src', "/img/wait30trans.gif"));
      } else if (attachment.hasFile()) {
          container.append($("<div class='icon' />"));
          var label = $("<div class='file' />");
          label.append($("<div class='name' />").text(this.model.file().name() + ".pdf"));
          var actions = $("<div />");
          //review button change
          //please delete this line after May 1, 2012
          // -- Eric
          //actions.append($("<div class='action' />").append(this.fileLink()));
          if (!attachment.signatory().hasSigned() && attachment.signatory().document().pending()) {
              actions.append($("<div class='action' />").append(this.removeLink()));
          }
          actions.append($("<div class='clearfix' />"));
          label.append(actions);
          label.append($("<div class='clearfix' />"));
          container.append(label);
          var buttonbox = $('<div class="buttonbox" />');
          buttonbox.append(this.reviewButton().input());
          container.append(buttonbox);

      } else if (attachment.signatory().document().pending() || attachment.signatory().document().currentSignatoryCanSign()){
          container.append(this.uploadButton().input());
      }
      container.append($("<div class='clearfix' />"));

      $(this.el).empty();
      $(this.el).append(container);

      return this;
  }
});

var SignatoryAttachmentView = Backbone.View.extend({
  
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.view = this;
    this.render();
  },
  uploadView: function() {
    return new SignatoryAttachmentUploadView({
      model: this.model,
      el: $("<div />")
    });
  },
  render: function() {
    $(this.el).empty();

    var firstcol = $("<div class='first column'/>");
    firstcol.append($("<div class='name' />").text(this.model.name()));
    firstcol.append($("<div class='description' />").text(this.model.description()));

    var container = $("<div class='item' />");
    container.append(firstcol);
    this.uploadElems = $("<div class='second column'/>").append($(this.uploadView().el));
    container.append(this.uploadElems);

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

})(window);
