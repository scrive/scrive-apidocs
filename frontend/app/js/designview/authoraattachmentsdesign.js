/* This is component for uploding author attachments (with upload and for server attachments).
 */
define(['React','designview/attachmentslist','Backbone', 'legacy_code'], function(React,AttachmentsList) {

window.DesignAuthorAttachment = Backbone.Model.extend({
  defaults : {
      name : "",
      serverFileId : undefined,
      fileUpload: undefined
  },
  name: function() {
       return this.get("name");
  },
  isFile : function(){
       return this.get("fileUpload") != undefined;
  },
  isServerFile : function(){
       return this.get("serverFileId") != undefined;
  },
  fileUpload: function() {
       return this.get("fileUpload");
  },
  serverFileId : function(){
       return this.get("serverFileId");
  }
});


  var DesignAuthorAttachments = Backbone.Model.extend({
  defaults : {
      attachments : []
  },
  initialize: function (args) {
      var attachments = new Array();
      _.each(args.document.authorattachments(), function(attachment) {
          attachments.push(new DesignAuthorAttachment({
              serverFileId : attachment.fileid(),
              name: attachment.name()
          }));
        });
      this.set({"attachments":attachments});
    },
  attachments : function(){
       return this.get("attachments");
  },
  addAttachment : function(newAttachment){
        this.attachments().push(newAttachment);
        this.trigger("change:attachments");
  },
  removeAttachment: function(attachment) {
       var newattachments = new Array();
       for(var i=0;i<this.attachments().length;i++)
          if (attachment !== this.attachments()[i])
             newattachments.push(this.attachments()[i]);
       this.set({attachments : newattachments});
       this.trigger("change:attachments");

  },
  isEmpty: function() {
     return this.attachments().length == 0;
  }
});

/*
 */
var DesignAuthorAttachmentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'renderAttachmentsList');
        this.model.bind('change:attachments', this.renderAttachmentsList);
        this.showAvaibleAttachmentsList = false;
        this.model = args.model;
        this.render();
    },
    uploadButton : function() {
        var attachmentsList = this.model;
        var uploadButton = new UploadButton({
                size: 'big',
                text: localization.authorattachments.selectFile,
                width: 'auto',
                maxlength: 2,
                onAppend : function(input,title,multifile) {
                    mixpanel.track('Upload attachment', {'File Title as supplied by browser': title});
                    var name_parts = title.split("\\").reverse()[0].split(".");
                    name_parts.pop(); // drop the extension
                    title = name_parts.join('.');
                    attachmentsList.addAttachment(
                                              new DesignAuthorAttachment({
                                                    name : title,
                                                    fileUpload : $(input)
                                              }));
                },
                onError : function() {}
        });
        return uploadButton.el();
    },
    selectFromTemplateButton : function () {
        var view = this;
        var selectAttachmentButton = new Button({
            size: 'big',
            text: localization.authorattachments.selectAttachment,
            width: 'auto',
            onClick : function() {
                mixpanel.track('Click select attachment');
                view.showAvaibleAttachmentsList = true;
                view.render();
            }
        });
        return selectAttachmentButton.el();
    },
    attachmentButtonsTable : function () {
        var table = $('<table>').append($('<tbody>'));

        var headlineRow = $('<tr>');
        var uploadHeader = $('<div class="header"/>').text(localization.selectFileToUpload);
        var selectHeader = $('<div class="header"/>').text(localization.authorattachments.selectFromScrive);
        headlineRow.append($('<td class="header-td">').append(uploadHeader))
                   .append($('<td class="header-td">').append(selectHeader));

        var buttonsRow = $('<tr>').append($('<td>').append(this.uploadButton()))
                                  .append($('<td>').append(this.selectFromTemplateButton()));

        table.append(headlineRow).append(buttonsRow);

        return $('<div class="attachmentsButtonsTable">').append(table);
    },
    avaibleAttachmentsList : function() {
        var box = $("<div>");
        var attachmentsList = this.model;
        var view = this;

        var arrowBack = $("<div class='back'>");
        arrowBack.click(function() {
            mixpanel.track('Click back (author attachment)');
              view.showAvaibleAttachmentsList = false;
              view.render();
              return false;
        });
        box.append(arrowBack);
        var attachmentTable = $('<div/>');
        React.render(React.createElement(AttachmentsList,{model : attachmentsList}), attachmentTable[0]);
        box.append(attachmentTable);
        return box;
    },
    attachmentList : function() {
        var box = $("<div>");
        var attachmentsList = this.model;
        if (attachmentsList.isEmpty()) return box;
        box.addClass("attachmentsList");
        _.each(attachmentsList.attachments(),function(attachment){
            var attachmentBox = $("<div class='attachmentBox'>");
            attachmentBox.append($("<span/>").text(attachment.name()));
            var removeLink = $("<span class='removeLink'>X</span>");
            removeLink.click(function() {attachmentsList.removeAttachment(attachment); return false;});
            attachmentBox.append(removeLink);
            box.append(attachmentBox);
        });
        return box;
    },
    renderAttachmentsList : function() {
        this.attachmentListBox.empty();
        this.attachmentListBox.append(this.attachmentList());
    },
    render: function () {
        this.container = $(this.el);
        this.container.addClass("selectAuthorAttachmentPopupContent");
        this.container.empty();
        if (!this.showAvaibleAttachmentsList) {
            this.container.append(this.attachmentButtonsTable());
        }
        else
        {
            this.container.append(this.avaibleAttachmentsList());
        }
        this.attachmentListBox = $("<div/>");
        this.container.append(this.attachmentListBox);
        this.renderAttachmentsList();
    return this;
    }
});


window.DesignAuthorAttachmentsPopup = function(args) {
         var viewmodel = args.viewmodel;
         var document = viewmodel.document();
         var model = new DesignAuthorAttachments({ document : document  });
         var view = new DesignAuthorAttachmentsView({model : model, el : $("<div/>")});
         var popup = new Confirmation({
              title  : localization.authorattachments.selectAttachments,
              content: $("<div>").append($("<div class='modal-subtitle centered'>").html(localization.selectFiles)).append($(view.el)),
              acceptText: localization.save,
              width: 740,
              onAccept : function() {
                  mixpanel.track('Save attachments', {documentid:document.documentid()});
                  document.afterSave( function() {
                      var submit = document.setAttachments();
                      var counter = 0;
                      _.each(model.attachments(), function(att){
                          var name = "attachment_" + counter;
                          if (att.isServerFile())
                            submit.add(name, att.serverFileId());
                          else
                            submit.addInputs(att.fileUpload().attr("name", name));
                          counter++;
                      });
                      submit.sendAjax(
                        function() {
                            document.recall(function() {
                                document.trigger("change:attachments");
                                LoadingDialog.close();
                                viewmodel.saveAndFlashMessageIfAlreadySaved();
                                popup.close();
                            });
                        },
                        function(xhr) {
                          var errorMsg;
                          if (xhr.status == 413) {
                            if (model.attachments().length > 1) {
                              errorMsg = localization.authorattachments.tooLargeAttachments;
                            } else {
                              errorMsg = localization.authorattachments.tooLargeAttachment;
                            }
                          } else {
                            errorMsg = localization.authorattachments.invalidAttachments;
                          }
                          new FlashMessage({type: 'error', content: errorMsg});
                          LoadingDialog.close();
                        }
                      );
                      LoadingDialog.open();
                  });
                  return false;
            }
         });
};

});
