/* This is component for uploding author attachments (with upload and for server attachments).
 */
(function(window){

  var DesignAuthorAttachment = Backbone.Model.extend({
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
    uploadButtonBox : function() {
        var view = this;
        var attachmentsList = this.model;
        var box = $("<div class='option-box'>");
        var header = $("<div class='header'/>").text(localization.selectFileToUpload);
        this.uploadButton = new UploadButton({
                color : "black",
                size: 'big',
                shape: 'rounded',
                text: localization.authorattachments.selectFile,
                width: 256,
                maxlength: 2,
                onAppend : function(input,title,multifile) {
                    mixpanel.track('Upload attachment');
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
        box.append(header);
        box.append($("<div class='buttonbox'/>").append(this.uploadButton.el()));
        return box;
    },
    selectFromTemplateButtonBox : function() {
        var view = this;
        var attachmentsList = this.model;
        var box = $("<div class='option-box'>");
        var header = $("<div class='header'/>").text(localization.authorattachments.selectFromScrive);
        var selectAttachmentButton = new Button({
            color : "black",
            shape: 'rounded',
            size: 'big',
            text: localization.authorattachments.selectAttachment,
            width: 256,
            onClick : function() {
                mixpanel.track('Click select attachment');
                view.showAvaibleAttachmentsList = true;
                view.render();
            }
          });
        box.append(header);
        box.append($("<div class='buttonbox'/>").append(selectAttachmentButton.el()));
        return box;
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

        var documentsTable = new KontraList({
                name : "Attachments table",
                schema: new Schema({
                    url: "/a",
                    extraParams : { domain : "All" },
                    cells : [
                        new Cell({name: localization.authorattachments.selectAttachment,
                                  width:"400px",
                                  field:"title",
                                  rendering : function(title, _mainrow, listobject) {
                                      var link = jQuery("<a/>").text(title);
                                      var attachment_file = listobject.field("file");
                                      link.click(function(){
                                          mixpanel.track('Select attachment from list');
                                          attachmentsList.addAttachment(
                                              new DesignAuthorAttachment({
                                                    name : title,
                                                    serverFileId : attachment_file
                                                }));
                                          return false;
                                      });
                                      return link;
                                  }
                                 })
                    ]
                })
            });
        box.append(documentsTable.el());
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
            var c1  = $("<td/>");
            c1.append(this.uploadButtonBox());

            var c2  = $("<td/>");
            c2.append(this.selectFromTemplateButtonBox());

            var table = $("<table/>").append($("<tbody/>").append($("<tr>").append(c1).append(c2)));
            this.container.append(table);
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
         var document = args.document;
         var model = new DesignAuthorAttachments({ document : document  });
         var view = new DesignAuthorAttachmentsView({model : model, el : $("<div/>")});
         var popup = new Confirmation({
              content  : $(view.el),
              title  : localization.authorattachments.selectAttachments,
              subtitle : localization.selectFiles,
              icon : '/img/modal-icons/attachments.png',
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
                                popup.close();
                            });
                        },
                        function() {
                            new FlashMessage({
                                color: "red",
                                content: localization.authorattachments.invalidAttachments
                            });
                                LoadingDialog.close();
                        }
                      );
                      LoadingDialog.open();
                  });
                  return false;
            }
         });
};


})(window);
