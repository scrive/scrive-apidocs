/* This is component for uploding author attachments (with upload and for server attachments).
 * Second part is a component for designing an signatory attachment
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
          }))
        });
      this.set({"attachments":attachments})
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
        var subheader = $("<div class='sheader'/>").text(localization.onlyPDF);
        this.uploadButton = UploadButton.init({
                color : "green",
                size: "small",
                text: localization.attachments.selectFile,
                width: 200,
                maxlength: 2,
                onAppend : function(input,title,multifile) {
                    attachmentsList.addAttachment(
                                              new DesignAuthorAttachment({
                                                    name : title,
                                                    fileUpload : $(input)
                                              }));
                },
                onError : function() {}
            });
        box.append(header);
        box.append(subheader);
        box.append($("<div class='buttonbox'/>").append(this.uploadButton.input()));
        return box;
    },
    selectFromTemplateButtonBox : function() {
        var view = this;
        var attachmentsList = this.model;
        var box = $("<div class='option-box'>");
        var header = $("<div class='header'/>").text(localization.attachments.selectAttachment);
        var subheader = $("<div class='sheader'/>").text(localization.attachments.storedInScrive);
        var selectAttachmentButton = Button.init({
            color : "green",
            size: "small",
            text: localization.attachments.selectAttachment,
            width: 200,
            onClick : function() {
                view.showAvaibleAttachmentsList = true;
                view.render();
            }
          });
        box.append(header);
        box.append(subheader);
        box.append($("<div class='buttonbox'/>").append(selectAttachmentButton.input()));
        return box;
    },
    avaibleAttachmentsList : function() {
        var box = $("<div>");
        var attachmentsList = this.model;
        var view = this;

        var arrowBack = $("<div class='back'>");
        arrowBack.click(function() {
              view.showAvaibleAttachmentsList = false;
              view.render();
              return false;
        })
        box.append(arrowBack);
        
        var documentsTable = KontraList().init({
                name : "Templates table",
                schema: new Schema({
                    url: "/docs",
                    extraParams : { documentType : "Attachment" },
                    cells : [
                        new Cell({name: localization.attachments.selectAttachment,
                                  width:"400px",
                                  field:"title",
                                  special: "rendered",
                                  rendering : function(title, _mainrow, listobject) {
                                      var link = jQuery("<a/>").text(title);
                                      var attachment_file = listobject.field("file");
                                      link.click(function(){
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
        documentsTable.view.render();
        box.append($(documentsTable.view.el));
        return box;
    },
    attachmentList : function() {
        var box = $("<div>");
        var attachmentsList = this.model;
        if (attachmentsList.isEmpty()) return box;
        box.addClass("attachmentsList");
        box.append($("<div class='header'/>").text(localization.attachments.selectedAttachments))
        _.each(attachmentsList.attachments(),function(attachment){
            var attachmentBox = $("<div class='attachmentBox'>");
            attachmentBox.append($("<span/>").text(attachment.name()));
            var removeLink = $("<span class='removeLink'>x</span>");
            removeLink.click(function() {attachmentsList.removeAttachment(attachment); return false;})
            attachmentBox.append(removeLink);
            box.append(attachmentBox);
        })
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
            var header = $("<div class='description'/>").text(localization.attachments.selectAttachmentsDescription);
            this.container.append(header);

            var c1  = $("<td/>")
            c1.append(this.uploadButtonBox());

            var c2  = $("<td/>")
            c2.append(this.selectFromTemplateButtonBox());

            var table = $("<table/>").append($("<tbody/>").append($("<tr>").append(c1).append(c2)));
            this.container.append(table);
       }
        else
        {
            this.container.append(this.avaibleAttachmentsList());
        }
        this.attachmentListBox = $("<div/>")
        this.container.append(this.attachmentListBox);
        this.renderAttachmentsList();
    return this;
    }
});


window.DesignAuthorAttachmentsPopup = {
    popup: function(args) {
         var document = args.document;
         var model = new DesignAuthorAttachments({ document : document  });
         var view = new DesignAuthorAttachmentsView({model : model, el : $("<div/>")})
         Confirmation.popup({
              content  : $(view.el),
              title  : localization.attachments.selectAttachments,
              acceptText: localization.attachments.attach,
              width: "800px",
              onAccept : function() {
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

                submit.success(function(){
                  SessionStorage.set(document.documentid(), "step", "3");
                  window.location = window.location;
                });
                LoadingDialog.open();
                submit.send();
                return false;
            }


         });
    }
};


})(window);

// Popup for designing signatory attachments

(function(window){
  // This is separate so we can commit or rolback changes to document.
  var DesignSignatoryAttachment = Backbone.Model.extend({
  defaults : {
      name : "",
      description : "",
      signatory : undefined
  },
  name: function() {
       return this.get("name");
  },
  description : function(){
       return this.get("description");
  },
  signatory : function(){
       return this.get("signatory");
  },
  setSignatory: function(signatory) {
      this.set({signatory : signatory})
  },
  setName : function(name) {
      this.set({name : name})
  },
  setDescription : function(description) {
      this.set({description : description})  
  },
  ready : function() {
     return this.signatory() != undefined && this.name() != "" && this.description() != ""
  }
});


  var DesignSignatoryAttachments = Backbone.Model.extend({
  defaults : {
      attachments : []
  },
  initialize: function (args) {
      this.document = args.document;
      var attachments = new Array();
      _.each(args.document.signatories(), function(signatory) {
          _.each(signatory.attachments(), function(attachment) {
              attachments.push(new DesignSignatoryAttachment({
                name: attachment.name(),
                description : attachment.description(),
                signatory:  signatory                                            
              }))
          })     
      })
      this.set({"attachments":attachments})
  },
  attachments : function(){
       return this.get("attachments");
  },
  addNewAttachment : function(){
        this.attachments().push(new DesignSignatoryAttachment());
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
var DesignSignatoryAttachmentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change:attachments', this.render);
        this.model = args.model;
        this.render();
    },
    addAttachmentButton : function() {
        var attachments = this.model;
        return Button.init({
            size: 'tiny',
            color: 'blue',
            text: localization.signatoryAttachments.addAttachment,
            onClick: function() {
            attachments.addNewAttachment();
            return false;
            }
        }).input()
    },
    attachmentRow : function(attachment) {
        var attachments = this.model;
        var row = $("<tr/>");
        var td1 = $("<td class='editSignatoryAttachmentTDName'>");
        var editName = $("<input type='text' class='editSignatoryAttachmentName'>");
        editName.val(attachment.name());
        editName.change(function() {
            attachment.setName(editName.val());
        })
        td1.append(editName);
        
        var td2 = $("<td class='editSignatoryAttachmentTDDescription'>");
        var editDesc = $("<textarea class='editSignatoryAttachmentDescription'/>");
        editDesc.val(attachment.description());
        editDesc.change(function() {
            attachment.setDescription(editDesc.val());
        })
        td2.append(editDesc);

        var td3 = $("<td class='editSignatoryAttachmentTDSelect'>");
        var selectSignatory = $("<select class='editSignatoryAttachmentSelect'/>");
        if (attachment.signatory()== undefined)
            selectSignatory.append("<option value='' selected=''>");
        
        _.each(attachments.document.signatories(), function(sig)  {
           if (sig.signs() && !sig.author()) {
                var option = $("<option>").text(sig.nameOrEmail() != "" ? sig.nameOrEmail() :  sig.view.name());
                if (attachment.signatory() == sig)
                    option.attr("selected","yes");
                option.data("signatory",sig);
                selectSignatory.append(option);
                } 
            });
        selectSignatory.change(function(){
            var signatory = $("option:selected",selectSignatory).data("signatory")
            attachment.setSignatory(signatory);
        })
        td3.append(selectSignatory);
            
        var td4 = $("<td class='editSignatoryAttachmentTDRemove'>");
        var removeIcon = $("<div class='removeSignatoryAttachmentIcon'>");
        removeIcon.click(function() {
            attachments.removeAttachment(attachment);
        })
        td4.append(removeIcon);
        
        row.append(td1).append(td2).append(td3).append(td4);
        return row;
    },
    render: function () {
        var view = this;
        var attachments = this.model;
        this.container = $(this.el);
        this.container.addClass("designSignatoryAttachmentsPopupContent");
        this.container.empty();
        if (!attachments.isEmpty())
        {
            var table= $("<table class='editSignatoryAttachmentTable'/>");
            var th1 = $("<th class='editSignatoryAttachmentTDName'>").text(localization.signatoryAttachments.attachment);
            var th2 = $("<th class='editSignatoryAttachmentTDDescription'>").text(localization.signatoryAttachments.description);
            var th3 = $("<th class='editSignatoryAttachmentTDSelect'>").text(localization.signatoryAttachments.from);
            var th4 = $("<th class='editSignatoryAttachmentTDRemove'>");
            var thead = $("<thead/>").append(th1).append(th2).append(th3).append(th4);
        
            var tbody = $("<tbody/>")
            _.each(attachments.attachments(), function(a) { tbody.append(view.attachmentRow(a));});

            this.container.append(table.append(thead).append(tbody));
        }
        this.container.append(this.addAttachmentButton());
        return this;
    }
});


window.DesignSignatoryAttachmentsPopup = {
    popup: function(args) {
         var document = args.document;
         var model = new DesignSignatoryAttachments({ document : document  });
         var view = new DesignSignatoryAttachmentsView({model : model, el : $("<div/>")})
         Confirmation.popup({
              content  : $(view.el),
              title  : localization.signatoryAttachments.requestAttachments,
              acceptText: localization.save,
              width: "800px",
              onAccept : function() {
                  if (_.any(model.attachments(), function(a) {return  !a.ready() }))
                      return false;
                  _.each(document.signatories(), function(sig) {
                      sig.clearAttachments();
                  });
                  _.each(model.attachments(), function(att) {
                      att.signatory().addAttachment(new SignatoryAttachment({
                          name : att.name(),
                          description: att.description()
                        }));
                  });
                  document.save().sendAjax();
                  return true;    
            }


         });
    }
};


})(window);
