/* This is component for uploding author attachments (with upload and for server attachments).
 */
define(['React','designview/authorattachments/designviewattachments','designview/authorattachments/attachmentsdesign','Backbone', 'legacy_code'], function(React,DesignAuthorAttachments, AttachmentsDesign) {

  window.DesignAuthorAttachmentsPopup = function(args) {
    var self = this;
    var onStartShowingList = function() {
      if (self.popup) {
          self.popup.hideCancel();
          // TODO - we are changing button to label - with display:block on one and display:none on other.
          // This should be droped with rewrite of modals to react
          self.acceptButton.el().css("display","none");
          self.backLabel.css("display","block");
      }
    };
    var onStopShowingList = function() {
      if (self.popup) {
        self.popup.showCancel();
        // TODO - we are changing button to label - with display:block on one and display:none on other.
        // This should be droped with rewrite of modals to react
        self.acceptButton.el().css("display","block");
        self.backLabel.css("display","none");
      }
    };
    var viewmodel = args.viewmodel;
    var document = viewmodel.document();
    var model = new DesignAuthorAttachments({ document : document});
    var contentDiv = $("<div/>");
    var view = React.render(React.createElement(AttachmentsDesign,{model : model, onStartShowingList : onStartShowingList, onStopShowingList : onStopShowingList}), contentDiv[0]);

    this.acceptButton = new Button({
      type: "action",
      size: "small",
      text: localization.save,
      onClick :function() {
        mixpanel.track('Save attachments', {documentid:document.documentid()});
        document.afterSave( function() {
          var submit = document.setAttachments();
          var counter = 0;
          _.each(model.attachments(), function(att){

              var name = "attachment_" + counter;
              if (att.isServerFile()) {
                submit.add(name, att.serverFileId());
              } else {
                submit.addInputs(att.fileUpload().attr("name", name));
              }
              var detailsName = "attachment_details_" + counter;
              submit.add(detailsName, JSON.stringify({name : att.name() || att.originalName(), required: att.isRequired()}));

              counter++;
          });
          submit.sendAjax(
            function() {
                document.recall(function() {
                    document.trigger("change");
                    LoadingDialog.close();
                    viewmodel.saveAndFlashMessageIfAlreadySaved();
                    self.popup.close();
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

      }
    });
    this.backLabel = $("<label class='close' style='display:none;'/>").text(localization.authorattachments.back);
    this.backLabel.click(function() {
      view.stopShowingAttachmentList();
      return false; // Need to return false here - since else modal will close due to close class
    });

    this.popup = new Confirmation({
        title  : localization.authorattachments.selectAttachments,
        content: contentDiv,
        width: 740,
        acceptButton : this.acceptButton.el().add($("<div/>").append(this.backLabel))
    });
  };

});
