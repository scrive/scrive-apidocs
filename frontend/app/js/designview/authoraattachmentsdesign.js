var React = require("react");
var DesignAuthorAttachments = require("../../scripts/designview/authorattachments/designviewattachments");
var AttachmentsDesign = require("../../scripts/designview/authorattachments/attachmentsdesign");
var $ = require("jquery");
var Button = require("../buttons.js").Button;
var _ = require("underscore");
var LoadingDialog = require("../loading.js").LoadingDialog;
var FlashMessage = require("../flashmessages.js").FlashMessage;
var Confirmation = require("../confirmations.js").Confirmation;

/* This is component for uploding author attachments (with upload and for server attachments).
 */

  var DesignAuthorAttachmentsPopup = exports.DesignAuthorAttachmentsPopup = function(args) {
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
          var attachments = _.map(model.attachments(), function(att,i) {
            if (!att.isServerFile()) {
              att.fileUpload().attr("name", "attachment_" + i)
              submit.addInputs(att.fileUpload());
            }
            return {
              name : att.name() || att.originalName(),
              required: att.isRequired(),
              file_id: att.isServerFile() ? att.serverFileId() : undefined,
              file_param: att.isServerFile() ? undefined : att.fileUpload().attr("name")
            };
          });

          submit.add("attachments", JSON.stringify(attachments));
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

