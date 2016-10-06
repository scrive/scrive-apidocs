var React = require("react");
var DesignAuthorAttachments = require("../../scripts/designview/authorattachments/designviewattachments");
var AttachmentsDesign = require("../../scripts/designview/authorattachments/attachmentsdesign");
var Track = require("../../scripts/common/track");
var $ = require("jquery");
var Button = require("../buttons.js").Button;
var _ = require("underscore");
var LoadingDialog = require("../loading.js").LoadingDialog;
var FlashMessage = require("../flashmessages.js").FlashMessage;
var Confirmation = require("../react_confirmations.js");

/* This is component for uploding author attachments (with upload and for server attachments).
 */

var DesignAuthorAttachmentsPopup = exports.DesignAuthorAttachmentsPopup = function(args) {
  var self = this;
  var document = args.document;
  var saveAndFlashMessageIfAlreadySaved = args.saveAndFlashMessageIfAlreadySaved;

  var onStartShowingList = function() {
    if (self.popup) {
      self.popup.hideCancel();
      self.popup.hideAccept();
      self.popup.showExtraButtons();
    }
  };
  var onStopShowingList = function() {
    if (self.popup) {
      self.popup.showCancel();
      self.popup.showAccept();
      self.popup.hideExtraButtons();
    }
  };

  var model = new DesignAuthorAttachments({ document : document});
  var contentDiv = $("<div/>");
  var view = React.render(
    React.createElement(
      AttachmentsDesign,
      {
        model: model,
        onStartShowingList: onStartShowingList,
        onStopShowingList: onStopShowingList
      }
    ),
    contentDiv[0]
  );

  var backLabel = React.createElement(
    "label",
    {
      className: "close",
      onClick: function (event) {
        event.preventDefault();
        event.stopPropagation();

        view.stopShowingAttachmentList()
      }
    },
    localization.authorattachments.back
  );

  this.popup = new Confirmation({
    title: localization.authorattachments.selectAttachments,
    acceptText: localization.save,
    content: contentDiv,
    width: 740,
    extraButtons: backLabel,
    extraButtonsVisible: false,
    onAccept: function () {
      var uniquelyNamedAttachments = _.uniq(model.attachments(), function(a) {
        return a.name();
      });

      if (model.attachments().length > uniquelyNamedAttachments.length) {
        new FlashMessage({
          type: 'error',
          content: localization.signatoryAttachments.uniqueAttachmentNamesError
        });

        return false;
      }

      Track.track('Save attachments', {documentid:document.documentid()});
      document.afterSave(function() {
        var submit = document.setAttachments();
        var counter = 0;
        var attachments = _.map(model.attachments(), function(att,i) {
          if (!att.isServerFile()) {
            att.fileUpload().attr("name", "attachment_" + i)
            submit.addInputs(att.fileUpload());
          }

          return {
            name: att.name() || att.originalName(),
            required: att.isRequired(),
            add_to_sealed_file: att.isAddToSealedFile(),
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
              saveAndFlashMessageIfAlreadySaved();
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
};

