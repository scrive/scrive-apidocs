var React = require("react");

var AttachmentsDesign = require("./attachmentsdesign");
var DesignViewAuthorAttachments = require("./designviewattachments");
var Document = require("../../../js/documents.js").Document;
var FlashMessage = require("../../../js/flashmessages.js");
var LoadingDialog = require("../../../js/loading.js");
var Modal = require("../../common/modal");
var Track = require("../../common/track");

var AuthorAttachmentsModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    document: React.PropTypes.instanceOf(Document).isRequired,
    saveAndFlashMessageIfAlreadySaved: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      acceptVisible: true,
      cancelVisible: true,
      extraButtonsVisible: false
    };
  },
  componentWillMount: function () {
    this._model = null;

    if (this.props.active) {
      this._model = new DesignViewAuthorAttachments({
        document: this.props.document
      });
    }
  },
  componentWillReceiveProps: function (nextProps) {
    if (this.props.active != nextProps.active) {
      if (nextProps.active) {
        this._model = new DesignViewAuthorAttachments({
          document: this.props.document
        });

        this.setState(this.getInitialState());
      }
    }
  },
  attachmentsToSave: function (submit) {
    return _.map(this._model.attachments(), function (attachment, i) {
      var fileId = attachment.serverFileId();
      var fileParam = undefined;
      if (!attachment.isServerFile()) {
        attachment.fileUpload().attr("name", "attachment_" + i);
        submit.addInputs(attachment.fileUpload());

        fileId = undefined;
        fileParam = attachment.fileUpload().attr("name");
      }

      return {
        name: attachment.name() || attachment.originalName(),
        required: attachment.isRequired(),
        add_to_sealed_file: attachment.isAddToSealedFile(),
        file_id: fileId,
        file_param: fileParam
      };
    });
  },
  saveAttachments: function () {
    var submit = this.props.document.setAttachments();
    var attachments = this.attachmentsToSave(submit);

    submit.add("attachments", JSON.stringify(attachments));
    submit.sendAjax(
      this.onSaveAttachmentsSuccess,
      this.onSaveAttachmentsError
    );

    LoadingDialog.LoadingDialog.open();
  },
  onHide: function () {
    this._model = null;
  },
  onRecallDocument: function () {
    this.props.document.trigger("change");
    LoadingDialog.LoadingDialog.close();
    this.props.saveAndFlashMessageIfAlreadySaved();
    this.props.onClose();
  },
  onSaveAttachmentsError: function (xhr) {
    var errorMsg = null;
    if (xhr.status == 413) {
      if (this._model.attachments().length > 1) {
        errorMsg = localization.authorattachments.tooLargeAttachments;
      } else {
        errorMsg = localization.authorattachments.tooLargeAttachment;
      }
    } else {
      errorMsg = localization.authorattachments.invalidAttachments;
    }

    new FlashMessage.FlashMessage({type: "error", content: errorMsg});
    LoadingDialog.LoadingDialog.close();
  },
  onSaveAttachmentsSuccess: function () {
    this.props.document.recall(this.onRecallDocument);
  },
  onStartShowingList: function () {
    this.setState({
      acceptVisible: false,
      cancelVisible: false,
      extraButtonsVisible: true
    });
  },
  onStopShowingList: function () {
    this.setState({
      acceptVisible: true,
      cancelVisible: true,
      extraButtonsVisible: false
    });
  },
  onAcceptButtonClick: function () {
    var uniquelyNamedAttachments = _.uniq(
      this._model.attachments(),
      function (attachment) {
        return attachment.name();
      }
    );

    if (this._model.attachments().length > uniquelyNamedAttachments.length) {
      new FlashMessage.FlashMessage({
        type: "error",
        content: localization.signatoryAttachments.uniqueAttachmentNamesError
      });

      return false;
    }

    Track.track(
      "Save attachments", {documentid: this.props.document.documentid()}
    );

    this.props.document.afterSave(this.saveAttachments);

    return true;
  },
  onBackButtonClick: function (event) {
    event.preventDefault();
    event.stopPropagation();

    this.refs.contentView.stopShowingAttachmentList();
  },
  onClose: function () {
    this.refs.contentView.stopShowingAttachmentList();
    this.props.onClose();
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        width={740}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.authorattachments.selectAttachments}
          onClose={this.onClose}
        />
        <Modal.Content>
          {this._model &&
            <AttachmentsDesign
              ref="contentView"
              model={this._model}
              onStartShowingList={this.onStartShowingList}
              onStopShowingList={this.onStopShowingList}
            />
          }
        </Modal.Content>
        <Modal.Footer>
          {this.state.cancelVisible &&
            <Modal.CancelButton
              ref="cancelButton"
              onClick={this.props.onClose}
            />
          }
          {this.state.extraButtonsVisible &&
            <Modal.ExtraButtons>
              <label
                ref="backButton"
                className="close"
                onClick={this.onBackButtonClick}
              >
                {localization.authorattachments.back}
              </label>
            </Modal.ExtraButtons>
          }
          {this.state.acceptVisible &&
            <Modal.AcceptButton
              ref="acceptButton"
              text={localization.save}
              onClick={this.onAcceptButtonClick}
            />
          }
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = AuthorAttachmentsModal;
