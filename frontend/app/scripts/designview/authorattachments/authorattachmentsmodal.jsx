var React = require("react");
var $ = require("jquery");

var AttachmentsDesign = require("./attachmentsdesign");
var DesignViewAttachments = require("./designviewattachments");
var Document = require("../../../js/documents.js").Document;
var FlashMessage = require("../../../js/flashmessages.js");
var Modal = require("../../common/modal");
var Track = require("../../common/track");
var Queue = require("../../../js/queue").Queue;
var Dialog = require("../../common/dialog");
var BackboneMixin = require("../../common/backbone_mixin");

var AuthorAttachmentsModal = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  propTypes: {
    active: React.PropTypes.bool.isRequired,
    document: React.PropTypes.instanceOf(Document).isRequired,
    saveAndFlashMessageIfAlreadySaved: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },

  getBackboneModels: function () {
    if (this.state.loadingQueue) {
      return [this.state.loadingQueue];
    } else {
      return [];
    }
  },

  getInitialState: function () {
    return {
      acceptVisible: true,
      cancelVisible: true,
      extraButtonsVisible: false,
      isSettingAttachments: false
    };
  },

  componentWillMount: function () {
    this._model = null;

    if (this.props.active) {
      this._model = new DesignViewAttachments({
        document: this.props.document
      });
    }
  },

  componentWillReceiveProps: function (nextProps) {
    if (this.props.active != nextProps.active) {
      if (nextProps.active) {
        this._model = new DesignViewAttachments({
          document: this.props.document
        });

        this.setState(this.getInitialState());
      }
    }
  },

  attachmentsToSave: function () {
    var attachments = new Array();

    _.each(this._model.attachments(), function (attachment, i) {
      if (attachment.isServerFile()) {
        attachments.push({
          name: attachment.name() || attachment.originalName(),
          required: attachment.isRequired(),
          add_to_sealed_file: attachment.isAddToSealedFile(),
          file_id: attachment.serverFileId()
        });
      }
    });

    return attachments;
  },

  attachmentsToUpload: function () {
    return _.filter(this._model.attachments(), function (attachment) {
      return attachment.isFile();
    });
  },

  saveAttachments: function () {
    var self = this;
    this._model.clearErrorMessages();
    self.saveServerAttachments(function () {
      self.uploadNewAttachments(function () {
        self.props.document.recall(self.onRecallDocument);
      });
    });
  },

  uploadNewAttachments: function (cont) {
    var attachments = this.attachmentsToUpload();
    if (attachments.length > 0) {
      var queue = new Queue({
        processItem: this.uploadAttachment,
        onEmpty: cont
      });
      this.setState({
        loadingQueue: queue
      });
      queue.pushItems(attachments);
    } else {
      cont();
    }
  },

  uploadAttachment: function (attachment, next) {
    var self = this;
    var model = this._model;

    var submit = this.props.document.setAttachmentsIncrementally();
    submit.add("file", attachment.fileUpload());
    submit.add("attachments", JSON.stringify([{
      name: attachment.name(),
      required: attachment.isRequired(),
      add_to_sealed_file: attachment.isAddToSealedFile(),
      file_param: "file"
    }]));

    submit.sendAjax(
      function (xhr) {
        next();
        self.onUploadAttachmentSuccess(attachment, xhr);
      },
      function (xhr) {
        next();
        self.onUploadAttachmentError(attachment, xhr);
      }
    );
  },

  onUploadAttachmentSuccess: function (attachment, xhr) {
    var document = JSON.parse(xhr.responseText);
    var serverAttachment = _.find(document.author_attachments, function (att) {
      return att.name == attachment.name();
    });
    attachment.setServerFileId(serverAttachment.file_id);
  },

  onUploadAttachmentError: function (attachment, xhr) {
    var errorMsg = null;
    if (xhr.status == 413) {
      errorMsg = localization.authorattachments.tooLargeAttachment;
    } else {
      errorMsg = localization.authorattachments.invalidAttachments;
    }
    attachment.setErrorMessage(errorMsg);
    errorMsg = errorMsg + " (" + attachment.name() + ")";
    new FlashMessage.FlashMessage({type: "error", content: errorMsg});
  },

  saveServerAttachments: function (cont) {
    var self = this;
    var submit = this.props.document.setAttachments();
    var attachments = this.attachmentsToSave();

    this.setState({
      isSettingAttachments: true
    });

    submit.add("attachments", JSON.stringify(attachments));
    submit.sendAjax(
      function () {
        cont();
        self.onSaveAttachmentsSuccess();
      },
      self.onSaveAttachmentsError
    );
  },

  onHide: function () {
    this._model = null;
  },

  onRecallDocument: function () {
    this.props.document.trigger("change");
    this.props.saveAndFlashMessageIfAlreadySaved();
    if (!this._model.hasErrorMessages()) {
      this.props.onClose();
    } else {
      this.forceUpdate();
    }
  },

  onSaveAttachmentsError: function (xhr) {
    var errorMsg = localization.authorattachments.invalidAttachments;
    new FlashMessage.FlashMessage({type: "error", content: errorMsg});
    this.setState({
      isSettingAttachments: false
    });
  },

  onSaveAttachmentsSuccess: function () {
    this.setState({
      isSettingAttachments: false
    });
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

  isLoading: function () {
    var queue = this.state.loadingQueue;
    var uploading = queue ? !queue.isEmpty() : false;
    return uploading || this.state.isSettingAttachments;
  },

  render: function () {
    var uploadingText = undefined;

    if (this.state.loadingQueue && !this.state.loadingQueue.isEmpty()) {
      var textSpan = $("<span/>").html(localization.authorattachments.uploadingCounter);
      var currentItem = this.state.loadingQueue.currentItem();
      $(".filename", textSpan).text(currentItem ? currentItem.name() : "");
      $(".count", textSpan).text(this.state.loadingQueue.processed() + 1);
      $(".total", textSpan).text(this.state.loadingQueue.total());
      uploadingText = textSpan.text();
    }

    return (
      <div>
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

        {/* if */ this.isLoading() &&
          <Dialog.Dialog active={true}>
            <Dialog.Content>
              <img style={{margin: "30px"}}
                   src={window.cdnbaseurl + "/img/wait30trans.gif"} />
              {/* if */ uploadingText &&
                <p>{uploadingText}</p>
              }
            </Dialog.Content>
          </Dialog.Dialog>
        }
      </div>
    );
  }
});

module.exports = AuthorAttachmentsModal;
