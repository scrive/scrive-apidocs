var React = require("react");
var _ = require("underscore");

var Button = require("../../common/button");
var DesignSignatoryAttachment = require("./designsignatoryattachment");
var Document = require("../../../js/documents.js").Document;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Modal = require("../../common/modal");
var SignatoryAttachment = require(
  "../../../js/signatoryattachment.js"
).SignatoryAttachment;
var SignatoryAttachmentsTable = require("./signatoryattachmentstable");
var Track = require("../../common/track");

var SignatoryAttachmentsModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    document: React.PropTypes.instanceOf(Document).isRequired,
    saveAndFlashMessageIfAlreadySaved: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      attachments: []
    };
  },
  componentWillReceiveProps: function (nextProps) {
    if ((nextProps.active != this.props.active) && nextProps.active) {
      this.refreshAttachments();
    }
  },
  refreshAttachments: function () {
    var newAttachments = [];
    _.each(this.props.document.signatories(), function (signatory) {
      _.each(signatory.attachments(), function (attachment) {
        newAttachments.push(
          new DesignSignatoryAttachment({
            name: attachment.name(),
            description: attachment.description(),
            signatory: signatory,
            isRequired: attachment.isRequired()
          })
        );
      });
    });

    this.setState({attachments: newAttachments});
  },
  onHide: function () {
    this.setState({attachments: []});
  },
  afterRecall: function () {
    this.props.document.trigger("change:attachments");
    this.props.saveAndFlashMessageIfAlreadySaved();
    this.props.onClose();
  },
  afterSave: function () {
    this.props.document.recall(this.afterRecall);
  },
  onAcceptButtonClick: function () {
    var areAllAttachmentsReady = _.all(
      this.state.attachments,
      function (attachment) {
        return attachment.ready();
      }
    );

    if (!areAllAttachmentsReady) {
      new FlashMessage({
        type: "error",
        content: localization.signatoryAttachments.errorFlashMessage
      });

      return false;
    }

    var uniquelyNamedAttachments = _.unique(
      this.state.attachments,
      function (attachment) {
        return (
          attachment.get("signatory").cid + attachment.get("name")
        );
      }
    );

    if (uniquelyNamedAttachments.length != this.state.attachments.length) {
      new FlashMessage({
        type: "error",
        content: localization.signatoryAttachments.uniqueAttachmentNamesError
      });

      return false;
    }

    _.each(this.props.document.signatories(), function (signatory) {
      signatory.clearAttachments();
    });

    _.each(this.state.attachments, function (attachment) {
      attachment.get("signatory").addAttachment(
        new SignatoryAttachment({
          name: attachment.get("name"),
          description: attachment.get("description"),
          required: attachment.get("isRequired")
        })
      );
    });

    this.props.document.save();
    this.props.document.afterSave(this.afterSave);

    return true;
  },
  onAddAttachmentButtonClick: function () {
    Track.track("Click add sig attachment (popup)");

    var newAttachments = _.rest(this.state.attachments, 0);
    newAttachments.push(new DesignSignatoryAttachment());

    this.setState({attachments: newAttachments});
  },
  onRemoveAttachment: function (attachment) {
    Track.track("Remove sig attachment");

    var attachmentIndex = this.state.attachments.indexOf(attachment);
    if (attachmentIndex != -1) {
      var newAttachments = _.rest(this.state.attachments, 0);
      newAttachments.splice(attachmentIndex, 1);

      this.setState({attachments: newAttachments});
    }
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        width={850}
        onHide={this.onHide}
        onShow={this.onShow}
      >
        <Modal.Header
          showClose={true}
          title={localization.signatoryAttachments.requestAttachments}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="modal-subtitle centered">
            {localization.signatoryAttachments.defineRequests}
          </div>

          <div className="designSignatoryAttachmentsPopupContent">
            {(this.state.attachments.length > 0) &&
              <SignatoryAttachmentsTable
                ref="signatoryAttachmentsTable"
                attachments={this.state.attachments}
                signatories={this.props.document.signatories()}
                onRemove={this.onRemoveAttachment}
              />
            }
          </div>

          <div className="modal-buttons centered">
            <Button
              ref="addAttachmentButton"
              size="big"
              text={localization.signatoryAttachments.addAttachment}
              onClick={this.onAddAttachmentButtonClick}
            />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            ref="acceptModalButton"
            text={localization.save}
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = SignatoryAttachmentsModal;
