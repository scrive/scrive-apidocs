var React = require("react");
var SignviewSettingsModal = require("./signviewsettings");
var AttachmentsList = require("./attachmentslist");
var Button = require("../../common/button");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var DocumentSaveMixin = require("../document_save_mixin");
var Document = require("../../../js/documents.js").Document;
var Modal = require("../../common/modal");
var SignatoryAttachmentsModal = require(
  "../signatoryattachments/signatoryattachmentsmodal"
);
var AuthorAttachmentsModal = require(
  "../authorattachments/authorattachmentsmodal"
);


module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  mixins: [DocumentSaveMixin],
  getInitialState: function() {
    return {
      authorAttachmentsModalOpened: false,
      settingsModalOpened: false,
      signatoryAttachmentsModalOpened: false
    };
  },
  onAuthorAttchmentsModalCancel: function () {
    this.setState({authorAttachmentsModalOpened: false});
  },
  onSignviewSettingsModalCancel: function () {
    this.setState({settingsModalOpened: false});
  },
  onSignatoryAttachmentsModalCancel: function () {
    this.setState({signatoryAttachmentsModalOpened: false});
  },
  render: function() {
    var self = this;
    var document = self.props.document;
    return (
      <div>
        <div className="design-view-action-process-signview-settings">
          <Button
            text={localization.designview.signviewsettings.button}
            className='design-view-action-process-signview-settings-button'
            onClick= {function() {
              if (!self.state.settingsModalOpened) {
                Track.track('Open signview settings');
                self.setState({settingsModalOpened: true});
              }
            }}
          />
        </div>
        <div className="design-view-action-process-left-column-attachments">
          <Button
            text={localization.designview.addRemove}
            className='design-view-action-process-left-column-attachments-author-button'
            onClick= {function() {
              Track.track('Open author attachments');
              document.save();
              self.setState({authorAttachmentsModalOpened: true});
            }}
          />
          <Button
            text={localization.designview.request}
            className="design-view-action-process-left-column-attachments-signatory-button"
            onClick= {function() {
              if(document.signatoriesWhoSign().length == 0 || document.authorIsOnlySignatory()) {
                Track.track('Open signatory attachments but not enough participants');
                new FlashMessage({ type: 'error' , content: localization.designview.validation.requestAttachmentFlashMessage});
              } else {
                Track.track('Open signatory attachments');
                document.save();
                self.setState({signatoryAttachmentsModalOpened: true});
              }
            }}
          />
          <AttachmentsList
            document={document}
          />

          <SignviewSettingsModal
            active={self.state.settingsModalOpened}
            document={document}
            onClose={self.onSignviewSettingsModalCancel}
          />

          <SignatoryAttachmentsModal
            active={self.state.signatoryAttachmentsModalOpened}
            document={document}
            saveAndFlashMessageIfAlreadySaved={self.saveAndFlashMessageIfAlreadySaved}
            onClose={self.onSignatoryAttachmentsModalCancel}
          />

          <AuthorAttachmentsModal
            active={self.state.authorAttachmentsModalOpened}
            document={document}
            saveAndFlashMessageIfAlreadySaved={self.saveAndFlashMessageIfAlreadySaved}
            onClose={self.onAuthorAttchmentsModalCancel}
          />
        </div>
      </div>
    );
  }
});
