var React = require("react");
var SignviewSettings = require("./signviewsettings");
var AttachmentsList = require("./attachmentslist");
var Button = require("../../common/button");
var DesignAuthorAttachmentsPopup = require("../../../js/designview/authoraattachmentsdesign.js").DesignAuthorAttachmentsPopup;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var DesignSignatoryAttachmentsPopup = require("../../../js/designview/signatoryattachmentsdesignview.js").DesignSignatoryAttachmentsPopup;
var DocumentSaveMixin = require("../document_save_mixin");
var Document = require("../../../js/documents.js").Document;


module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  mixins: [DocumentSaveMixin],
  getInitialState: function() {
    return {settingsModalOpened: false};
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
              if (self.state.settingsModalOpened == true) return; // We want to be sure that we will not open modal twice
              self.setState({modalOpened : true});
              mixpanel.track('Open signview settings');
              new SignviewSettings({
                document: document,
                onClose: function() {
                  self.setState({settingsModalOpened : false});
                }
              });
            }}
          />
        </div>
        <div className="design-view-action-process-left-column-attachments">
          <Button
            text={localization.designview.addRemove}
            className='design-view-action-process-left-column-attachments-author-button'
            onClick= {function() {
              mixpanel.track('Open author attachments');
              document.save();
              new DesignAuthorAttachmentsPopup({
                document: document,
                saveAndFlashMessageIfAlreadySaved: self.saveAndFlashMessageIfAlreadySaved
              });
            }}
          />
          <Button
            text={localization.designview.request}
            className="design-view-action-process-left-column-attachments-signatory-button"
            onClick= {function() {
              if(document.signatoriesWhoSign().length == 0 || document.authorIsOnlySignatory()) {
                mixpanel.track('Open signatory attachments but not enough participants');
                new FlashMessage({ type: 'error' , content: localization.designview.validation.requestAttachmentFlashMessage});
              } else {
                mixpanel.track('Open signatory attachments');
                document.save();
                new DesignSignatoryAttachmentsPopup({
                  document: document,
                  saveAndFlashMessageIfAlreadySaved: self.saveAndFlashMessageIfAlreadySaved
                });
              }
            }}
          />
          <AttachmentsList
            document={document}
          />
        </div>
      </div>
    );
  }
});
