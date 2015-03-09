/** @jsx React.DOM */

define(['legacy_code', 'React', 'designview/processsettings/signviewsettings', 'designview/processsettings/attachmentslist', 'common/button'], function(_Legacy, React, SignviewSettings,AttachmentsList, Button) {

return React.createClass({
  getInitialState: function() {
      return {settingsModalOpened: this.props['false']};
    },
  render: function() {
    var self = this;
    var document = self.props.model.document();
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
              new DesignAuthorAttachmentsPopup({viewmodel: self.props.model});
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
                new DesignSignatoryAttachmentsPopup({viewmodel: self.props.model});
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


});
