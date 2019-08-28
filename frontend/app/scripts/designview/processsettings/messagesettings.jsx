var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Track = require("../../common/track");
var CustomTextEditor = require("./customtexteditor");
var _ = require("underscore");
var EmailModal = require("../../common/email_modal");

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.document];
    },
    getInitialState: function () {
      return {
        showConfirmationPreviewModal: false,
        showInvitationPreviewModal: false
      };
    },
    onShowConfirmationPreviewModal: function () {
      Track.track('Open confirmation preview');
      this.props.document.save();

      var self = this;
      this.props.document.afterSave(function() {
        self.setState({showConfirmationPreviewModal: true});
      });
    },
    onConfirmationPreviewModalClose: function () {
      this.setState({showConfirmationPreviewModal: false});
    },
    onShowInvitationPreviewModal: function () {
      Track.track('Open invitation preview');
      this.props.document.save();

      var self = this;
      this.props.document.afterSave(function() {
        self.setState({showInvitationPreviewModal: true});
      });
    },
    onInvitationPreviewModalClose: function () {
      this.setState({showInvitationPreviewModal: false});
    },
    render: function() {
      var self = this;
      var doc = this.props.document;
      var emailInvitationMessageEditable = _.any(doc.signatories(), function(s) {
        return !s.author() && (s.emailDelivery() || s.emailMobileDelivery());
      });
      var emailConfirmationMessageEditable = _.any(doc.signatories(), function(s) {
        return !s.author() && (s.anyEmailConfirmationDelivery() || s.anyEmailMobileConfirmationDelivery());
      });

      if (!doc.ready()) {
        return <div/>;
      } else {
      return (
        <div>
          <div>
            <CustomTextEditor
              id = 'design-view-action-process-right-column-invitation-editor'
              customtext = {doc.invitationmessage()}
              editable = {emailInvitationMessageEditable}
              label = {localization.designview.customMessage.invitation}
              previewLabel = {localization.designview.customMessage.preview}
              onChange = {function(c) {doc.setInvitationMessage(c);}}
              placeholder = {localization.designview.editInvitation}
              disabledPlaceholder = {localization.designview.editInvitationMessagePlaceholder}
              onPreview = {self.onShowInvitationPreviewModal}
            />

            <EmailModal.EmailModal
              active={this.state.showInvitationPreviewModal}
              allowReject={false}
              allowEdit={false}
              document={doc}
              title={localization.designview.customMessage.invitation}
              type="invite"
              width={800}
              onAccept={this.onInvitationPreviewModalClose}
              onClose={this.onInvitationPreviewModalClose}
            />
          </div>
          <div style={{ "marginTop" :"15px" }}>
            <CustomTextEditor
              id = 'design-view-action-process-right-column-confirmation-editor'
              customtext = {doc.confirmationmessage()}
              editable = {emailConfirmationMessageEditable}
              label = {localization.designview.customMessage.confirmation}
              previewLabel = {localization.designview.customMessage.preview}
              placeholder  = {localization.designview.editConfirmation}
              disabledPlaceholder = {localization.designview.editConfirmationMessagePlaceholder}
              onChange = {function(c) {doc.setConfirmationMessage(c);}}
              onPreview = {self.onShowConfirmationPreviewModal}
            />

            <EmailModal.EmailModal
              active={this.state.showConfirmationPreviewModal}
              allowReject={false}
              allowEdit={false}
              document={doc}
              title={localization.designview.customMessage.confirmation}
              type="confirm"
              width={800}
              onAccept={this.onConfirmationPreviewModalClose}
              onClose={this.onConfirmationPreviewModalClose}
            />
          </div>
        </div>
      );
    }
  }
});
