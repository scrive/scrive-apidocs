/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','designview/processsettings/customtexteditor','legacy_code'], function(React, BackboneMixin, CustomTextEditor) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.document];
    },
    render: function() {
      var self = this;
      var doc = this.props.document;
      var emailInvitationMessageEditable = _.any(doc.signatories(), function(s) { return s.emailDelivery() || s.emailMobileDelivery();});
      var emailConfirmationMessageEditable = _.any(doc.signatories(), function(s) { return s.emailConfirmationDelivery() || s.emailMobileConfirmationDelivery();});
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
              disabledPlaceholder = {localization.designview.editMessagePlaceholder}
              onPreview = {function() {
                mixpanel.track('Open invitation preview');
                doc.save();
                doc.afterSave(function() {
                var popup = ConfirmationWithEmail.popup({
                              editText: '',
                              title: localization.designview.customMessage.invitation,
                              mail: doc.inviteMail(),
                              onAccept: function() {
                                popup.close();
                              }
                            });
                });
              }}
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
              disabledPlaceholder = {localization.designview.editMessagePlaceholder}
              onChange = {function(c) {doc.setConfirmationMessage(c);}}
              onPreview = {function() {
                mixpanel.track('Open confirmation preview');
                doc.save();
                doc.afterSave(function() {
                var popup = ConfirmationWithEmail.popup({
                              editText: '',
                              title: localization.designview.customMessage.confirmation,
                              mail: doc.confirmMail(),
                              onAccept: function() {
                                popup.close();
                              }
                            });
                });
              }}
            />
          </div>
        </div>
      );
    }
  }
});


});


