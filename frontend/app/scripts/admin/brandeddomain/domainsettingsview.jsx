/** @jsx React.DOM */

define(["React","common/backbone_mixin","admin/brandeddomain/domainviewmodel","legacy_code","common/button","admin/brandeddomain/domainimageeditor","admin/brandeddomain/domaintexteditor","admin/brandeddomain/domaincoloreditor"], function(React, BackboneMixin, DomainViewModel,_Legacy, Button, DomainImageEditor,DomainTextEditor,DomainColorEditor) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    saveSettings : function() {
      this.props.model.save(function() {
        new FlashMessage({type : "success", content: "Saved"});
      });
    },
    hideAllColorPickers : function() {
      var self = this;
      self.refs.participant1.hideColorPicker();
      self.refs.participant2.hideColorPicker();
      self.refs.participant3.hideColorPicker();
      self.refs.participant4.hideColorPicker();
      self.refs.participant5.hideColorPicker();
      self.refs.participant6.hideColorPicker();
      self.refs.draftColor.hideColorPicker();
      self.refs.errorColor.hideColorPicker();
      self.refs.initiatedColor.hideColorPicker();
      self.refs.sentColor.hideColorPicker();
      self.refs.deliveredColor.hideColorPicker();
      self.refs.openedColor.hideColorPicker();
      self.refs.reviewedColor.hideColorPicker();
      self.refs.signedColor.hideColorPicker();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className='domain-settings-edit'>
          <div className="domain-settings-edit-panel">
            <div className="domain-settings-edit-panel-title">
              {localization.branding.settingsTitle}
            </div>
            <div className='domain-settings-edit-panel-column left'>
              <DomainTextEditor
                title="URL."
                description="The full web address for the domain."
                getValue={function() {return model.getUrl()}}
                setValue={function(v) {return model.setUrl(v)}}
              />
              <DomainTextEditor
                title="Browser title."
                description="The text at the top of the browser."
                getValue={function() {return model.browserTitle()}}
                setValue={function(v) {return model.setBrowserTitle(v)}}
              />
              <DomainTextEditor
                title="SMS originator."
                description="The name displayed to the recipient when receiving an SMS. Maximum 11 alpha-numeric characters."
                getValue={function() {return model.smsOriginator()}}
                setValue={function(v) {return model.setSmsOriginator(v)}}
                maxLength={11}
              />
              <DomainTextEditor
                title=" Email originator."
                description="The name displayed to the recipient when receiving emails."
                getValue={function() {return model.emailOriginator()}}
                setValue={function(v) {return model.setEmailOriginator(v)}}
              />
              <DomainTextEditor
                title="Contact email."
                description="In places where the user can contact you this is the email address that will be used."
                getValue={function() {return model.contactEmail()}}
                setValue={function(v) {return model.setContactEmail(v)}}
              />
              <DomainTextEditor
                title="No reply email."
                description="This email address will be used as the no-reply address. Setting this address may cause email delivery issues."
                getValue={function() {return model.noreplyEmail()}}
                setValue={function(v) {return model.setNoreplyEmail(v)}}
              />
            </div>
            <div className='domain-settings-edit-panel-column right'>
              <DomainImageEditor
                title="Favicon."
                description="This icon is used in the title of the browser window and when the page is bookmarked. Accepted files are ICO and PNG."
                uploadText="Select favicon"
                getValue={function() {return model.favicon()}}
                setValue={function(v) {return model.setFavicon(v)}}
              />
              <div className="domain-property-editor domain-property-title">
                <strong>
                  Participant colours.
                </strong>
                  &nbsp;These colours will help while designing a document to create a visual connection between the party and the placed fields. The colours should be as different as possible to easily distinguish them from each other. If there are more than 6 parties the colours will be repeated.              </div>
              <DomainColorEditor
                ref="participant1"
                title="Participant 1"
                getValue={function() {return model.participantColor1()}}
                setValue={function(v) {return model.setParticipantColor1(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
              <DomainColorEditor
                ref="participant2"
                title="Participant 2"
                getValue={function() {return model.participantColor2()}}
                setValue={function(v) {return model.setParticipantColor2(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
              <DomainColorEditor
                ref="participant3"
                title="Participant 3"
                getValue={function() {return model.participantColor3()}}
                setValue={function(v) {return model.setParticipantColor3(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="participant4"
                title="Participant 4"
                getValue={function() {return model.participantColor4()}}
                setValue={function(v) {return model.setParticipantColor4(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="participant5"
                title="Participant 5"
                getValue={function() {return model.participantColor5()}}
                setValue={function(v) {return model.setParticipantColor5(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="participant6"
                title="Participant 6"
                getValue={function() {return model.participantColor6()}}
                setValue={function(v) {return model.setParticipantColor6(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <div className="domain-property-editor domain-property-title">
                <strong>
                  Status and event colours.
                </strong>
                 &nbsp;This is the document status indicated with an icon and a colour. Here you can modify the colours.
              </div>

              <DomainColorEditor
                ref="draftColor"
                title="Draft, template"
                icons={["draft","template"]}
                getValue={function() {return model.draftColor()}}
                setValue={function(v) {return model.setDraftColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="errorColor"
                title="Error, withdrawn, cancelled and timed-out"
                icons={["problem","cancelled","rejected","timeouted"]}
                getValue={function() {return model.cancelledColor()}}
                setValue={function(v) {return model.setCancelledColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="initiatedColor"
                title="Initiated"
                icons={["initiated"]}
                getValue={function() {return model.initatedColor()}}
                setValue={function(v) {return model.setInitatedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="sentColor"
                title="Sent"
                icons={["sent"]}
                getValue={function() {return model.sentColor()}}
                setValue={function(v) {return model.setSentColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
            <DomainColorEditor
                ref="deliveredColor"
                title="Delivered"
                icons={["delivered"]}
                getValue={function() {return model.deliveredColor()}}
                setValue={function(v) {return model.setDeliveredColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
            <DomainColorEditor
                ref="openedColor"
                title="Email opened, prolonged"
                icons={["opened","prolonged"]}
                getValue={function() {return model.openedColor()}}
                setValue={function(v) {return model.setOpenedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="reviewedColor"
                title="Reviewed online"
                icons={["read"]}
                getValue={function() {return model.reviewedColor()}}
                setValue={function(v) {return model.setReviewedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}
              />
              <DomainColorEditor
                ref="signedColor"
                title="Signed, sealed"
                icons={["signed","sealed"]}
                getValue={function() {return model.signedColor()}}
                setValue={function(v) {return model.setSignedColor(v)}}
                onColorPickerOpen={function() { self.hideAllColorPickers(); }}

              />
            </div>
          </div>
          <div className='save-button-area'>
            <Button
              type="action"
              text={localization.branding.save}
              className="save"
              onClick={function() {self.saveSettings();}}
            />
          </div>
        </div>
      );
    }
  });
});
