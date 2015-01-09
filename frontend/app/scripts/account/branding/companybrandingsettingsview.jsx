/** @jsx React.DOM */

define(["React","common/backbone_mixin","admin/brandeddomain/domainviewmodel","legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, BackboneMixin, DomainViewModel,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {

var CompanyBrandingImagePropertyEditor = React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="companybranding-property-editor companybranding-image-property">
        <div className="companybranding-image-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="companybranding-text-property-edit">
          <img src={this.props.getValue() || this.props.alternativeImage} className="favicon-image"/>
          <UploadImageButton
                text={self.props.uploadText}
                width={160}
                size="tiny"
                onUpload={function(image) {
                  self.props.setValue(image);
                }}
          />
        </div>
      </div>
    );
  }
});

var CompanyBrandingTextPropertyEditor = React.createClass({
  render: function() {
    var self = this;
    return (
      <div className="companybranding-property-editor domain-text-property">
        <div className="companybranding-text-property-title">
          <strong>{self.props.title}</strong> {self.props.description}
        </div>
        <div className="companybranding-text-property-edit">
          <InfoTextInput
            value={self.props.getValue()}
            infotext=""
            onChange={function(v) {self.props.setValue(v)}}
            maxLength={self.props.maxLength}
          />
        </div>
      </div>
    );
  }
});


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
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className="companybranding-settings-edit">
          <div className="companybranding-settings-edit-panel">
            <div className="companybranding-settings-edit-panel-title">
              {localization.branding.settingsTitle}
            </div>
            <div className="companybranding-settings-edit-panel-column left">
              <CompanyBrandingTextPropertyEditor
                title={localization.branding.companySettings.browserTitle}
                description={localization.branding.companySettings.browserTitleDescription}
                getValue={function() {return model.browserTitle()}}
                setValue={function(v) {return model.setBrowserTitle(v)}}
              />
              <CompanyBrandingTextPropertyEditor
                title={localization.branding.companySettings.smsOriginator}
                description={localization.branding.companySettings.smsOriginatorDescription}
                getValue={function() {return model.smsOriginator()}}
                setValue={function(v) {return model.setSmsOriginator(v)}}
                maxLength={11}
              />
            </div>
            <div className="companybranding-settings-edit-panel-column right">
              <CompanyBrandingImagePropertyEditor
                title={localization.branding.companySettings.favicon}
                description={localization.branding.companySettings.faviconDescription}
                alternativeImage={"/favicon/" + window.brandinghash}
                uploadText={localization.branding.companySettings.uploadFavicon}
                getValue={function() {return model.favicon()}}
                setValue={function(v) {return model.setFavicon(v)}}
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