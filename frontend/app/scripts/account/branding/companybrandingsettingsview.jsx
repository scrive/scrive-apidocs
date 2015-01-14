/** @jsx React.DOM */

define(["React","common/backbone_mixin","legacy_code","common/button","account/branding/companybrandingsettingstexteditor","account/branding/companybrandingsettingsimageeditor"], function(React, BackboneMixin,_Legacy, Button,CompanyBrandingTextEditor,CompanyBrandingImageEditor) {


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
              <CompanyBrandingTextEditor
                title={localization.branding.companySettings.browserTitle}
                description={localization.branding.companySettings.browserTitleDescription}
                getValue={function() {return model.browserTitle()}}
                setValue={function(v) {return model.setBrowserTitle(v)}}
              />
              <CompanyBrandingTextEditor
                title={localization.branding.companySettings.smsOriginator}
                description={localization.branding.companySettings.smsOriginatorDescription}
                getValue={function() {return model.smsOriginator()}}
                setValue={function(v) {return model.setSmsOriginator(v)}}
                maxLength={11}
              />
            </div>
            <div className="companybranding-settings-edit-panel-column right">
              <CompanyBrandingImageEditor
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
