var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var CompanyBrandingTextEditor = require("./companybrandingsettingstexteditor");
var CompanyBrandingImageEditor = require("./companybrandingsettingsimageeditor");
var FlashMessageAfterReload = require("../../../js/flashmessages.js").FlashMessageAfterReload;



module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    saveSettings : function() {
      this.props.model.save(function() {
        new FlashMessageAfterReload({type : "success", content: "Saved"});
        window.location.reload(true);
      });
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var faviconAltUrl = window.cdnbaseurl + "/favicon/" + window.brandingdomainid + "/" + window.brandinguserid + "/" + window.brandinghash;
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
                readonly={model.brandingIsInherited()}
              />
              { /* if */ model.forAdminonly() &&
                <CompanyBrandingTextEditor
                  title={localization.branding.companySettings.smsOriginator}
                  description={localization.branding.companySettings.smsOriginatorDescription}
                  getValue={function() {return model.smsOriginator()}}
                  setValue={function(v) {return model.setSmsOriginator(v)}}
                  maxLength={11}
                  readonly={model.brandingIsInherited()}
                />
              }
            </div>
            <div className="companybranding-settings-edit-panel-separator"/>
            <div className="companybranding-settings-edit-panel-separator-bottom-margin"/>
            <div className="companybranding-settings-edit-panel-column right">
              <CompanyBrandingImageEditor
                title={localization.branding.companySettings.favicon}
                description={localization.branding.companySettings.faviconDescription}
                alternativeImage={faviconAltUrl}
                uploadText={localization.branding.companySettings.uploadFavicon}
                getValue={function() {return model.favicon()}}
                setValue={function(v) {return model.setFavicon(v)}}
                readonly={model.brandingIsInherited()}
              />
            </div>
          </div>
          {!model.brandingIsInherited() &&
            <div className='save-button-area'>
              <Button
                type="action"
                text={localization.branding.save}
                className="save"
                onClick={function() {self.saveSettings();}}
              />
            </div>
          }
        </div>
      );
    }
  });
