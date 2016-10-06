var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CompanyBrandingViewModel = require("./companybrandingviewmodel");
var ThemeView = require("../../themes/themeview");
var CompanySettingsView = require("./companybrandingsettingsview");
var Button = require("../../common/button");
var Select = require("../../common/select");
var EmailPreview = require("../../themes/previews/email");
var SigningPreview = require("../../themes/previews/signing");
var ServicePreview = require("../../themes/previews/service");
var $ = require("jquery");
var _ = require("underscore");

module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.object.isRequired,
      onOpenNewThemeModal: React.PropTypes.func.isRequired
    },
    setTheme : function(themeid) {
      var model = this.props.model;
      var companybranding = this.props.model.companybranding();

      if (model.mailThemeMode()) {
        companybranding.setMailTheme(themeid);
      } else if (model.signviewThemeMode()) {
         companybranding.setSignviewTheme(themeid);
      } else if (model.serviceThemeMode()) {
         companybranding.setServiceTheme(themeid);
      }
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready())
        return (<div/>);
      var createNewThemeFunction = function() {
        self.props.onOpenNewThemeModal();
        return true;
      };
      var themeList = model.themeList();
      var availableThemesOptions = [];
      _.each(themeList.list().models, function(t) {
          availableThemesOptions.push({
            name:  model.themeName(t.field("id")),
            onSelect : function() {
              self.setTheme(t.field("id"));
            }
          });
      });
      availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});
      availableThemesOptions.unshift({name: localization.branding.defaultTheme, value:"", selected: true})
      availableThemesOptions.push({
            name: localization.branding.newThemeWithDots,
            onSelect : createNewThemeFunction
      });
      return (
        <div className="companybranding-create-new-theme-panel">
          <div className="theme-edit">
            <div className="theme-edit-panel">
              <div className="theme-choose-theme-section">
                <div className='title text-with-bottom-spacing'>{localization.branding.createCustomThemeTitle}</div>
                <div className='text-with-bottom-spacing'>{localization.branding.createCustomThemeDescription}</div>
                 {/*if*/ (model.themeList().list().models.length > 0 ) &&
                   <div>
                     <div className='text-with-bottom-spacing'>{localization.branding.useExistingOrCreateNewTheme}</div>
                     <Select
                       options={availableThemesOptions}
                       width = {300}
                     />
                   </div>
                 }
                 {/*else*/ (model.themeList().list().models.length == 0 ) &&
                     <Button
                      onClick={createNewThemeFunction}
                      text ={localization.branding.createNewTheme}
                     />
                 }
              </div>
            </div>
            <div className='separator'/>
            <div className='previews'>
              {/*if*/ (model.mailThemeMode() ) &&
                <div className="preview">
                  <div className="preview-title">{localization.branding.themes.emailPreview}</div>
                  <EmailPreview
                    model={model.domainMailTheme()}
                  />
                </div>
              }
              {/*else if*/ (model.signviewThemeMode() ) &&
                <div className="preview">
                  <div className="preview-title">{localization.branding.themes.signviewPreview}</div>
                  <SigningPreview
                    model={model.domainSignviewTheme()}
                  />
                </div>
              }
              {/*else if*/ (model.serviceThemeMode() ) &&
                <div className="preview">
                  <div className="preview-title">{localization.branding.themes.servicePreview}</div>
                  <ServicePreview
                    model={model.domainServiceTheme()}
                  />
                </div>
              }
            </div>
          </div>
        </div>
      );
    }
  });
