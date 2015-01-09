/** @jsx React.DOM */

define(['React','common/backbone_mixin','account/branding/companybrandingviewmodel','themes/themeview','account/branding/companybrandingsettingsview'  ,'legacy_code','common/button','common/select',"themes/previews/email", "themes/previews/signing", "themes/previews/service"], function(React, BackboneMixin, CompanyBrandingViewModel,ThemeView,CompanySettingsView, _Legacy, Button, NewSelect, EmailPreview, SigningPreview, ServicePreview) {

return React.createClass({
    opendNewThemeModal : function() {
      var self = this;
      var input = new InfoTextInput({infotext: localization.branding.themes.name, value: ""});
      var content = $("<div/>");
      content.append($("<div/>").text(localization.branding.enterNameOfThemeBellow))
             .append(input.el());
      var popup = new Confirmation({
        title: localization.branding.newTheme,
        content : content,
        acceptText : localization.branding.save,
        onAccept : function() {
          new Submit({
           method: "POST",
           url: self.newThemeUrl(),
           name : input.value() || self.props.model.newThemeDefaultName(),
           ajax: true,
           ajaxsuccess: function(resp) {
             self.props.model.reloadThemesList(function() {
               popup.clear();
               self.setTheme(resp.id);
             });
           }
          }).send();
        }
      });
    },
    newThemeUrl : function() {
      var model = this.props.model;
      var companybranding = this.props.model.companybranding();
      if (model.mailThemeMode()) {
        return companybranding.newThemeUrl("mail");
      } else if (model.signviewThemeMode()) {
        return companybranding.newThemeUrl("signview");
      } else if (model.serviceThemeMode()) {
        return companybranding.newThemeUrl("service");
      }
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
    title : function() {
      var model = this.props.model;
      var companybranding = this.props.model.companybranding();
      if (model.mailThemeMode()) {
        return localization.branding.emailThemeTitle;
      } else if (model.signviewThemeMode()) {
        return localization.branding.signviewThemeTitle;
      } else if (model.serviceThemeMode()) {
        return localization.branding.serviceThemeTitle;
      }
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready())
        return (<div/>)

      var themeList = model.themeList();
      var availableThemesOptions = [];
      _.each(themeList.list().models, function(t) {
          availableThemesOptions.push({
            name:  model.themeName(t.field("id")),
            onSelect : function() {
              self.setTheme(t.field("id"));
              return true;
            }
          });
      });
      availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});
      availableThemesOptions.push({
            name: localization.branding.newThemeWithDots,
            onSelect : function() {
              self.opendNewThemeModal();
              return true;
            }
      });
      var Select = NewSelect.Select;

      return (
        <div className="companybranding-create-new-theme-panel">
          <div className="theme-edit">
            <div className="theme-edit-panel">
              <div className="theme-title-section">
                {self.title()}
              </div>
              <div className="theme-choose-theme-section">
                <div className='title text-with-bottom-spacing'>{localization.branding.createCustomThemeTitle}</div>
                <div className='text-with-bottom-spacing'>{localization.branding.createCustomThemeDescription}</div>
                 {/*if*/ (model.themeList().list().models.length > 0 ) &&
                    <div className='text-with-bottom-spacing'>{localization.branding.useExistingOrCreateNewTheme}</div>
                 }
                 {/*else*/ (model.themeList().list().models.length == 0 ) &&
                    <div className='text-with-bottom-spacing'>{localization.branding.createNewTheme}</div>
                 }
                <Select
                  color={"#000000"}
                  options={availableThemesOptions}
                  name ={localization.branding.defaultTheme}
                  textWidth = {273}
                  optionsWidth = "300px"
                />
              </div>
            </div>
            <div className='separator'/>
            <div className='previews'>
              {/*if*/ (model.mailThemeMode() ) &&
                <EmailPreview
                  model={model.domainMailTheme()}
                />
              }
              {/*else if*/ (model.signviewThemeMode() ) &&
                <SigningPreview
                  model={model.domainSignviewTheme()}
                />
              }
              {/*else if*/ (model.serviceThemeMode() ) &&
                <ServicePreview
                  model={model.domainServiceTheme()}
                />
              }
            </div>
          </div>
        </div>

      );
    }
  });
});
