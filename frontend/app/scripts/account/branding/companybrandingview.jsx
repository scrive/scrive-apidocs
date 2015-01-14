/** @jsx React.DOM */

define(['React','common/backbone_mixin','account/branding/companybrandingviewmodel','themes/themeview','account/branding/companybrandingsettingsview','account/branding/companybrandingnewthemeview','account/branding/companythemesmanagementbar'   ,'legacy_code','common/button',"themes/previews/email", "themes/previews/signing", "themes/previews/login", "themes/previews/service"], function(React, BackboneMixin, CompanyBrandingViewModel,ThemeView,CompanySettingsView,CompanyBrandingNewThemeView,CompanyThemesManagementBar, _Legacy, ReactButton, EmailPreview, SigningPreview, LoginPreview, ServicePreview) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    onThemeDelete : function(theme) {
      var model = this.props.model;
      var content = $("<div/>").html(localization.branding.doYouWantToDeleteTheme);
      content.find('.put-theme-name-here').text(theme.name());
      var popup = new Confirmation({
        title : localization.branding.deleteTheme,
        content : content,
        onAccept : function() {
          popup.close();
          LoadingDialog.open();
          model.deleteTheme(theme, function() {
            LoadingDialog.close();
            new FlashMessage({type: "success", content : localization.branding.themeDeleted});
            model.reload();
          });
        }
      });
    },
    save : function(callback) {
      var self = this;
      var model = this.props.model;
      var companybranding = model.companybranding();

      var saveCompanyBranding = function() {
       model.companybranding().save(function() {
         new FlashMessage({type : "success", content: localization.branding.saved});
         if (callback) {
           callback();
         }
       });
      };
      var saveServiceThemeAndCompanyBranding = saveCompanyBranding;
      if (model.serviceThemeForEditing() != undefined) {
        saveServiceThemeAndCompanyBranding = function() {
           model.serviceThemeForEditing().save(companybranding.updateThemeUrl(model.serviceThemeForEditing().themeid()), saveCompanyBranding);
        };
      }

      var saveSignviewAndServiceThemeAndCompanyBranding = saveServiceThemeAndCompanyBranding;
      if (model.signviewThemeForEditing() != undefined) {
        saveSignviewAndServiceThemeAndCompanyBranding = function() {
           model.signviewThemeForEditing().save(companybranding.updateThemeUrl(model.signviewThemeForEditing().themeid()), saveServiceThemeAndCompanyBranding);
        };
      }

      var saveAllThemesAndCompanyBranding = saveSignviewAndServiceThemeAndCompanyBranding;
      if (model.mailThemeForEditing() != undefined) {
        saveAllThemesAndCompanyBranding = function() {
           model.mailThemeForEditing().save(companybranding.updateThemeUrl(model.mailThemeForEditing().themeid()), saveSignviewAndServiceThemeAndCompanyBranding);
        };
      }

      saveAllThemesAndCompanyBranding();
    },
    changeMode : function(switchModeFunction) {
      var self = this;
      var model = self.props.model;
      if (!model.dirty()) {
        switchModeFunction();
      }
      else {
        var popup;
        var onSave = function() {
          self.save(function() {
            switchModeFunction();
            popup.close();
          });
        };
        var onDiscard = function() {
            model.reload();
            switchModeFunction();
            popup.close();
        };
        var acceptButton = new Button({type: "action", text: localization.branding.saveNow, style: "margin-left : 20px" , onClick : onSave});
        var discardButton = new Button({text: localization.branding.discard, onClick : onDiscard});
        popup = new Confirmation({
          title : localization.branding.unsavedChangesTitle,
          content : $("<div/>").text(localization.branding.unsavedChangesDescription),
          acceptButton : $("<div/>").append(discardButton.el()).append(acceptButton.el())
        });
      }

    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready())
        return (<div/>);
      return (
        <div className="tab-container">
          <div className="tab-content">
            <div className="tab-viewer inner">
              <div className="tab-viewer-header">
                <ul className="tabs">
                <li
                  className={"float-left " + (model.themeMode() ? "active" : "")}
                   onClick={function() {
                    self.changeMode(
                      function() {
                        model.switchToMailThemeMode();
                      });
                    }
                  }
                >
                  <h4>{localization.branding.themesTitle}</h4>
                </li>
                <li
                  className={"float-left last-tab " + (model.additionalSettingsMode() ? "active" : "")}
                  onClick={function() {
                    self.changeMode(
                      function() {
                        model.switchToAdditionalSettingsMode();
                      });
                    }
                  }
                >
                  <h4>{localization.branding.settingsTitle}</h4>
                </li>
                </ul>
              </div>

        <div className="companybranding">
          { /*if*/ (model.themeMode() ) &&
            <div>
              <CompanyThemesManagementBar model={model} onSave={function() {self.save();}} />
              {/*if*/ (model.mailThemeMode() && model.mailThemeForEditing() != undefined) &&
                <ThemeView
                  title={localization.branding.emailThemeTitle}
                  model={model.mailThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.mailThemeForEditing())}}
                  previews={_.compact([
                      EmailPreview,
                      (model.mailThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                      (model.mailThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                    ])
                  }
                />
              }
              {/*else*/ (model.mailThemeMode() && model.mailThemeForEditing() == undefined) &&
                <CompanyBrandingNewThemeView
                  model={model}
                />
              }
              {/*else if*/ (model.signviewThemeMode() && model.signviewThemeForEditing() != undefined) &&
                <ThemeView
                  title={localization.branding.signviewThemeTitle}
                  model={model.signviewThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.signviewThemeForEditing())}}
                  previews={_.compact([
                      SigningPreview,
                      (model.signviewThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.signviewThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                    ])
                  }
                />
              }
              {/*else*/ (model.signviewThemeMode() && model.signviewThemeForEditing() == undefined) &&
                <CompanyBrandingNewThemeView
                  model={model}
                />
              }
              {/*else if*/ (model.serviceThemeMode() && model.serviceThemeForEditing() != undefined) &&
                <ThemeView
                  title={localization.branding.serviceThemeTitle}
                  model={model.serviceThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.serviceThemeForEditing())}}
                  previews={_.compact([
                      ServicePreview,
                      (model.serviceThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.serviceThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined
                    ])
                  }
                />
              }
              {/*else*/ (model.serviceThemeMode() && model.serviceThemeForEditing() == undefined) &&
                <CompanyBrandingNewThemeView
                  model={model}
                />
              }
              <div className='save-button-area'>
                <ReactButton
                  size="small"
                  type="action"
                  text={localization.branding.save}
                  onClick={function() {self.save();}}
                />
              </div>
            </div>
          }
          {/*else if*/ (model.additionalSettingsMode() ) &&
            <CompanySettingsView
              model={model.companybranding()}
            />
           }
        </div>

        </div>
        </div>
        </div>
      );
    }
  });
});
