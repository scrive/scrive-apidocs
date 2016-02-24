var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CompanyBrandingViewModel = require("./companybrandingviewmodel");
var ThemeView = require("../../themes/themeview");
var CompanySettingsView = require("./companybrandingsettingsview");
var CompanyBrandingNewThemeView = require("./companybrandingnewthemeview");
var CompanyThemesManagementBar = require("./companythemesmanagementbar");
var ReactButton = require("../../common/button");
var EmailPreview = require("../../themes/previews/email");
var SigningPreview = require("../../themes/previews/signing");
var LoginPreview = require("../../themes/previews/login");
var ServicePreview = require("../../themes/previews/service");
var $ = require("jquery");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Button = require("../../../js/buttons.js").Button;
var _ = require("underscore");


module.exports = React.createClass({
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
        acceptText : localization.branding.deleteNow,
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
    save : function(callback,urlInsteadOfCallback) {
      var self = this;
      var model = this.props.model;
      var companybranding = model.companybranding();
      var shouldReloadOnSave= model.shouldReloadOnSave();
      var saveCompanyBranding = function() {
       model.companybranding().save(function() {
         if (!shouldReloadOnSave) {
           new FlashMessage({type : "success", content: localization.branding.saved});
           if (callback) {
             callback();
           }
         } else {
           if (urlInsteadOfCallback) {
            new FlashMessage({type : "success", content: localization.branding.saved, withRedirect: true, redirect: urlInsteadOfCallback, hashChangeOnly : true});
           } else {
            new FlashMessage({type : "success", content: localization.branding.saved, withReload: true});
           }
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
    changeMode : function(switchModeFunction, newModeUrl) {
      var self = this;
      var model = self.props.model;
      if (!model.dirty()) {
        switchModeFunction();
      }
      else {
        var popup;
        var onSave = function() {
          self.save(function(afterSave) {
            switchModeFunction();
            popup.close();
            afterSave();
          }, newModeUrl);
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
                    }}
                  >
                    <h4>{localization.branding.themesTitle}</h4>
                  </li>
                  <li
                    className={"float-left last-tab " + (model.additionalSettingsMode() ? "active" : "")}
                    onClick={function() {
                      self.changeMode(
                        function() {
                          model.switchToAdditionalSettingsMode();
                        },
                        model.additonalSettingsUrl()
                      );
                    }}
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
                        model={model.mailThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.onThemeDelete(model.mailThemeForEditing())}}
                        onSave={function() {self.save();}}
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
                        model={model.signviewThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.onThemeDelete(model.signviewThemeForEditing())}}
                        onSave={function() {self.save();}}
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
                        model={model.serviceThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.onThemeDelete(model.serviceThemeForEditing())}}
                        onSave={function() {self.save();}}
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
