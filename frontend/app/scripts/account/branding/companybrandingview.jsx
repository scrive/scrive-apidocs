var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CompanyBrandingViewModel = require("./companybrandingviewmodel");
var Theme = require("../../themes/theme");
var ThemeView = require("../../themes/themeview");
var InheritedThemeView = require("../../themes/inheritedthemeview");
var CompanySettingsView = require("./companybrandingsettingsview");
var CompanyBrandingNewThemeView = require("./companybrandingnewthemeview");
var CompanyThemesManagementBar = require("./companythemesmanagementbar");
var ReactButton = require("../../common/button");
var EmailPreview = require("../../themes/previews/email");
var SigningPreview = require("../../themes/previews/signing");
var LoginPreview = require("../../themes/previews/login");
var ServicePreview = require("../../themes/previews/service");
var $ = require("jquery");
var Button = require("../../../js/buttons.js").Button;
var _ = require("underscore");
var FlashMessages = require("../../../js/flashmessages.js");
var FlashMessage = FlashMessages.FlashMessage;
var FlashMessageAfterReload = FlashMessages.FlashMessageAfterReload;

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getInitialState: function () {
      return {
        themeToDelete: null
      };
    },
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object,
      onOpenNewThemeModal: React.PropTypes.func.isRequired,
      onThemeDelete: React.PropTypes.func.isRequired,
      onChangeMode: React.PropTypes.func.isRequired
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
             new FlashMessageAfterReload({type : "success", content: localization.branding.saved});
             window.location = urlInsteadOfCallback;
             $(window).unbind("hashchange");
             window.location.reload(true);
           } else {
             new FlashMessageAfterReload({type : "success", content: localization.branding.saved});
             window.location.reload(true);
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
      } else {
        var onSave = function(closeModal) {
          self.save(
            function(afterSave) {
              switchModeFunction();
              closeModal();
              // afterSave(); // Should this be removed?
            },
            newModeUrl
          );
        };

        var onDiscard = function(closeModal) {
            model.reload();
            switchModeFunction();
            closeModal();
        };

        this.props.onChangeMode(onSave, onDiscard);
      }
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      if (!model.ready())
        return (<div/>);
      else {
      var brandingIsInherited = model.companybranding().brandingIsInherited();
      var inheritedTheme = function(id) {
        var theme = _.find( model.inheritableThemesList().list().models
                            , function(et) { return et.field("id") == id; });
        return new Theme({listobject: theme});
      };

      return (
        <div className="tab-container">
          <div className="tab-content">
            <div className="tab-viewer inner">
              { /*if*/ brandingIsInherited && (
                <div className="companybranding-explanation">
                  {localization.branding.isInheritedExplanation}
                </div>)
              }
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
                    <CompanyThemesManagementBar
                      model={model}
                      onSave={function() {self.save();}}
                      onOpenNewThemeModal={this.props.onOpenNewThemeModal}
                    />
                    {/*if*/ (brandingIsInherited && model.mailThemeMode()) &&
                      <InheritedThemeView
                        model={ inheritedTheme(model.companybranding().mailTheme())
                          || model.domainMailTheme()}
                        preview={EmailPreview}
                      />
                    }
                    {/*else if*/ (brandingIsInherited && model.signviewThemeMode()) &&
                      <InheritedThemeView
                        model={inheritedTheme(model.companybranding().signviewTheme())
                          || model.domainSignviewTheme()}
                        preview={SigningPreview}
                      />
                    }
                    {/*else if*/ (brandingIsInherited && model.serviceThemeMode()) &&
                      <InheritedThemeView
                        model={inheritedTheme(model.companybranding().serviceTheme())
                          || model.domainServiceTheme()}
                        preview={ServicePreview}
                      />
                    }
                    {/*else if*/ (!brandingIsInherited && model.mailThemeMode() && model.mailThemeForEditing() != undefined) &&
                      <ThemeView
                        model={model.mailThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.props.onThemeDelete(model.mailThemeForEditing())}}
                        onSave={function() {self.save();}}
                        previews={_.compact([
                            EmailPreview,
                            (model.mailThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                            (model.mailThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                          ])
                        }
                      />
                    }
                    {/*else if*/ (!brandingIsInherited && model.mailThemeMode() && model.mailThemeForEditing() == undefined) &&
                      <CompanyBrandingNewThemeView
                        model={model}
                        onOpenNewThemeModal={this.props.onOpenNewThemeModal}
                      />
                    }
                    {/*else if*/ (!brandingIsInherited && model.signviewThemeMode() && model.signviewThemeForEditing() != undefined) &&
                      <ThemeView
                        model={model.signviewThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.props.onThemeDelete(model.signviewThemeForEditing())}}
                        onSave={function() {self.save();}}
                        previews={_.compact([
                            SigningPreview,
                            (model.signviewThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                            (model.signviewThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                          ])
                        }
                      />
                    }
                    {/*else*/ (!brandingIsInherited && model.signviewThemeMode() && model.signviewThemeForEditing() == undefined) &&
                      <CompanyBrandingNewThemeView
                        model={model}
                        onOpenNewThemeModal={this.props.onOpenNewThemeModal}
                      />
                    }
                    {/*else if*/ (!brandingIsInherited && model.serviceThemeMode() && model.serviceThemeForEditing() != undefined) &&
                      <ThemeView
                        model={model.serviceThemeForEditing()}
                        getDefaultName={function() {return model.newThemeDefaultName()}}
                        onDelete={function() {self.props.onThemeDelete(model.serviceThemeForEditing())}}
                        onSave={function() {self.save();}}
                        previews={_.compact([
                            ServicePreview,
                            (model.serviceThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                            (model.serviceThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined
                          ])
                        }
                      />
                    }
                    {/*else*/ (!brandingIsInherited && model.serviceThemeMode() && model.serviceThemeForEditing() == undefined) &&
                      <CompanyBrandingNewThemeView
                        model={model}
                        onOpenNewThemeModal={this.props.onOpenNewThemeModal}
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
    }
  });
