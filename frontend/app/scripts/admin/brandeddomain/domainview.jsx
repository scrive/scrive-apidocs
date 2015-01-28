/** @jsx React.DOM */

define(['React','common/backbone_mixin','admin/brandeddomain/domainviewmodel','admin/brandeddomain/domainthemesmanagementbar','themes/themeview','admin/brandeddomain/domainsettingsview'  ,'legacy_code','common/button',"themes/previews/email", "themes/previews/signing", "themes/previews/login", "themes/previews/service"], function(React, BackboneMixin, DomainViewModel,ThemeManagementTopBar,ThemeView,DomainSettingsView, _Legacy, ReactButton, EmailPreview, SigningPreview, LoginPreview, ServicePreview) {

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
      if (model.themeList().list().length == 1) {
        new FlashMessage({type: "error", content : "Can't remove last theme for domain"});
        return;
      } else {
        var alternativeTheme = _.find(model.themeList().list().models,function(t) { return t.field("id")  != theme.themeid();});

        var content = $("<div>If you delete the theme <strong class='put-name-of-theme-to-delete-here'/> the <strong class='put-name-of-new-theme-here'/> theme will be used instead.</div>");
        content.find(".put-name-of-theme-to-delete-here").text(theme.name());
        content.find(".put-name-of-new-theme-here").text(alternativeTheme.field("name"));

        var popup = new Confirmation({
          title : localization.branding.deleteTheme,
          content : content,
          acceptText : localization.branding.deleteNow,
          onAccept : function() {
            popup.close();
            LoadingDialog.open();
            model.deleteTheme(theme, function() {
              LoadingDialog.close();
              new FlashMessage({type: "positive", content : localization.branding.themeDeleted});
              model.reload();
            });
          }
        });
      }
    },
    save : function(callback) {
      var model = this.props.model;
      model.mailThemeForEditing().save("/adminonly/brandeddomain/updatetheme/" + model.domainid() + "/" + model.mailThemeForEditing().themeid(),function() {
        model.signviewThemeForEditing().save("/adminonly/brandeddomain/updatetheme/" + model.domainid() + "/" + model.signviewThemeForEditing().themeid(),function() {
          model.serviceThemeForEditing().save("/adminonly/brandeddomain/updatetheme/" + model.domainid() + "/" + model.serviceThemeForEditing().themeid(),function() {
            model.loginThemeForEditing().save("/adminonly/brandeddomain/updatetheme/" + model.domainid() + "/" + model.loginThemeForEditing().themeid(),function() {
              model.domain().save(function() {
                new FlashMessage({type : "success", content: "Saved"});
                if (callback) {
                  callback();
                }
              });
            });
          });
        });
      });
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

        <div className="domain-management">
          { /*if*/ (model.themeMode() ) &&
            <div>
              <ThemeManagementTopBar model={model} onSave={function() {self.save();}}/>
              {/*if*/ (model.mailThemeMode() ) &&
                <ThemeView
                  model={model.mailThemeForEditing()}
                  getDefaultName={function() {return model.newThemeDefaultName()}}
                  onDelete={function() {self.onThemeDelete(model.mailThemeForEditing())}}
                  onSave={function() {self.save();}}
                  previews={_.compact([
                      EmailPreview,
                      (model.mailThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                      (model.mailThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined,
                      (model.mailThemeForEditing() == model.loginThemeForEditing()) ? LoginPreview : undefined
                    ])
                  }
                />
              }
              {/*else if*/ (model.signviewThemeMode() ) &&
                <ThemeView
                  model={model.signviewThemeForEditing()}
                  getDefaultName={function() {return model.newThemeDefaultName()}}
                  onDelete={function() {self.onThemeDelete(model.signviewThemeForEditing())}}
                  onSave={function() {self.save();}}
                  previews={_.compact([
                      SigningPreview,
                      (model.signviewThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.signviewThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined,
                      (model.signviewThemeForEditing() == model.loginThemeForEditing()) ? LoginPreview : undefined
                    ])
                  }
                  />
              }
              {/*else if*/ (model.serviceThemeMode() ) &&
                <ThemeView
                  model={model.serviceThemeForEditing()}
                  getDefaultName={function() {return model.newThemeDefaultName()}}
                  onDelete={function() {self.onThemeDelete(model.serviceThemeForEditing())}}
                  onSave={function() {self.save();}}
                  previews={_.compact([
                      ServicePreview,
                      (model.serviceThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.serviceThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                      (model.serviceThemeForEditing() == model.loginThemeForEditing()) ? LoginPreview : undefined
                    ])
                  }
                />
              }
              {/*else if*/ (model.loginThemeMode() ) &&
                <ThemeView
                  model={model.loginThemeForEditing()}
                  getDefaultName={function() {return model.newThemeDefaultName()}}
                  onDelete={function() {self.onThemeDelete(model.loginThemeForEditing())}}
                  onSave={function() {self.save();}}
                  previews={_.compact([
                      LoginPreview,
                      (model.loginThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.loginThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                      (model.loginThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                    ])
                  }
                />
              }
            </div>
          }
          {/*else if*/ (model.additionalSettingsMode() ) &&
            <DomainSettingsView
              model={model.domain()}
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

