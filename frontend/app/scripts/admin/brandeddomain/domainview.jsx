/** @jsx React.DOM */

define(['React','common/backbone_mixin','admin/brandeddomain/domainviewmodel','themes/themeview','admin/brandeddomain/domainsettingsview'  ,'legacy_code','common/button','common/select',"themes/previews/email", "themes/previews/signing", "themes/previews/login", "themes/previews/service"], function(React, BackboneMixin, DomainViewModel,ThemeView,DomainSettingsView, _Legacy, ReactButton, NewSelect, EmailPreview, SigningPreview, LoginPreview, ServicePreview) {



var ThemeManagementPanelTopBar = React.createClass({
    opendNewThemeModal : function(getTheme,setTheme) {
      var self = this;
      var input = new InfoTextInput({infotext: localization.branding.themes.name ,value: ""});
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
           url: "/adminonly/brandeddomain/newtheme/" + self.props.model.domainid() + "/" + getTheme(),
           name : input.value() || self.props.model.newThemeDefaultName(),
           ajax: true,
           ajaxsuccess: function(resp) {
             self.props.model.reloadThemesList(function() {
               popup.clear();
               setTheme(resp.id);
             });
           }
          }).send();
        }
      });
    },
    themeSelector : function(getTheme,setTheme) {
      var self = this;
      var model = self.props.model;
      var themeList = model.themeList();
      var selectedThemeID = getTheme();
      var availableThemesOptions = [];
      var selectedThemeName = "";
      _.each(themeList.list().models, function(t) {
        if (t.field("id")  != selectedThemeID) {
          availableThemesOptions.push({
            name: model.themeName(t.field("id")),
            onSelect : function() {
              setTheme(t.field("id"));
              return true;
            }
          });
        } else {
          selectedThemeName = model.themeName(t.field("id"));
        }
      });
      availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});
      availableThemesOptions.push({
            name: localization.branding.newThemeWithDots,
            onSelect : function() {
              self.opendNewThemeModal(getTheme,setTheme);
              return true;
            }
      });
      var Select = NewSelect.Select;
      return (
        <Select
          color={"#000000"}
          options={availableThemesOptions}
          name ={selectedThemeName}
          textWidth = {129}
          optionsWidth = "156px"
       />
      );
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var domain = this.props.model.domain();
      return (
        <div className="domain-management-top-bar">

          <div className="select-theme-for-view">
            <h5
              className={"select-theme-header " + (model.mailThemeMode() ? "active" : "")}
              onClick={function() {model.switchToMailThemeMode();}}
            >
              {localization.branding.emailThemeTitle}
            </h5>
            {self.themeSelector(function() {return domain.mailTheme();}, function(t) {domain.setMailTheme(t);model.switchToMailThemeMode();})}
          </div>

          <div className="select-theme-for-view">

            <h5
              className={"select-theme-header " + (model.signviewThemeMode() ? "active" : "")}
              onClick={function() {model.switchToSignviewThemeMode();}}
            >
              {localization.branding.signviewThemeTitle}
            </h5>
            {self.themeSelector(function() {return domain.signviewTheme();}, function(t) {domain.setSignviewTheme(t);model.switchToSignviewThemeMode();})}
          </div>

          <div className="select-theme-for-view">
            <h5
              className={"select-theme-header " + (model.serviceThemeMode() ? "active" : "")}
              onClick={function() {model.switchToServiceThemeMode();}}
            >
              {localization.branding.serviceThemeTitle}
            </h5>
            {self.themeSelector(function() {return domain.serviceTheme();}, function(t) {domain.setServiceTheme(t);model.switchToServiceThemeMode();})}
          </div>

          <div className="select-theme-for-view">
            <h5
                className={"select-theme-header " + (model.loginThemeMode() ? "active" : "")}
                onClick={function() {model.switchToLoginThemeMode();}}
            >
              {localization.branding.loginThemeTitle}
            </h5>
            {self.themeSelector(function() {return domain.loginTheme();}, function(t) {domain.setLoginTheme(t);model.switchToLoginThemeMode();})}
          </div>

          <div className="select-theme-save float-right">
            <ReactButton
              text={localization.branding.save}
              type="action"
              width={60}
              className="save"
              onClick={this.props.onSave}
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

        var content = $("<div>Are you sure that you want to delete this theme. Theme <strong class='put-name-of-theme-here'/> will be used instead</div>");
        content.find(".put-name-of-theme-here").text(alternativeTheme.field("name"));

        var popup = new Confirmation({
          title : localization.branding.deleteTheme,
          content : content,
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
              <ThemeManagementPanelTopBar model={model} onSave={function() {self.save();}}/>
              {/*if*/ (model.mailThemeMode() ) &&
                <ThemeView
                  title={localization.branding.emailThemeTitle}
                  model={model.mailThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.mailThemeForEditing())}}
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
                  title={localization.branding.signviewThemeTitle}
                  model={model.signviewThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.signviewThemeForEditing())}}

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
                  title={localization.branding.serviceThemeTitle}
                  model={model.serviceThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.serviceThemeForEditing())}}

                  previews={_.compact([
                      ServicePreview,
                      (model.serviceThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.serviceThemeForEditing() == model.signviewThemeForEditing()) ? ServicePreview : undefined,
                      (model.serviceThemeForEditing() == model.loginThemeForEditing()) ? LoginPreview : undefined
                    ])
                  }
                />
              }
              {/*else if*/ (model.loginThemeMode() ) &&
                <ThemeView
                  title={localization.branding.loginThemeTitle}
                  model={model.loginThemeForEditing()}
                  onDelete={function() {self.onThemeDelete(model.loginThemeForEditing())}}
                  previews={_.compact([
                      LoginPreview,
                      (model.loginThemeForEditing() == model.mailThemeForEditing()) ? EmailPreview : undefined,
                      (model.loginThemeForEditing() == model.signviewThemeForEditing()) ? SigningPreview : undefined,
                      (model.loginThemeForEditing() == model.serviceThemeForEditing()) ? ServicePreview : undefined
                    ])
                  }
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

