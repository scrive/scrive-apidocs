var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var DomainViewModel = require("./domainviewmodel");
var ThemeManagementTopBar = require("./domainthemesmanagementbar");
var ThemeView = require("../../themes/themeview");
var DomainSettingsView = require("./domainsettingsview");
var ReactButton = require("../../common/button");
var EmailPreview = require("../../themes/previews/email");
var SigningPreview = require("../../themes/previews/signing");
var LoginPreview = require("../../themes/previews/login");
var ServicePreview = require("../../themes/previews/service");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var _ = require("underscore");
var $ = require("jquery");
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Button = require("../../common/button");

var Modal = require("../../common/modal");

var ThemeDeleteModalContent = React.createClass({
  propTypes: {
    model: React.PropTypes.object.isRequired,
    theme: React.PropTypes.object
  },
  render: function () {
    if (!this.props.theme) {
      return (<div/>);
    }

    var self = this;
    var alternativeTheme = _.find(
      this.props.model.themeList().list().models,
      function (t) {
        return t.field("id") != self.props.theme.themeid();
      }
    );

    return (
      <div>
        If you delete the theme <strong>{this.props.theme.name()}</strong><span> the </span>
        <strong>{alternativeTheme.field("name")}</strong> theme will be used instead.
      </div>
    );
  }
});


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    getInitialState: function () {
      return {
        themeToDelete: null,
        showChangeModeModal: false
      };
    },
    componentWillMount: function () {
      this._switchModeFunction = null;
    },
    onThemeDelete : function(theme) {
      var model = this.props.model;
      if (model.themeList().list().length == 1) {
        new FlashMessage({
          type: "error",
          content: "Can't remove last theme for domain"
        });
      } else {
        this.setState({themeToDelete: theme});
      }
    },
    onThemeDeleteModalClose: function () {
      this.setState({themeToDelete: null});
    },
    onThemeDeleteModalAccept: function () {
      var theme = this.state.themeToDelete;
      this.onThemeDeleteModalClose();

      var self = this;
      this.props.model.deleteTheme(theme, function() {
        LoadingDialog.close();
        new FlashMessage({
          type: "positive",
          content: localization.branding.themeDeleted
        });

        self.props.model.reload();
      });
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
      if (!this.props.model.dirty()) {
        switchModeFunction();
      } else {
        this._switchModeFunction = switchModeFunction;
        this.setState({showChangeModeModal: true});
      }
    },
    onChangeModalClose: function () {
      this.setState({showChangeModeModal: false});
    },
    onChangeModalDiscard: function () {
      this.setState({showChangeModeModal: false});
      this.props.model.reload();
      if (_.isFunction(this._switchModeFunction)) {
        this._switchModeFunction();
        this._switchModeFunction = null;
      }
    },
    onChangeModalSave: function () {
      var self = this;
      this.save(function () {
        self.setState({showChangeModeModal: false});

        if (_.isFunction(self._switchModeFunction)) {
          self._switchModeFunction();
          self._switchModeFunction = null;
        }
      });
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

          <Modal.Container active={(self.state.themeToDelete != null)}>
            <Modal.Header
              title={localization.branding.deleteTheme}
              showClose={true}
              onClose={self.onThemeDeleteModalClose}
            />
            <Modal.Content>
              <ThemeDeleteModalContent
                model={model}
                theme={self.state.themeToDelete}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.onThemeDeleteModalClose} />
              <Modal.AcceptButton onClick={self.onThemeDeleteModalAccept} />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={this.state.showChangeModeModal}>
            <Modal.Header
              title={localization.branding.unsavedChangesTitle}
              showClose={true}
              onClose={self.onChangeModalClose}
            />
            <Modal.Content>
              <div>{localization.branding.unsavedChangesDescription}</div>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onChangeModalClose} />
              <Modal.AcceptButton
                title={localization.branding.saveNow}
                onClick={this.onChangeModalSave}
              />
              <Modal.ExtraButtons>
                <Button
                  text={localization.branding.discard}
                  onClick={this.onChangeModalDiscard}
                />
              </Modal.ExtraButtons>
            </Modal.Footer>
          </Modal.Container>
        </div>

        </div>
        </div>
        </div>
      );
    }
  });
