var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var CompanyBrandingViewModel = require("./companybrandingviewmodel");
var CompanyBrandingView = require("./companybrandingview");
var $ = require("jquery");

var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var InfoTextInput = require("../../common/infotextinput");
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Modal = require("../../common/modal");
var Submit = require("../../../js/submits.js").Submit;
var Button = require("../../common/button");

var NewThemeModalContent = React.createClass({
  propTypes: {
    value: React.PropTypes.string.isRequired,
    onNewThemeNameInputChange: React.PropTypes.func.isRequired
  },
  render: function () {
    return (
      <div>
        <div>{localization.branding.enterNameOfThemeBellow}</div>
        <InfoTextInput
          infotext={localization.branding.themes.name}
          value={this.props.value}
          onChange={this.props.onNewThemeNameInputChange}
        />
      </div>
    );
  }
});

var DeleteThemeModalContent = React.createClass({
  propTypes: {
    name: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <div>
        <HtmlTextWithSubstitution
          secureText={localization.branding.doYouWantToDeleteTheme}
          subs={{".put-theme-name-here": this.props.name}}
        />
      </div>
    );
  }
});

var ChangeModeModalContent = React.createClass({
  render: function () {
    return (
      <div>{localization.branding.unsavedChangesDescription}</div>
    );
  }
});

module.exports = React.createClass({
    propTypes: {
      companyid: React.PropTypes.string
    },
    getInitialState: function() {
      var state = this.stateFromProps(this.props)

      state.showNewThemeModal = false;
      state.newThemeName = "";
      state.themeToDelete = null;
      state.showChangeModeModal = false;

      return state;
    },
    componentWillMount : function() {
      var self = this;
      var updateOnHashChangeFunction = function() {self.adjustToPageHash();};
      $(window).bind('hashchange',updateOnHashChangeFunction);
      this.setState({updateOnHashChangeFunction : updateOnHashChangeFunction});
      self.adjustToPageHash();

      self._onChangeModalSave = null;
      self._onChangeModalDiscard = null;
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    componentWillUnmount : function() {
      if (this.state.updateOnHashChangeFunction) {
        $(window).unbind('hashchange',this.state.updateOnHashChangeFunction);
      }
    },
    stateFromProps : function(props) {
      var model = new CompanyBrandingViewModel({
        companyid: props.companyid
      });
      return {model: model};
    },
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.state.model];
    },
    adjustToPageHash : function() {
      if (location.hash == "#branding-themes-email") {
        this.state.model.switchToMailThemeMode();
      } else if (location.hash == "#branding-themes-signing-page") {
        this.state.model.switchToSignviewThemeMode();
      } else if (location.hash == "#branding-themes-service") {
        this.state.model.switchToServiceThemeMode();
      } else if (location.hash == "#branding-settings") {
        this.state.model.switchToAdditionalSettingsMode();
      }
    },
    reload : function() {
      this.state.model.reload();
    },
    onOpenNewThemeModal: function () {
      this.setState({newThemeName: "", showNewThemeModal: true});
    },
    onNewThemeModalClose: function () {
      this.setState({showNewThemeModal: false});
    },
    onNewThemeModalAccept: function () {
      var self = this;

      new Submit({
        method: "POST",
        url: self.newThemeUrl(),
        name: self.state.newThemeName || self.state.model.newThemeDefaultName(),
        ajax: true,
        ajaxsuccess: function(rs) {
          self.state.model.reloadThemesList(function() {
            self.onNewThemeModalClose();
            self.setTheme(rs.id);
          });
        }
      }).send();
    },
    onNewThemeNameInputChange: function (newThemeName) {
      this.setState({newThemeName: newThemeName});
    },
    newThemeUrl : function() {
      var model = this.state.model;
      var companybranding = this.state.model.companybranding();
      if (model.mailThemeMode()) {
        return companybranding.newThemeUrl("mail");
      } else if (model.signviewThemeMode()) {
        return companybranding.newThemeUrl("signview");
      } else if (model.serviceThemeMode()) {
        return companybranding.newThemeUrl("service");
      }
    },
    setTheme : function(themeid) {
      var model = this.state.model;
      var companybranding = this.state.model.companybranding();

      if (model.mailThemeMode()) {
        companybranding.setMailTheme(themeid);
      } else if (model.signviewThemeMode()) {
         companybranding.setSignviewTheme(themeid);
      } else if (model.serviceThemeMode()) {
         companybranding.setServiceTheme(themeid);
      }
    },
    onThemeDelete : function(theme) {
      this.setState({themeToDelete: theme});
    },
    onThemeDeleteModalClose: function () {
      this.setState({themeToDelete: null});
    },
    onThemeDeleteModalAccept: function () {
      if (this.state.themeToDelete) {
        var model = this.state.model;
        var theme = this.state.themeToDelete;

        this.onThemeDeleteModalClose();
        LoadingDialog.open();

        model.deleteTheme(theme, function() {
          LoadingDialog.close();
          new FlashMessage({
            type: "success",
            content: localization.branding.themeDeleted
          });

          model.reload();
        });
      }
    },
    onOpenChangeModeModal: function (onSave, onDiscard) {
      this._onChangeModalSave = onSave;
      this._onChangeModalDiscard = onDiscard;

      this.setState({showChangeModeModal: true});
    },
    onChangeModeModalClose: function () {
      this._onChangeModalSave = null;
      this._onChangeModalDiscard = null;

      this.setState({showChangeModeModal: false});
    },
    onChangeModeModalDiscard: function () {
      if (this._onChangeModalDiscard) {
        this._onChangeModalDiscard(this.onChangeModeModalClose);
      }
    },
    onChangeModeModalAccept: function () {
      if (this._onChangeModalSave) {
        this._onChangeModalSave(this.onChangeModeModalClose);
      }
    },
    render: function() {
      return (
        <div>
          <CompanyBrandingView
            model={this.state.model}
            onOpenNewThemeModal={this.onOpenNewThemeModal}
            onThemeDelete={this.onThemeDelete}
            onChangeMode={this.onOpenChangeModeModal}
          />

          <Modal.Container active={this.state.showNewThemeModal}>
            <Modal.Header
              title={localization.branding.newTheme}
              onClose={this.onNewThemeModalClose}
            />
            <Modal.Content>
              <NewThemeModalContent
                value={this.state.newThemeName}
                onNewThemeNameInputChange={this.onNewThemeNameInputChange}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onNewThemeModalClose} />
              <Modal.AcceptButton
                text={localization.branding.save}
                onClick={this.onNewThemeModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={(this.state.themeToDelete != null)}>
            <Modal.Header
              title={localization.branding.deleteTheme}
              onClose={this.onThemeDeleteModalClose}
            />
            <Modal.Content>
              <DeleteThemeModalContent
                name={(this.state.themeToDelete) ? this.state.themeToDelete.name() : ""}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onThemeDeleteModalClose} />
              <Modal.AcceptButton
                text={localization.branding.deleteNow}
                onClick={this.onThemeDeleteModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={this.state.showChangeModeModal}>
            <Modal.Header
              title={localization.branding.unsavedChangesTitle}
              showClose={true}
              onClose={this.onChangeModeModalClose}
            />
            <Modal.Content>
              <ChangeModeModalContent />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onChangeModeModalClose} />
              <Modal.AcceptButton
                text={localization.branding.saveNow}
                onClick={this.onChangeModeModalAccept}
              />
              <Modal.ExtraButtons>
                <Button
                  text={localization.branding.discard}
                  onClick={this.onChangeModeModalDiscard}
                />
              </Modal.ExtraButtons>
            </Modal.Footer>
          </Modal.Container>
        </div>
      );
    }
  });
