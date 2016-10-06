var React = require("react");
var Button = require("../../common/button");
var Select = require("../../common/select");
var InfoTextInput = require("../../common/infotextinput");
var $ = require("jquery");
var Submit = require("../../../js/submits.js").Submit;
var _ = require("underscore");
var Modal = require("../../common/modal");

var NewThemeModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onClose: React.PropTypes.func.isRequired,
    onAccept: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      name: ""
    };
  },
  onNameChange: function (value) {
    this.setState({name: value});
  },
  onAccept: function () {
    this.props.onAccept(this.state.name);
  },
  onClose: function () {
    this.setState({name: ""});
    this.props.onClose();
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active}>
        <Modal.Header
          title={localization.branding.newTheme}
          showClose={true}
          onClose={this.onClose}
        />
        <Modal.Content>
          <div>
            <div>{localization.branding.enterNameOfThemeBellow}</div>
            <InfoTextInput
              infotext={localization.branding.themes.name}
              value={this.state.name}
              onChange={this.onNameChange}
            />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.onClose} />
          <Modal.AcceptButton
            text={localization.branding.save}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

var DomainThemesManagementBar = React.createClass({
    opendNewThemeModal : function(getTheme,setTheme) {
      this.props.openNewThemeModal(getTheme, setTheme);
    },
    themeSelector : function(getTheme,setTheme) {
      var self = this;
      var model = self.props.model;
      var themeList = model.themeList();
      var selectedThemeID = getTheme();
      var availableThemesOptions = [];
      var selectedThemeName = "";
      _.each(themeList.list().models, function(t) {
        availableThemesOptions.push({
          name: model.themeName(t.field("id")),
          selected: t.field("id")  == selectedThemeID,
          onSelect : function() {
            setTheme(t.field("id"));
          }
        });
      });
      availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});
      availableThemesOptions.push({
            name: localization.branding.newThemeWithDots,
            onSelect : function() {
              self.opendNewThemeModal(getTheme,setTheme);
            }
      });
      return (
        <Select
          options={availableThemesOptions}
          width = {156}
       />
      );
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      var domain = this.props.model.domain();
      return (
        <div className="domain-management-top-bar">

          <div className="select-theme-for-view-labels">
            <h5
              className={"select-theme-header " + (model.mailThemeMode() ? "active" : "")}
              onClick={function() {model.switchToMailThemeMode();}}
            >
              {localization.branding.emailThemeTitle}
            </h5>
            <h5
              className={"select-theme-header " + (model.signviewThemeMode() ? "active" : "")}
              onClick={function() {model.switchToSignviewThemeMode();}}
            >
              {localization.branding.signviewThemeTitle}
            </h5>
            <h5
              className={"select-theme-header " + (model.serviceThemeMode() ? "active" : "")}
              onClick={function() {model.switchToServiceThemeMode();}}
            >
              {localization.branding.serviceThemeTitle}
            </h5>
            <h5
                className={"select-theme-header " + (model.loginThemeMode() ? "active" : "")}
                onClick={function() {model.switchToLoginThemeMode();}}
            >
              {localization.branding.loginThemeTitle}
            </h5>
          </div>

          <div className="select-theme-for-view-selects">
            {self.themeSelector(function() {return domain.mailTheme();}, function(t) {domain.setMailTheme(t);model.switchToMailThemeMode();})}
            {self.themeSelector(function() {return domain.signviewTheme();}, function(t) {domain.setSignviewTheme(t);model.switchToSignviewThemeMode();})}
            {self.themeSelector(function() {return domain.serviceTheme();}, function(t) {domain.setServiceTheme(t);model.switchToServiceThemeMode();})}
            {self.themeSelector(function() {return domain.loginTheme();}, function(t) {domain.setLoginTheme(t);model.switchToLoginThemeMode();})}
          </div>

          <div className="select-theme-save float-right">
            <Button
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

module.exports = React.createClass({
  getInitialState: function () {
    return {
      showNewThemeModal: false
    };
  },
  componentWillMount: function () {
    this._newThemeModalGetTheme = null;
    this._newThemeModalSetTheme = null;
  },
  openNewThemeModal: function (getTheme, setTheme) {
    this.setState({showNewThemeModal: true});
    this._newThemeModalGetTheme = getTheme;
    this._newThemeModalSetTheme = setTheme;
  },
  onNewThemeModalClose: function () {
    self._newThemeModalGetTheme = null;
    self._newThemeModalSetTheme = null;

    this.setState({showNewThemeModal: false});
  },
  onNewThemeModalAccept: function (name) {
    var self = this;
    var theme = this._newThemeModalGetTheme();

    new Submit({
      method: "POST",
      url: "/adminonly/brandeddomain/newtheme/" + self.props.model.domainid() + "/" + theme,
      name: name || self.props.model.newThemeDefaultName(),
      ajax: true,
      ajaxsuccess: function(rs) {
        self.props.model.reloadThemesList(function() {
          self._newThemeModalSetTheme(rs.id);
          self.onNewThemeModalClose();
        });
      }
    }).send();
  },
  render: function () {
    return (
      <div>
        <DomainThemesManagementBar
          model={this.props.model}
          onSave={this.props.onSave}
          openNewThemeModal={this.openNewThemeModal}
        />

        <NewThemeModal
          active={this.state.showNewThemeModal}
          onClose={this.onNewThemeModalClose}
          onAccept={this.onNewThemeModalAccept}
        />
      </div>
    );
  }
});