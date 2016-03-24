var React = require("react");
var Button = require("../../common/button");
var Select = require("../../common/select");
var InfoTextInput = require("../../../js/infotextinputs.js").InfoTextInput;
var $ = require("jquery");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var Submit = require("../../../js/submits.js").Submit;
var _ = require("underscore");




module.exports = React.createClass({
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
           ajaxsuccess: function(rs) {
             var resp = JSON.parse(rs);
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
