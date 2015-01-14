/** @jsx React.DOM */

define(['React','legacy_code','common/button','common/select'], function(React, _Legacy, Button, NewSelect) {



return React.createClass({
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
});

