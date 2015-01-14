/** @jsx React.DOM */

define(['React','legacy_code','common/button','common/select'], function(React, _Legacy, Button, NewSelect) {



return React.createClass({
    opendNewThemeModal : function(getTheme,setTheme,newThemeUrl) {
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
           url: newThemeUrl,
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
    themeSelector : function(getTheme,setTheme,newThemeUrl) {
      var self = this;
      var model = self.props.model;
      var themeList = model.themeList();
      var selectedThemeID = getTheme();
      var availableThemesOptions = [];
      var selectedThemeName = localization.branding.defaultTheme;

      _.each(themeList.list().models, function(t) {
        if (t.field("id")  != selectedThemeID) {
          availableThemesOptions.push({
            name:  model.themeName(t.field("id")),
            onSelect : function() {
              setTheme(t.field("id"));
              return true;
            }
          });
        } else {
          selectedThemeName =  model.themeName(t.field("id"));
        }
      });
      availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});

      if (selectedThemeID != undefined) {
        availableThemesOptions.unshift({
            name: localization.branding.defaultTheme,
            onSelect : function() {
              setTheme(undefined);
              return true;
            }
          });
      }

      availableThemesOptions.push({
            name: localization.branding.newThemeWithDots,
            onSelect : function() {
              self.opendNewThemeModal(getTheme,setTheme,newThemeUrl);
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
      var companybranding = this.props.model.companybranding();
      return (
        <div className="companybranding-top-bar">

          <div className="select-theme-for-view">
            <h5
              className={"select-theme-header " + (model.mailThemeMode() ? "active" : "")}
              onClick={function() {model.switchToMailThemeMode();}}
            >
              {localization.branding.emailThemeTitle}
            </h5>
            {
              self.themeSelector(
                function() {return companybranding.mailTheme();},
                function(t) {
                  companybranding.setMailTheme(t);
                  model.switchToMailThemeMode();
                },
                companybranding.newThemeUrl("mail")
              )
            }
          </div>

          <div className="select-theme-for-view">

            <h5
              className={"select-theme-header " + (model.signviewThemeMode() ? "active" : "")}
              onClick={function() {model.switchToSignviewThemeMode();}}
            >
              {localization.branding.signviewThemeTitle}
            </h5>
            {
              self.themeSelector(
                function() {return companybranding.signviewTheme();},
                function(t) {
                  companybranding.setSignviewTheme(t);
                  model.switchToSignviewThemeMode();
                },
                companybranding.newThemeUrl("signview")
              )
            }
          </div>

          <div className="select-theme-for-view">
            <h5
              className={"select-theme-header " + (model.serviceThemeMode() ? "active" : "")}
              onClick={function() {model.switchToServiceThemeMode();}}
            >
             {localization.branding.serviceThemeTitle}
            </h5>
            {
              self.themeSelector(
                function() {return companybranding.serviceTheme();},
                function(t) {
                  companybranding.setServiceTheme(t);
                  model.switchToServiceThemeMode();
                },
                companybranding.newThemeUrl("service")
              )
            }
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
