var React = require("react");
var Button = require("../../common/button");
var Select = require("../../common/select");
var InfoTextInput = require("../../../js/infotextinputs.js").InfoTextInput;
var $ = require("jquery");
var Submit = require("../../../js/submits.js").Submit;
var _ = require("underscore");




module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.object,
      onOpenNewThemeModal: React.PropTypes.func.isRequired,
      onSave: React.PropTypes.func
    },
    opendNewThemeModal : function() {
      this.props.onOpenNewThemeModal();
    },
    themeSelector : function(getTheme,setTheme,newThemeUrl) {
      var self = this;
      var model = self.props.model;
      var themeList = model.themeList();
      var selectedThemeID = getTheme();
      var availableThemesOptions = [];

      if (model.companybranding().brandingIsInherited()) {
        availableThemesOptions.push({
          name: "Inherited",
          selected: true,
          onSelect : function() {}
        });
      } else {
        _.each(themeList.list().models, function(t) {
          availableThemesOptions.push({
            name:  model.themeName(t.field("id")),
            selected: selectedThemeID == t.field("id"),
            onSelect : function() {
              setTheme(t.field("id"));
            }
          });
        });

        availableThemesOptions = _.sortBy(availableThemesOptions,function(o) {return o.name.toLowerCase();});
        availableThemesOptions.unshift({
          name: localization.branding.defaultTheme,
          value:"",
          selected:selectedThemeID == undefined,
          onSelect : function() {
            setTheme(undefined);
          }
        })

        availableThemesOptions.push({
              name: localization.branding.newThemeWithDots,
              onSelect : function() {
                self.opendNewThemeModal();
              }
        });
      }

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
      var companybranding = this.props.model.companybranding();
      return (
        <div className="companybranding-top-bar">

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
          </div>

          <div className="select-theme-for-view-selects">
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

          {!model.companybranding().brandingIsInherited() &&
            <div className="select-theme-save float-right">
              <Button
                text={localization.branding.save}
                type="action"
                className="save"
                onClick={this.props.onSave}
              />
            </div>
          }

        </div>
      );
    }
  });
