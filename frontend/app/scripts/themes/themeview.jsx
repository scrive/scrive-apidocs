var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var ThemeLogoEditor = require("./themelogoeditor");
var ThemeColorEditor = require("./themecoloreditor");
var ThemeFontEditor = require("./themefonteditor");
var Button = require("../common/button");
var InfoTextInput = require("../common/infotextinput");
var EmailPreview = require("./previews/email");
var SigningPreview = require("./previews/signing");
var LoginPreview = require("./previews/login");
var ServicePreview = require("./previews/service");


module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    hideAllColorPickers : function() {
      var self = this;
      self.refs.brandColors.hideColorPickers();
      self.refs.actionColors.hideColorPickers();
      self.refs.actionSecondaryColors.hideColorPickers();
      self.refs.positiveColors.hideColorPickers();
      self.refs.negativeColors.hideColorPickers();
    },
    previewTitle : function(preview) {
      if (preview == EmailPreview) {
        return localization.branding.themes.emailPreview;
      } else if (preview == SigningPreview) {
        return localization.branding.themes.signviewPreview;
      } else if (preview == ServicePreview) {
        return localization.branding.themes.servicePreview;
      } else if (preview == LoginPreview) {
        return localization.branding.themes.loginPreview;
      }
      return "";
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      console.log("preview");
      return (
        <div className="theme-edit">
          <div className="theme-edit-panel">
              <div className="theme-edit-section">
                <div className='title'>{localization.branding.themes.name}</div>
                  <InfoTextInput
                    value={model.name()}
                    onBlur={function(v) {
                      if (model.name() == "" && self.props.getDefaultName != undefined) {
                        model.setName(self.props.getDefaultName());
                      }
                    }}
                    onChange={function(v) {
                        model.setName(v);
                    }}
                  />
              </div>
              <div className="theme-edit-section">
                <ThemeLogoEditor
                  getBackgroundColor={function() {return model.brandColor();}}
                  getLogo={function() {return model.logo();}}
                  setLogo={function(l) {return model.setLogo(l);}}
                />
              </div>
              <div className="theme-edit-section">
                <ThemeColorEditor
                  ref="brandColors"
                  title={localization.branding.themes.brandColor}
                  sampleText={localization.branding.themes.brandColorSample}
                  getColor={function() {return model.brandColor();}}
                  getTextColor={function() {return model.brandTextColor();}}
                  setColor={function(c) {return model.setBrandColor(c);}}
                  setTextColor={function(c) {return model.setBrandTextColor(c);}}
                  onColorPickerOpen={function() { self.hideAllColorPickers(); }}
                />
              </div>
              <div className="theme-edit-section">
                <ThemeColorEditor
                  ref="actionColors"
                  title={localization.branding.themes.actionColor}
                  sampleText={localization.branding.themes.actionColorSample}
                  getColor={function() {return model.actionColor();}}
                  getTextColor={function() {return model.actionTextColor();}}
                  setColor={function(c) {return model.setActionColor(c);}}
                  setTextColor={function(c) {return model.setActionTextColor(c);}}
                  onColorPickerOpen={function() { self.hideAllColorPickers(); }}

                />
              </div>
              <div className="theme-edit-section">
                <ThemeColorEditor
                  ref="actionSecondaryColors"
                  title={localization.branding.themes.actionSecondaryColor}
                  sampleText={localization.branding.themes.actionSecondaryColorSample}
                  getColor={function() {return model.actionSecondaryColor();}}
                  getTextColor={function() {return model.actionSecondaryTextColor();}}
                  setColor={function(c) {return model.setActionSecondaryColor(c);}}
                  setTextColor={function(c) {return model.setActionSecondaryTextColor(c);}}
                  onColorPickerOpen={function() { self.hideAllColorPickers(); }}
                />
              </div>
              <div className="theme-edit-section">
                <ThemeColorEditor
                  ref="positiveColors"
                  title={localization.branding.themes.positiveColor}
                  sampleText={localization.branding.themes.positiveColorSample}
                  getColor={function() {return model.positiveColor();}}
                  getTextColor={function() {return model.positiveTextColor();}}
                  setColor={function(c) {return model.setPositiveColor(c);}}
                  setTextColor={function(c) {return model.setPositiveTextColor(c);}}
                  onColorPickerOpen={function() { self.hideAllColorPickers(); }}
                />
              </div>
              <div className="theme-edit-section">
                <ThemeColorEditor
                  ref="negativeColors"
                  title={localization.branding.themes.negativeColor}
                  sampleText={localization.branding.themes.negativeColorSample}
                  getColor={function() {return model.negativeColor();}}
                  getTextColor={function() {return model.negativeTextColor();}}
                  setColor={function(c) {return model.setNegativeColor(c);}}
                  setTextColor={function(c) {return model.setNegativeTextColor(c);}}
                  onColorPickerOpen={function() { self.hideAllColorPickers(); }}
                />
              </div>
              { /* temporarily disable font edition for signview */ }
              { this.props.preview !== "SigningPreview" &&
                <div className="theme-edit-section">
                  <ThemeFontEditor
                    title={localization.branding.themes.font}
                    sampleText={localization.branding.themes.fontSample}
                    getFont={function() {return model.font();}}
                    setFont={function(f) {return model.setFont(f);}}
                  />
                </div>
              }
              <div className="theme-delete-wrapper">
                <div className="theme-delete">
                  <div className="theme-delete-title title"> {localization.branding.themes.deleteTitle} </div>
                  <Button
                    text={localization.branding.themes.deleteNow}
                    onClick={function() {self.props.onDelete();}}
                  />
                  <Button
                    type="action"
                    className="float-right"
                    text={localization.branding.save}
                    onClick={function() {self.props.onSave();}}
                  />
                </div>
              </div>
          </div>
          <div className="separator">
            <div className="separator-top-padding"/>
            <div className="separator-bottom-padding"/>
          </div>
          <div className="previews">
            {this.props.previews.map(function(preview,key) {
                return (
                  <div key={"preview-" + key}>
                    <div className="preview-title">{self.previewTitle(preview)}</div>
                    {React.createElement(preview,{model: model})}
                  </div>
                );
              })
            }
          </div>

        </div>
      );
    }
  });
