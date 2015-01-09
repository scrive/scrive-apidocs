/** @jsx React.DOM */

define(["React","tinycolor","common/backbone_mixin","admin/brandeddomain/domainviewmodel","themes/theme"  ,"legacy_code","common/button","common/uploadimagebutton","common/select","common/infotextinput"], function(React, tinycolor,BackboneMixin, DomainViewModel,Theme,_Legacy, Button, UploadImageButton,NewSelect,InfoTextInput) {


var ThemeLogoEditor = React.createClass({
  render: function() {
    var self = this;
    var model = this.props.model;
    return (
      <div className="theme-logo-editor">
        <div className="title theme-logo-editor-title">
          {this.props.title}
        </div>
        <div className="theme-logo-editor-edit">
          <div
            className="logo-sample"
            style={{backgroundColor: this.props.getBackgroundColor()}}
            >
            <img src={this.props.getLogo()}/>
          </div>
          <div className="theme-edit-logo-upload">
             <UploadImageButton
                text={localization.branding.themes.uploadNewLogo}
                width={140}
                size="tiny"
                onUpload={function(logo) {
                  self.props.setLogo(logo);
                }}
              />
          </div>
        </div>
      </div>
    );
  }
});

var ThemeFontEditor = React.createClass({
  availableFonts: function() {
    return [
        {name :'Comic Sans MS', value: 'comic sans ms,sans-serif'}
      , {name :'Courier New', value: 'courier new,monospace'}
      , {name :'Helvetica', value: 'helvetica,sans-serif'}
      , {name :'Garamond', value:  'garamond,serif'}
      , {name :'Georgia', value:  'georgia,serif'}
      , {name :'Narrow', value:  'arial narrow,sans-serif'}
      , {name :'Sans Serif', value:  'arial,helvetica,sans-serif'}
      , {name :'Serif', value:  'times new roman,serif'}
      , {name :'Source Sans Pro', value: 'Source Sans Pro, Helvetica Neue, Arial, sans-serif'}
      , {name :'Tahoma', value: 'tahoma,sans-serif'}
      , {name :'Trebuchet MS', value:  'trebuchet ms,sans-serif'}
      , {name :'Verdana', value:  'verdana,sans-serif'}
      , {name :'Wide', value: 'arial black,sans-serif'}
    ];
  } ,
  render: function() {
    var self = this;
    var model = this.props.model;
    var Select  = NewSelect.Select;
    var availableFonts = self.availableFonts();
    var availableFontOptions = [];
    var selectedFontName = localization.branding.themes.customFont;
    var selectedFontValue = "";

    _.each(availableFonts,function(f) {
      if (f.value == self.props.getFont()) {
        selectedFontName = f.name;
        selectedFontValue = f.value;
      } else {
        availableFontOptions.push({
          name : f.name,
          style: {fontFamily : f.value},
          onSelect : function() {
            self.props.setFont(f.value);
            return false;
          }
        });
      }
    });

    return (
      <div className="theme-font-editor">
        <div className="title theme-font-editor-title">
          {this.props.title}
        </div>
        <div className="theme-font-editor-edit">
          <div
            key={Math.random()} // This is needed, else react will not regenerate styles
            className="font-sample"
            style={{fontFamily: this.props.getFont()}}
            dangerouslySetInnerHTML={{__html: this.props.sampleText}}
            >
          </div>
          <div className="theme-edit-font-select">
              <Select
                color={"#000000"}
                options={availableFontOptions}
                name ={selectedFontName}
                textWidth = {109}
                optionsWidth = "136px"
                style={{fontFamily : selectedFontValue}}
              />
          </div>
        </div>
      </div>
    );
  }
});


var ThemeColorEditor = React.createClass({
  getInitialState: function() {
    return {
      showColorColorPicker : false,
      showTextColorPicker  : false
    };
  },
  hideColorPickers : function() {
    this.setState({showColorColorPicker : false,showTextColorPicker: false});

  },
  componentDidMount : function() {
    var self = this;
    var colorColorPicker = $(this.refs.colorColorPicker.getDOMNode()).ColorPicker({
      flat: true,
      color: this.props.getColor(),
      onChange: function(hsb, hex, rgb) {
        if (self.state.showColorColorPicker) {
          self.props.setColor("#" + hex);
        }
      }
    });

    var textColorPicker = $(this.refs.textColorPicker.getDOMNode()).ColorPicker({
      flat: true,
      color: this.props.getTextColor(),
      onChange: function(hsb, hex, rgb) {
        if (self.state.showTextColorPicker) {
          self.props.setTextColor("#" + hex);
        }
      }
    });
    this.setState({colorColorPicker : colorColorPicker, textColorPicker : textColorPicker});
    colorColorPicker.hover(undefined,function() {
      self.setState({showColorColorPicker : false});
    });
    textColorPicker.hover(undefined,function() {
      self.setState({showTextColorPicker : false});
    });
  },
  componentDidUpdate: function() {
     var self = this;
     if (this.state.colorColorPicker && self.state.showColorColorPicker) {
       this.state.colorColorPicker.ColorPickerSetColor(self.props.getColor());
     }
     if (this.state.textColorPicker && self.state.showTextColorPicker) {
       this.state.textColorPicker.ColorPickerSetColor(self.props.getTextColor());
     }
  },
  render: function() {
    var self = this;
    var model = this.props.model;
    return (
      <div className="theme-color-editor">
        <div className="title theme-color-editor-title">
          {this.props.title}
        </div>
        <div className="theme-color-editor-edit">

          <div
            className="color-sample"
            style={{color: this.props.getTextColor(), backgroundColor: this.props.getColor()}}
            dangerouslySetInnerHTML={{__html: this.props.sampleText}}
            >
          </div>
          <div className="theme-edit-editor-edit-colors">
            <div className="theme-edit-editor-edit-color">
              <InfoTextInput
                ref="colorInput"
                value={this.props.getColor()}
                onChange={function(v) {
                  if (tinycolor(v).ok)
                    self.props.setColor(v);
                }}
                onBlur={
                  function() {
                    self.refs.colorInput.setValue(self.props.getColor());
                  }
                }
              />
              <div
                className="color-display"
                style={{backgroundColor: this.props.getColor()}}
                onClick={function() {
                  if (!self.state.showColorColorPicker) {
                    self.props.onColorPickerOpen();
                  }
                  self.setState({showColorColorPicker : !self.state.showColorColorPicker});
                }}
              />
              <div ref="colorColorPicker" className={"color-picker " + (self.state.showColorColorPicker ? "" : "hidden")}/>
            </div>

            <div className="theme-edit-editor-edit-textcolor">
              <InfoTextInput
                ref="textColorInput"
                value={this.props.getTextColor()}
                onChange={function(v) {
                  if (tinycolor(v).ok)
                    self.props.setTextColor(v);
                }}
                onBlur={
                  function() {
                    self.refs.textColorInput.setValue(self.props.getTextColor());
                  }
                }
              />
              <div
                className="color-display"
                style={{backgroundColor: this.props.getTextColor()}}
                onClick={function() {
                  if (!self.state.showTextColorPicker) {
                    self.props.onColorPickerOpen();
                  }
                  self.setState({showTextColorPicker : !self.state.showTextColorPicker});
                }}
              />
              <div ref="textColorPicker" className={"color-picker " + (self.state.showTextColorPicker ? "" : "hidden")}/>
            </div>
          </div>
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
    hideAllColorPickers : function() {
      var self = this;
      self.refs.brandColors.hideColorPickers();
      self.refs.actionColors.hideColorPickers();
      self.refs.actionSecondaryColors.hideColorPickers();
      self.refs.positiveColors.hideColorPickers();
      self.refs.negativeColors.hideColorPickers();
    },
    render: function() {
      var self = this;
      var model = this.props.model;
      return (
        <div className="theme-edit">
          <div className="theme-edit-panel">
              <div className="theme-title-section">
                {self.props.title}
              </div>
              <div className="theme-edit-section">
                <div className='title'>{localization.branding.themes.name}</div>
                  <InfoTextInput
                    value={model.name()}
                    onChange={function(v) {model.setName(v);}}
                  />
              </div>
              <div className="theme-edit-section">
                <ThemeLogoEditor
                  title={localization.branding.themes.logo}
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
              <div className="theme-edit-section">
                <ThemeFontEditor
                  title={localization.branding.themes.font}
                  sampleText={localization.branding.themes.fontSample}
                  getFont={function() {return model.font();}}
                  setFont={function(f) {return model.setFont(f);}}
                />
              </div>
              <div className="theme-delete-wrapper">
                <div className="theme-delete">
                  <div className="theme-delete-title title"> {localization.branding.themes.deleteTitle} </div>
                  <Button
                    text={localization.branding.themes.delete}
                    onClick={function() {self.props.onDelete();}}
                  />
                </div>
              </div>
          </div>
          <div className="separator">
            <div className="separator-top-padding"/>
            <div className="separator-bottom-padding"/>
          </div>
          <div className="previews">
            { this.props.previews.map(function(preview) {
                return preview({model: model});
              })
            }
          </div>

        </div>
      );
    }
  });
});
