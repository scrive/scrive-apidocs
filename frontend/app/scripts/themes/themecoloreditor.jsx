/** @jsx React.DOM */

define(["React","tinycolor","legacy_code","common/infotextinput"], function(React, tinycolor,_Legacy, InfoTextInput) {

return React.createClass({
  propTypes: {
    title: React.PropTypes.string,
    sampleText: React.PropTypes.string,
    getColor: React.PropTypes.func,
    getTextColor: React.PropTypes.func,
    setColor: React.PropTypes.func,
    setTextColor: React.PropTypes.func,
    onColorPickerOpen: React.PropTypes.func
  },
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
                  if (tinycolor(v).ok && (tinycolor(v).format != "name" || /^[a-zA-Z]+$/.test(v)))
                    self.props.setColor(v);
                }}
                onBlur={
                  function() {
                    var hex = tinycolor(self.props.getColor()).toHexString();
                    self.props.setColor(hex);
                    self.refs.colorInput.setValue(hex);
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
                  if (tinycolor(v).ok && (tinycolor(v).format != "name" || /^[a-zA-Z]+$/.test(v)))
                    self.props.setTextColor(v);
                }}
                onBlur={
                  function() {
                    var hex = tinycolor(self.props.getTextColor()).toHexString();
                    self.props.setTextColor(hex);
                    self.refs.textColorInput.setValue(hex);
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
});
