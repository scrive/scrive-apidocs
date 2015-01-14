/** @jsx React.DOM */

define(["React","tinycolor","legacy_code","common/infotextinput"], function(React, tinycolor, _Legacy, InfoTextInput) {

return React.createClass({
  getInitialState: function() {
    return {
      showColorPicker : false
    };
  },
  hideColorPicker : function() {
    this.setState({showColorPicker : false});
  },
  componentDidMount : function() {
    var self = this;
    var colorPicker = $(this.refs.colorPicker.getDOMNode()).ColorPicker({
      flat: true,
      color: this.props.getValue(),
      onChange: function(hsb, hex, rgb) {
        if (self.state.showColorPicker) {
          self.props.setValue("#" + hex);
        }
      }
    });
    this.setState({colorPicker : colorPicker});
    colorPicker.hover(undefined,function() {
      self.setState({showColorPicker : false});
    });
  },
  componentDidUpdate: function() {
     var self = this;
     if (this.state.colorPicker && self.state.showColorPicker) {
       this.state.colorPicker.ColorPickerSetColor(self.props.getValue());
     }
  },
  render: function() {
    var self = this;
    return (
      <div className="domain-property-editor domain-color-property">
        <div className="domain-color-property-editor">
          <div>
            <strong className="domain-color-property-title">
              {self.props.title}
            </strong>
            <div className="domain-color-property-editor-color-picker-with-icons">
              { /*if*/ this.props.icons &&
                  _.map(this.props.icons,function(i) {
                    return (
                      <div className={"icon status " + i} key={Math.random()}  style={{backgroundColor: self.props.getValue() }}>
                      </div>
                    );
                  })


              }
              <div className="domain-color-property-editor-color-picker">
                <InfoTextInput
                  ref="colorInput"
                  value={this.props.getValue()}
                  onChange={function(v) {
                    if (tinycolor(v).ok)
                      self.props.setValue(v);
                    }
                  }
                  onBlur={
                    function() {
                      self.refs.colorInput.setValue(self.props.getValue());
                    }
                  }
                />
                <div
                  className="color-display"
                  style={{backgroundColor: this.props.getValue()}}
                  onClick={function() {
                    if (!self.state.showColorPicker) {
                      self.props.onColorPickerOpen();
                    }
                    self.setState({showColorPicker : !self.state.showColorPicker});
                  }}
                />
                <div ref="colorPicker" className={"color-picker " + (self.state.showColorPicker ? "" : "hidden")}/>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
});
});
