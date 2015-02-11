/** @jsx React.DOM */

/**
 * A infotextinput component in React. It does not have all properties of original backbone infotextinput
 *
 * Properties:
      text        : string , text on button,  default ""
      color       : string, red | green | black | light-blue | signview-blue, one of predefined colors
      size        : string, tiny | small | big
      shape       : string, rounded | square, where square is default
      textcolor   : string, color for text on button, else default value for predefined color will be used.
      className   : additional css classes
      style       : style object (react format)
      onClick     : func, functiona to be called on click
      multiline   : bool, if button should support multiline labels (if true, text must be an array of strings)
      oneClick    : bool, if the button can be clicked only once

 *
 * Example usage:
 * var button = React.render(InfoTextInput({
 *    text: "ABC"
 * }), div);
 *
 */

define(['React'], function(React) {
  return React.createClass({
    propTypes: {
      infotext      : React.PropTypes.string,
      value         : React.PropTypes.string,
      inputtype     : React.PropTypes.string,
      name          : React.PropTypes.string,

      className     : React.PropTypes.string,
      style         : React.PropTypes.object,
      inputStyle    : React.PropTypes.object,
      maxLength     : React.PropTypes.number,

      // Behaviour
      suppressSpace : React.PropTypes.bool,
      autocompleate : React.PropTypes.bool,
      readonly      : React.PropTypes.bool,
      focus         : React.PropTypes.bool,

      // Events
      onChange      : React.PropTypes.func,
      onEnter       : React.PropTypes.func,
      onTab         : React.PropTypes.func,
      onBlur        : React.PropTypes.func,
      onRemove      : React.PropTypes.func,
      onOk          : React.PropTypes.func,
      onClick       : React.PropTypes.func,

      // More
      buttonTitle    : React.PropTypes.string
    },
    getDefaultProps : function() {
        return {
          "infotext" : "",
          "focus" : false,
          "value" : "",
          "suppressSpace" : false,
          "inputtype" : "text",
          "name" : "",
          "className" : "",
          "style" : {},
          "inputStyle" : {},
          "autocomplete" : false,
          "readonly" : false
        };
      },
    getInitialState: function() {
      return {value: this.props.value, focus : this.props.focus};
    },
    componentDidMount : function() {
      if (this.props.focus)
        this.focus();
    },
    componentWillReceiveProps: function(props) {
      if (props.value != this.props.value)
        this.setState({value : props.value});
    },
    // External functions. If we will create interface to this element on they, we should move it there
    setValue : function(v) {
      this.setState({value :v});
    },
    value : function() {
      return this.state.value;
    },
    focus : function() {
      $("input",this.getDOMNode()).focus();
    },
    onFocus : function() {
      this.setState({focus : true});
      if (this.props.onFocus != undefined)
        this.props.onFocus();
    },
    onBlur : function() {
      this.setState({focus : false});
      if (this.props.onBlur != undefined)
        this.props.onBlur();
    },
    onChange : function(event) {
        var newvalue = event.target.value;
        if (this.props.suppressSpace)
          newvalue.replace(/\s/g,'');
        this.setState({value :newvalue});
        if (this.props.onChange)
          this.props.onChange(newvalue);
    },
    onOk : function() {
      if (this.props.onOk != undefined)
        this.props.onOk();
    },
    onRemove : function() {
      if (this.props.onRemove != undefined)
        this.props.onRemove();
    },
    onKeyDown : function(e) {
      if (e.keyCode == 13 && this.props.onEnter != undefined)
          this.props.onEnter();
      else if (e.keyCode == 9 && this.props.onTab != undefined)
          this.props.onTab();
    },
    render: function() {
      var fakePlaceholder = BrowserInfo.isIE9orLower() && !this.state.focus && (this.state.value == undefined || this.state.value =="") ;
      return (
        <div
          style={this.props.style}
          className={"info-text-input " + (this.props.className ? this.props.className : "") + " " + (this.props.readonly ? "readonly" : "")}
        >
          <input
            name={this.props.name}
            type={this.props.inputtype}
            autoComplete={this.props.autocompleate}
            style={this.props.inputStyle}
            readOnly={this.props.readonly}
            disabled={this.props.readonly}
            className={fakePlaceholder ? "grayed" : ""}
            placeholder={this.props.infotext}
            maxLength={this.props.maxLength}
            value={fakePlaceholder ? this.props.infotext : this.state.value}
            onFocus={this.onFocus}
            onBlur={this.onBlur}
            onChange={this.onChange}
            onKeyDown={this.onKeyDown}
          />

           {/*if*/ this.props.onRemove != undefined &&
              <div className="closer" onClick={this.onRemove}/>
           }
           {/*if*/ this.props.onOk != undefined &&
              <div className="ok-button" onClick={this.onOk}>OK</div>
           }
           {/*if*/ (this.props.buttonTitle != undefined && this.props.onClick != undefined) &&
              <div className="internal-button-wrapper"><div className="internal-button" onClick={this.props.onClick}>{this.props.buttonTitle}</div></div>
           }
        </div>
      );
    }
  });
});


