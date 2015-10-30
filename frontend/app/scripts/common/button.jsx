/** @jsx React.DOM */

/**
 * A button component in React. It does not have all properties of original backbone button,
 * so please double chack if you use options not described here.
 *
 * Properties:
      text        : string , text on button,  default ""
      type:       : string, "action | optional | cancel | main",
      size        : string, tiny | small | big
      textcolor   : string, color for text on button, else default value for predefined color will be used.
      width       : integer, final width of button, if not set, button will adjust to text
      className    : additional css classes
      style       : style object (react format)
      onClick     : func, functiona to be called on click
      multiline   : bool, if button should support multiline labels (if true, text must be an array of strings)
      oneClick    : bool, if the button can be clicked only once

 *
 * Example usage:
 * var button = React.render(React.createElement(Button,{
 *    text: "ABC"
 * }), div);
 *
 */

define(['React'], function(React) {
  return React.createClass({
    propTypes: {
      text        : function(props, propName, componentName) {
        var val = props[propName];
        if (typeof val !== 'string' && !$.isArray(val)) {
          console.error('Button.text should be a string or an array of strings');
        }
      },
      type        : React.PropTypes.string,
      size        : React.PropTypes.string,
      textcolor   : React.PropTypes.string,
      width       : React.PropTypes.number,
      className   : React.PropTypes.string,
      style       : React.PropTypes.object,
      onClick     : React.PropTypes.func,
      multiline   : React.PropTypes.bool,
      oneClick    : React.PropTypes.bool
    },
    getDefaultProps : function() {
        return {
          "text"      : "",
          "size"      : "small",
          "multiline" : false,
          "style"     : {}
        };
      },
    getInitialState: function() {
      return {clicked: false,
              "type": this.props.type,
              originalType: this.props.type};
    },
    handleClick: function() {
      var eventName = 'Clicked button' + this.props.text;
      var fireClick = !this.state.clicked ||  !this.props.oneClick;
      mixpanel.track(eventName);
      this.setState({'clicked': true});
      if (fireClick) {
        this.props.onClick();
      }
    },
    borderWidth : function(){
        if (this.props.size == "tiny" || this.props.size == "small")
            return 1;
        else if (this.props.size == "big")
            return 2;
        return 0;
    },
    labelPadding: function() {
        if (this.props.size == "tiny" || this.props.size == "small")
            return 16;
        else if (this.props.size == "big")
            return 30;
        return 0;
    },
    width: function() {
      if (this.props.width)
        return this.props.width - 2*this.borderWidth() - 2* this.labelPadding();
    },
    sizeClass : function() {
     if (this.props.size == "tiny")
         return "button-small";
     else if (this.props.size == "big")
         return "button-large";
     return "";
    },
    multilineClass : function() {
      return this.props.multiline ? "button-signleline": "";
    },
    inactiveClass : function() {
      return (this.props.oneClick && this.state.clicked) ? "inactive " : "";
    },
    className : function() {
      return this.inactiveClass() + (this.props.className || "") + " button " + this.sizeClass() + " " + (this.state.type || "") + " " + this.multilineClass();
    },
    style : function() {
      return  _.extend({width: this.width()},this.props.style);
    },
    render: function() {
      return (
        <a className={this.className()} onClick={this.handleClick} style={this.style()}>
          <div className="label" style={{'color': this.props.textcolor}}>
            {/*if*/ this.props.multiline &&
              this.props.text.map(function(text, i) {
                return (
                  <div key={i}>{text}</div>
                );
              })
            }
            {/*else*/ !this.props.multiline &&
              this.props.text
            }
          </div>
        </a>
      );
    }
  });
});
