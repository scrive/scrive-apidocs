var React = require("react");
var $ = require("jquery");
var _ = require("underscore");
var classNames = require("classnames");
var Track = require("./track");

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
      id          : string, button's id attribute value (default's to undefined=no attribute)
      href        : string, button's href attribute value (default's to undefined=no attribute)

 *
 * Example usage:
 * var button = React.render(React.createElement(Button,{
 *    text: "ABC"
 * }), div);
 *
 */

  module.exports = React.createClass({
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
      href        : React.PropTypes.string,
      id          : React.PropTypes.string,
      oneClick    : React.PropTypes.bool
    },
    getDefaultProps : function() {
        return {
          "text"      : "",
          "size"      : "small",
          "multiline" : false,
          "href"      : undefined,
          "id"        : undefined,
          "style"     : {},
          onClick: function () { }
        };
      },
    getInitialState: function() {
      return {clicked: false,
              "type": this.props.type,
              originalType: this.props.type};
    },
    handleClick: function() {
      var eventName = "Clicked button " + this.props.text;
      var fireClick = !this.state.clicked ||  !this.props.oneClick;
      Track.track(eventName);
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
    className : function() {
      return classNames("button",this.props.className, this.state.type, {
        "button-small": this.props.size == "tiny",
        "button-large": this.props.size == "big",
        "inactive": this.props.oneClick && this.state.clicked,
        "button-signleline" : this.props.multiline
      });
    },
    style : function() {
      return  _.extend({width: this.width()},this.props.style);
    },
    componentDidUpdate: function () {
      // This is a workaround for https://github.com/facebook/react/issues/1448
      // and can be removed after updating to react v15
      var hrefAttr = this.props.href;
      if ((hrefAttr === undefined || hrefAttr === "") && this.isMounted()) {
        $(this.getDOMNode()).removeAttr("href");
      }
    },
    render: function() {
      return (
        <a id={this.props.id} href={this.props.href} className={this.className()} onClick={this.handleClick} style={this.style()}>
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
