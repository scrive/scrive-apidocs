var React = require("react");
var $ = require("jquery");
var BrowserInfo = require("../../js/utils/browserinfo.js").BrowserInfo;
var _ = require("underscore");

/**
 * A infotextinput component in React. Used instead of input tag for text input.
 * It can automaticly deal with IE8, and also has a lot of features like
 *
 *   - autogrowth
 *   - internal small button
 *   - remove button (x on in top, right corner)
 *   - restrict input, not to let people type in numbers in name field
 *
 * Basic props, for full list check propTypes:
      infotext    : string , used instead of placeholder
      value       : string,
      inputtype   : string, default is "text", but "password" is also used
      name        : string, name for input
      disabled    : bool
      focus       : bool, focus initially. WARNING: DO NOT USE THIS
                    WHEN INPUT IS PART OF ANIMATION, BECAUSE IT WILL FAIL IN IE
                    BEFORE ANIMATION ENDS

      className   : additional css classes
      style       : style object (react format)
      inputStyle  : style object - one to applied only to input element

      onChange    : func with new value as param,
 *
 * Sample usage:
 * var button = React.render(React.createElement(InfoTextInput(,
 *    infotext: "Name",
      value : signatory.name(),
 *    onChange: function(v) {signatory.setName(v);}
 * }), div);
 *
 */

  module.exports = React.createClass({
    displayName: "InfoTextInput",

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
      autocomplete  : React.PropTypes.bool,
      readonly      : React.PropTypes.bool,
      focus         : React.PropTypes.bool,
      autoGrowth    : React.PropTypes.bool,
      restrictInput : React.PropTypes.func,
      tabIndex      : React.PropTypes.number,
      disabled      : React.PropTypes.bool,

      // Events
      onChange      : React.PropTypes.func,
      onEnter       : React.PropTypes.func,
      onTab         : React.PropTypes.func,
      onBlur        : React.PropTypes.func,
      onRemove      : React.PropTypes.func,
      onClick       : React.PropTypes.func,
      onButtonClick : React.PropTypes.func,
      onAutoGrowth  : React.PropTypes.func,
      onMouseEnter  : React.PropTypes.func,
      onMouseLeave  : React.PropTypes.func,

      // More
      buttonTitle     : React.PropTypes.string,
      buttonClassName : React.PropTypes.string,
      buttonToolTip   : React.PropTypes.string
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
          "readonly" : false,
          "disabled" : false,
          "restrictInput": function () { return true; }
        };
      },
    getInitialState: function() {
      return {value: this.props.value, focus : this.props.focus, textWidth: 0};
    },
    componentDidMount : function() {
      if (this.props.focus)
        this.focus();

      if (this.props.autoGrowth) {
        this.computeTextWidth();
      }
    },
    componentDidUpdate: function (prevProps, prevState) {
      if (this.props.onAutoGrowth) {
        this.props.onAutoGrowth();
      }

      var textWidthValuesChanged = prevState.value !== this.state.value || prevProps.infotext !== this.props.infotext;
      var textWidthStylesChanged = prevProps.className !== this.props.className ||
        !_.isEqual(prevProps.inputstyle, this.props.inputstyle) ||  !_.isEqual(prevProps.style, this.props.style);

      if ((textWidthValuesChanged || textWidthStylesChanged) && this.props.autoGrowth) {
        this.computeTextWidth();
      }
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
    measureText: function (text) {
      if (!this.refs.growth) {
        return 0;
      }

      var $growth = $(this.refs.growth.getDOMNode());
      $growth.text(text);
      return $growth.width() + 1;
    },
    computeTextWidth: function () {
      var valueWidth = this.measureText(this.state.value);
      var infotextWidth = this.measureText(this.props.infotext);
      var textWidth = Math.max(valueWidth, infotextWidth);

      var width = textWidth;

      this.setState({textWidth: width});
    },
    focus : function() {
      $(this.refs.input.getDOMNode()).focus();
      // After calling focus - caret is sometimes at the begining of text.
      // Use text selection of 0 length at the end to move the caret position
      var length = $(this.refs.input.getDOMNode()).val().length;
      // setSelectionRange is only supports "text" input type.
      if (this.props.inputtype === "text") {
        this.refs.input.getDOMNode().setSelectionRange(length, length);
      }
    },
    hasFocus : function () {
      return this.state.focus;
    },
    selectText : function() {
      this.refs.input.getDOMNode().setSelectionRange(0, $(this.refs.input.getDOMNode()).val().length);
    },
    onClick: function (e) {
      if (this.props.onClick != undefined) {
        this.props.onClick();
      }
      // focus when clicking on the padding of the outer element,
      // but only when clicking on the outer element as we dont want to
      // break selecting.
      if (this.getDOMNode() === e.target) {
        this.focus();
      }
    },
    onFocus : function(e) {
      this.setState({focus : true});
      if (this.props.onFocus != undefined)
        this.props.onFocus(e);
    },
    onBlur : function(e) {
      this.setState({focus : false});
      if (this.props.onBlur != undefined)
        this.props.onBlur(e);
    },
    onChange : function(event) {
        var newvalue = event.target.value;
        if (this.props.suppressSpace)
          newvalue.replace(/\s/g,'');
        var valid = this.props.restrictInput(newvalue);
        if (valid) {
          this.setState({value :newvalue});
          if (this.props.onChange)
            this.props.onChange(newvalue);
        }
    },
    onKeyDown : function(e) {
      if (e.keyCode == 13 && this.props.onEnter != undefined)
          this.props.onEnter();
      else if (e.keyCode == 9 && this.props.onTab != undefined) {
        this.props.onTab(e);
      }
    },
    onMouseEnter : function(e) {
      if (_.isFunction(this.props.onMouseEnter)) {
        this.props.onMouseEnter(e);
      }
    },
    onMouseLeave : function(e) {
      if (_.isFunction(this.props.onMouseLeave)) {
        this.props.onMouseLeave(e);
      }
    },
    blurInput: function() {
      if (this.isMounted() && this.refs.input) {
        $(this.refs.input.getDOMNode()).blur();
      }
    },
    render: function() {
      var fakePlaceholder = BrowserInfo.isIE9orLower() && !this.state.focus && (this.state.value == undefined || this.state.value =="") ;

      var inputStyle = _.extend({}, this.props.inputStyle);

      if (this.props.autoGrowth) {
        _.extend(inputStyle, {width: this.state.textWidth + "px"});
      }

      var type = this.props.inputtype == "numeric" ? "tel" : this.props.inputtype;

      return (
        <div
          style={this.props.style}
          className={"info-text-input " + (this.props.className ? this.props.className : "") + " " + (this.props.readonly ? "readonly" : "")}
          onClick={this.onClick}
        >
          <input
            id={this.props.id}
            name={this.props.name}
            type={type}
            autoComplete={this.props.autocomplete ? "on" : "off"}
            style={inputStyle}
            readOnly={this.props.readonly}
            tabIndex={this.props.tabIndex}
            className={fakePlaceholder ? "grayed" : ""}
            placeholder={this.props.infotext}
            maxLength={this.props.maxLength}
            value={fakePlaceholder ? this.props.infotext : this.state.value}
            disabled={this.props.disabled}
            onFocus={this.onFocus}
            onBlur={this.onBlur}
            onChange={this.onChange}
            onKeyDown={this.onKeyDown}
            onMouseEnter={this.onMouseEnter}
            onMouseLeave={this.onMouseLeave}
            ref="input"
          />

           {/*if*/ this.props.onRemove != undefined &&
              <div ref="close" className="closer" onClick={this.props.onRemove}/>
           }
           {/*if*/ (this.props.onButtonClick != undefined) &&
              <div className="internal-button-wrapper">
                <div
                  title={this.props.buttonTooltip}
                  className={"internal-button " + (this.props.buttonClassName || "")}
                  onClick={this.props.onButtonClick}
                >
                  {this.props.buttonTitle}
                </div>
              </div>
           }
           {/*if*/ this.props.autoGrowth &&
              <div ref="growth" className="growth" style={this.props.inputStyle} />
           }
          <div className="after" />
        </div>
      );
    }
  });
