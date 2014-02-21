/** @jsx React.DOM */

/**
 * A button component. Used in branding settings.
 *
 * Properties taken:
 * label, a string labeling the button.
 * initiallyChecked, a boolean representing the initial checked(=true)/unchecked(=false) state.
 * onChange, a callback function being called every time the checkbox is checked or unchecked.
 *
 * Example usage:
 * var button = React.renderComponent(Button.Button({
 *    text: "ABC"
 * }), div);
 *
 */

define(['React'], function(React) {
  var expose = {};

  var Button = React.createClass({
    propTypes: {
      text        : React.PropTypes.string,
      color       : React.PropTypes.string,
      customcolor : React.PropTypes.string,
      size        : React.PropTypes.string,
      shape       : React.PropTypes.string,
      textcolor   : React.PropTypes.string,
      width       : React.PropTypes.string,
      cssClass    : React.PropTypes.string,
      style       : React.PropTypes.object,
      onClick     : React.PropTypes.func,
      oneClick    : React.PropTypes.bool
    },
    getDefaultProps : function() {
        return {
          "text"     : "",
          "color"    : "green",
          "size"     : "small",
          "shape"    : "squere",
          "style"    : {}
        };
      },
    getInitialState: function() {
      return {clicked: false};
    },
    handleClick: function() {
      var eventName = 'Clicked button' + this.props.text;
      mixpanel.track(eventName);
      if (!this.state.clicked ||  !this.props.oneClick) {
        this.props.onClick();
      }
      this.setState({'clicked': true});
    },
    borderWidth : function(){
        if (this.props.size == "tiny" || this.props.size == "small")
            return 1;
        else if (this.props.size == "big")
            return 2;
        return 0;
    },
    labelPadding: function() {
        if (this.props.size == "tiny" || this.propssize == "small")
            return 16;
        else if (this.props.size == "big")
            return 30;
        return 0;
    },
    width: function() {
      if (this.props.width)
        return this.props.width() - 2*this.borderWidth() - 2* this.labelPadding();
    },
    sizeClass : function() {
     if (this.props.size == "tiny")
         return {"button-small":true};
     else if (this.props.size == "big")
         return {"button-large":true};
    },
    colorClass : function() {
     if (this.props.color  == "red" )
        return {"button-red":true};
     else if (this.props.color  == "green" )
        return {"button-green":true};
     else if (this.props.color  == "black")
        return {"button-gray":true};
     else if (this.props.color == "light-blue")
        return {"button-light-blue":true};
     else if (this.props.color == "signview-blue")
        return {"button-signview-blue":true};
    },
    shapeClass : function() {
       if (this.props.shape == "rounded" )
         return {"button-round":true};
    },
    cssClasses : function() {
      return React.addons.classSet(_.extend(
                            {button: true},
                            this.sizeClass(),
                            this.colorClass(),
                            this.shapeClass()
             ));
    },
    style : function() {
      return  _.extend({width: this.width()},this.props.style);
    },
    render: function() {
      return (
        <a className={this.cssClasses()} onClick={this.handleClick} style={this.style()}>
          <div class='label'>{this.props.text}</div>
        </a>
      );
    }
  });

  expose.Button = Button;

  return expose;
});
