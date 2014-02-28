/** @jsx React.DOM */

/**
 * A button component in React. It does not have all properties of original backbone button,
 * so please double chack if you use options not described here.
 *
 * Properties:
      text        : string , text on button,  default ""
      color       : string, red | green | black | light-blue | signview-blue, one of predefined colors
      customcolor : string, custom color for button, overwrites color
      size        : string, tiny | small | big
      shape       : string, rounded | square, where square is default
      textcolor   : string, color for text on button, else default value for predefined color will be used.
      width       : integer, final width of button, if not set, button will adjust to text
      cssClass    : additional css classes
      style       : style object (react format)
      onClick     : func, functiona to be called on click
      oneClick    : bool, if the button can be clicked only once

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
          "shape"    : "square",
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
