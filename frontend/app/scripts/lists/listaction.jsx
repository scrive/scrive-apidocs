/** @jsx React.DOM */

define(['React','common/button','legacy_code'], function(React, NewButton) {

return React.createClass({
    propTypes: {
       onSelect : React.PropTypes.func,
       name : React.PropTypes.string,
       color :  React.PropTypes.string,
       size:  React.PropTypes.string,
       className : React.PropTypes.string,
       width : React.PropTypes.number,
       // elFunction - Function that can generate jQuery DOM element. Used only in speciall cases, when React component is not available
       elFunction : React.PropTypes.func
    },
    getDefaultProps: function() {
      return {
        onSelect: function() {},
        name : "",
        color: "black",
        size: "normal",
        className: ""
      };
    },
    componentDidMount : function() {
      if (this.props.elFunction && this.refs.placeholder)
        $(this.refs.placeholder.getDOMNode()).append(this.props.elFunction(this.props.model));
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      if (this.props.elFunction != undefined) {
        return (
          <div
            className='float-left actionButton'
            ref='placeholder'
          />
        );
      } else {
        return (
          <NewButton
            onClick={function() {
              self.props.onSelect(model.list().getSelected(),model);
            }}
            color={this.props.color}
            size={this.props.size}
            text={this.props.name}
            width={this.props.width}
            className={"float-left actionButton " + this.props.className}
          />
        );
      }
    }
});

});

