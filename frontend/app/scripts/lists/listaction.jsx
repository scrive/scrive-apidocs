/** @jsx React.DOM */

define(['React','common/button','legacy_code'], function(React, NewButton) {

return React.createClass({
    propTypes: {
       onSelect : React.PropTypes.func,
       name : React.PropTypes.string,
       type :  React.PropTypes.string,
       size:  React.PropTypes.string,
       className : React.PropTypes.string,
       width : React.PropTypes.number,
    },
    getDefaultProps: function() {
      return {
        onSelect: function() {},
        name : "",
        size: "normal",
        className: ""
      };
    },
    render: function() {
      var self = this;
      var model = self.props.model;
      if (this.props.component != undefined) {
        return (this.props.component(model));
      } else {
        return (
          <NewButton
            onClick={function() {
              self.props.onSelect(model.list().getSelected(),model);
            }}
            type={this.props.type}
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

