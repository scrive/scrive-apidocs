var React = require("react");
var NewButton = require("../common/button");


module.exports = React.createClass({
    propTypes: {
       onSelect : React.PropTypes.func,
       name : React.PropTypes.string,
       type :  React.PropTypes.string,
       size:  React.PropTypes.string,
       className : React.PropTypes.string,
       locked : React.PropTypes.bool,
       width : React.PropTypes.number
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
            locked={this.props.locked}
          />
        );
      }
    }
});
