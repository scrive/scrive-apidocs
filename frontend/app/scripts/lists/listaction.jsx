var React = require("react");
var NewButton = require("../common/button");


module.exports = React.createClass({
    propTypes: {
       onSelect : React.PropTypes.func,
       name : React.PropTypes.string,
       makeName: React.PropTypes.func,
       type :  React.PropTypes.string,
       size:  React.PropTypes.string,
       className : React.PropTypes.string,
       locked : React.PropTypes.bool,
       hidden : React.PropTypes.bool,
       width : React.PropTypes.number
    },
    getDefaultProps: function() {
      return {
        onSelect: function() {},
        name : "",
        size: "normal",
        disabled: false,
        className: ""
      };
    },
    render: function() {
      var self = this;
      var model = self.props.model;

      var list = model.list();
      var selected = list ? list.getSelected() : [];
      var name = self.props.makeName === undefined
                   ? self.props.name
                   : self.props.makeName(selected);
      var style = self.props.disabled ? {display: "none"} : {};

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
            text={name}
            width={this.props.width}
            style={style}
            className={"float-left actionButton " + this.props.className}
            locked={this.props.locked}
          />
        );
      }
    }
});
