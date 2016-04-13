var React = require("react");
var classNames = require("classnames");

  module.exports = React.createClass({
    render: function () {
      var overlayClass = classNames({
        "overlay": true,
        "active": this.props.on
      });

      return (
        <div className={overlayClass} />
      );
    }
  });
