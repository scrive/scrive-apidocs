var React = require("react");
  module.exports = React.createClass({
    render: function () {
      var overlayClass = React.addons.classSet({
        "overlay": true,
        "active": this.props.on
      });

      return (
        <div className={overlayClass} />
      );
    }
  });
