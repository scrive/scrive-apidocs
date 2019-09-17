var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var IDINIdentifyModel = require("./idinidentifymodel");

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(IDINIdentifyModel).isRequired
  },
  render: function () {
    var link = this.props.model.idinLink();
    window.location = this.props.model.idinLink();
    return (
      <div className="loadingSpinner"/>
    );
  }
});
