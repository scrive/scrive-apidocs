var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var VerimiIdentifyModel = require("./verimiidentifymodel");
  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(VerimiIdentifyModel).isRequired
    },
    render: function () {
      var link = this.props.model.verimiLink();
      window.location = this.props.model.verimiLink();
      return (
        <div className="loadingSpinner"/>
      );
    }
  });
