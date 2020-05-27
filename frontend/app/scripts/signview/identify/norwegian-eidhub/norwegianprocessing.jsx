var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;
var NorwegianIdentifyModel = require("./norwegianidentifymodel");
  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },

    render: function () {
      window.location = this.props.model.noBankIDLink();
      return (
        <div className="loadingSpinner"/>
      );

    }
  });
