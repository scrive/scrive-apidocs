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
      var link = this.props.model.noBankIDLink();
      return (
        <div style={{padding: "10px"}} >
            <iframe
          ref="iframe"
          style={{minHeight: "350px", width: "100%", margin: "auto"}}
          src={link}
          />
      </div>
      );

    }
  });
