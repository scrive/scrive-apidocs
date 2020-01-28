var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;
var DanishIdentifyModel = require("./danishidentifymodel");
  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(DanishIdentifyModel).isRequired
    },

    render: function () {
      var link = this.props.model.nemIDLink();
      return (
        <span>
          <iframe
            ref="iframe"
            style={{minHeight: "350px", width: "100%", margin: "auto"}}
            src={link}
            />
        </span>
      );

    }
  });
