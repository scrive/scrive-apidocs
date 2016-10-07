var React = require("react");
var Backbone = require("backbone");
var InfoTextInput = require("../../common/infotextinput");
var LocationUtils = require("../../common/location");
var $ = require("jquery");

  module.exports = React.createClass({

    propTypes: {
      signatory: React.PropTypes.object.isRequired
    },

    getAPIDeliveryURL: function () {
      var signatory = this.props.signatory;
      return LocationUtils.origin() + signatory.apideliveryurl();
    },

    selectText: function () {
      this.refs.input.focus();
      this.refs.input.selectText();
    },

    render: function () {
      var signatory = this.props.signatory;

      return (
        <div className="docview-showapidelivery-modal">
          <label>{localization.docview.showAPIDelivery.description}</label>
          <InfoTextInput
            className="api-delivery-url"
            inputtype="text"
            ref="input"
            readonly={true}
            disabled={false}
            value={this.getAPIDeliveryURL()}
            onClick={this.selectText}
          />
        </div>
      );
    }
  });
