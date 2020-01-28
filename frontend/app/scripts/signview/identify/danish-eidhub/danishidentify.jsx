var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var NemIDModel = require("./danishidentifymodel");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NemIDModel).isRequired
    },
    handleIdentify: function () {
      this.props.model.identify();
    },

    render: function () {
      var model = this.props.model;


      return (
        <span>
          <div className="identify-box-content">
            <div className="identify-box-button">
              <Button
                ref="identify-box-identify-button"
                size="big"
                type="action"
                text={localization.identifyBankId}
                onClick={this.handleIdentify}
              />
            </div>
          </div>
        </span>
      );
    }
  });
