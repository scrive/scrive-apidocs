var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var Checkbox = require("../../../common/checkbox");
var SwedishIdentifyModel = require("./swedishidentifymodel");
var MaskedPersonalNumber = require("../masked_personal_number");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SwedishIdentifyModel).isRequired
    },
    toggleThisDevice: function () {
      this.props.model.setThisDevice(!this.props.model.thisDevice());
    },
    handleIdentify: function () {
      this.props.model.identify();
    },
    render: function () {
      var buttonStyle = {
        marginTop: "30px",
        marginBottom: "38px"
      };

      return (
        <span>
          {localization.idNumber} <MaskedPersonalNumber
            number={this.props.model.doc().currentSignatory().personalnumber()}
            isNorwegian={false}
            isDanishPersonal={false}
            isDanishEmployee={false}
            isFinnish={false}
          />

          <div className="identify-box-button">
            <Button
              ref="identify-button"
              style={buttonStyle}
              size="big"
              type="action"
              text={localization.identifyBankId}
              onClick={this.handleIdentify}
            />
          </div>
          <Checkbox
            ref="this-device-checkbox"
            className="identify-box-checkbox"
            label={localization.openBankId}
            onChange={this.toggleThisDevice}
            checked={this.props.model.thisDevice()}
          />
        </span>
      );
    }
  });
