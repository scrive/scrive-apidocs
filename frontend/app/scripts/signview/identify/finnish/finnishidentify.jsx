var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var InfoTextInput = require("../../../common/infotextinput");
var FinnishIdentifyModel = require("./finnishidentifymodel");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;
var classNames = require("classnames");
var MaskedPersonalNumber = require("../masked_personal_number");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FinnishIdentifyModel).isRequired
    },

    handleIdentify: function () {
      this.props.model.identify();
    },

    render: function () {
      var model = this.props.model;

      var buttonStyle = {
        marginTop: "27px"
      };

      return (
        <span>
          <div className="identify-box-content">
            <span>
              {localization.idNumber} <MaskedPersonalNumber
                number={this.props.model.doc().currentSignatory().personalnumber()}
                isNorwegian={false}
                isDanishPersonal={false}
                isDanishEmployee={false}
                isFinnish={true}
              />
            </span>
            <div className="identify-box-button">
              <Button
                ref="identify-box-identify-button"
                style={buttonStyle}
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
