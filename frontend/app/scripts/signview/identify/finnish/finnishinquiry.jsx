var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var InfoTextInput = require("../../../common/infotextinput");
var FinnishIdentifyModel = require("./finnishidentifymodel");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FinnishIdentifyModel).isRequired
    },

    getInitialState: function () {
      return {ssnValid: false};
    },

    handleInquiry: function () {
      if (this.state.ssnValid) {
        this.props.model.setProcessing();
      }
    },

    render: function () {
      var model = this.props.model;
      var self = this;

      var buttonStyle = {
        marginTop: "27px"
      };

      return (
        <span>
          <div className="identify-box-content">
            <InfoTextInput
              ref="identify-box-inquiry-personalnumber-input"
              infotext={localization.ssnInfoText}
              className={self.state.ssnValid ? "" : "redborder"}
              value={""}
              focus={true}
              onChange={function (val) {
                model.setPersonalNumber(val);
                self.setState({ssnValid: model.personalnumberValid()});
              }}
            />
            <div className="identify-box-button">
              <Button
                ref="identify-box-inquiry-button"
                style={buttonStyle}
                className={self.state.ssnValid ? "" : "disabled"}
                size="big"
                type="action"
                text={localization.identifyBankId}
                onClick={this.handleInquiry}
              />
            </div>
          </div>
        </span>
      );
    }
  });
