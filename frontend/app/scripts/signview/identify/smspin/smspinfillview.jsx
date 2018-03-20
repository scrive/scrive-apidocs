var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var SMSPinIdentifyModel = require("./smspinidentifymodel");
var InfoTextInput = require("../../../common/infotextinput");
var Button = require("../../../common/button");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SMSPinIdentifyModel).isRequired
    },
    handleIdentify: function () {
      var model = this.props.model;
      model.doc().identifyToViewWithSMSPin(
        model.pin(),
        function () {
          window.location.reload();
        },
        function (xhr) {
          new FlashMessage({content: localization.identifyWithPin.invalidPin, type: "error"});
        }
      ).send();
    },
    render: function () {
      var model = this.props.model;
      return (
        <span>
          <div className="identify-box-content">
            <div className="identify-with-pin-box">
              <div>{localization.identifyWithPin.enterPin}</div>
              <InfoTextInput
                infotext={localization.identifyWithPin.pinInfotext}
                inputtype="number"
                onChange={ function (v) { model.setPin(v); } }
                onEnter={this.handleIdentify}
              />
              <div className="identify-box-button">
                <Button
                  size="big"
                  type="action"
                  text={localization.identifyBankId}
                  onClick={this.handleIdentify}
                />
              </div>
            </div>
          </div>
        </span>
      );
    }
  });
