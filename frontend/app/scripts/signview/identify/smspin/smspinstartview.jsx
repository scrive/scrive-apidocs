var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var classNames = require("classnames");
var ErrorModal = require("../../errormodal");
var SMSPinIdentifyModel = require("./smspinidentifymodel");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SMSPinIdentifyModel).isRequired
    },

    handleStart: function () {
      var model = this.props.model;
      model.doc().requestPinToView(
        model.setToFillStep,
        function (xhr) {
          new ErrorModal(xhr);
        }
      ).send();

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
                onClick={this.handleStart}
              />
            </div>
          </div>
        </span>
      );
    }
  });
