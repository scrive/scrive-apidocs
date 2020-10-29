var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var ViewSize = require("../../viewsize");
var classNames = require("classnames");
var Button = require("../../../common/button");
var DanishIdentifyModel = require("./danishidentifymodel");
  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(DanishIdentifyModel).isRequired,
      onBack: React.PropTypes.func.isRequired,
      onChoice: React.PropTypes.func.isRequired
    },

    onChoice: function (cvrAuthMethod) {
      this.props.model.setCvrAuthMethod(cvrAuthMethod);
      this.props.model.identify();
    },

    render: function () {
      var self = this;
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });
      return (
          <div className="identify-box-content">
           <div className={divClass}>
              <Button
                text={localization.docsignview.eleg.bankid.signDKSignMethodEmployeeKeycard}
                onClick={function () {
                  self.onChoice("dk_nemid_cvr_keycard");
                }}
                className="identify-box-button-select"
              />
              <Button
                text={localization.docsignview.eleg.bankid.signDKSignMethodEmployeeKeyfile}
                onClick={function () {
                  self.onChoice("dk_nemid_cvr_keyfile");
                }}
                className="identify-box-button-select"
              />
              <Button
                text={localization.cancel}
                onClick={this.props.onBack}
                className="identify-box-button-select identify-box-transparent-button"
              />
            </div>
          </div>
      );

    }
  });
