define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/checkbox", "signview/identify/swedish/swedishidentifymodel"],
  function (legacy_code, _, Backbone, React, Button, Checkbox, SwedishIdentifyModel) {
  return React.createClass({
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
          {localization.idNumber} <b>{this.props.model.doc().currentSignatory().personalnumber()}</b>
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
});
