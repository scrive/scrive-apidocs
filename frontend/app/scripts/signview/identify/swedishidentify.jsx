define(["legacy_code", "Underscore", "Backbone", "React", "common/button", "common/checkbox"],
  function (legacy_code, _, Backbone, React, Button, Checkbox) {
  return React.createClass({
    propTypes: {
      onIdentify: React.PropTypes.func.isRequired,
      personalNumber: React.PropTypes.string.isRequired,
      thisDevice: React.PropTypes.bool.isRequired,
      onSetThisDevice: React.PropTypes.func.isRequired
    },
    toggleThisDevice: function () {
      this.props.onSetThisDevice(!this.props.thisDevice);
    },
    handleIdentify: function () {
      this.props.onIdentify();
    },
    render: function () {
      var buttonStyle = {
        marginTop: "30px",
        marginBottom: "38px"
      };

      return (
        <span>
          {localization.idNumber} <b>{this.props.personalNumber}</b>
          <div className="identify-box-button">
            <Button
              style={buttonStyle}
              size="big"
              type="action"
              text={localization.identifyBankId}
              onClick={this.handleIdentify}
            />
          </div>
          <Checkbox
            className="identify-box-checkbox"
            label={localization.openBankId}
            onChange={this.toggleThisDevice}
            checked={this.props.thisDevice}
          />
        </span>
      );
    }
  });
});
