define(["legacy_code", "Underscore", "Backbone", "React", "common/button"],
  function (legacy_code, _, Backbone, React, Button) {
  return React.createClass({
    propTypes: {
      onBack: React.PropTypes.func.isRequired
    },

    render: function () {
      var buttonStyle = {
        marginTop: "34px"
      };

      return (
        <span>
          {localization.problemBankId}
          <div className="identify-box-button">
            <Button
              style={buttonStyle}
              size="big"
              type="action"
              text={localization.ok}
              onClick={this.props.onBack}
            />
          </div>
        </span>
      );
    }
  });
});
