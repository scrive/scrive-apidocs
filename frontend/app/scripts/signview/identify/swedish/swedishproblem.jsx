define(["legacy_code", "Underscore", "Backbone", "React", "common/button",  "signview/identify/swedish/swedishidentifymodel"],
  function (legacy_code, _, Backbone, React, Button, SwedishIdentifyModel) {
  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SwedishIdentifyModel).isRequired
    },

    render: function () {
      var model = this.props.model;
      var buttonStyle = {
        marginTop: "34px"
      };

      return (
        <span>
          {model.statusText()}
          <div className="identify-box-button">
            <Button
              ref="identify-problem-ok"
              style={buttonStyle}
              size="big"
              type="action"
              text={localization.ok}
              onClick={function() {model.back();}}
            />
          </div>
        </span>
      );
    }
  });
});
