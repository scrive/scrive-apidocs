var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var SwedishIdentifyModel = require("./swedishidentifymodel");
  module.exports = React.createClass({
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
              onClick={function () { model.back(); }}
            />
          </div>
        </span>
      );
    }
  });
