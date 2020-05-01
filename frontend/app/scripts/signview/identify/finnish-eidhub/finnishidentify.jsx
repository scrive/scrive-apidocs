var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var FinnishIdentifyModel = require("./finnishidentifymodel");

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(FinnishIdentifyModel).isRequired
  },
  handleIdentify: function () {
    this.props.model.identify();
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
              text={localization.identify.identifyWithTupas}
              onClick={this.handleIdentify}
            />
          </div>
        </div>
      </span>
    );
  }
});
