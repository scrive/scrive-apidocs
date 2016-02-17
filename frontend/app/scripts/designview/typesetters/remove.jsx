var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");

  var Remove = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    handleClick: function () {
      this.props.model.remove();
      this.props.model.removeField();
    },

    render: function () {
      return (
        <Button
          size="tiny"
          text={localization.designview.textFields.remove}
          className="fieldTypeSetter-button"
          onClick={this.handleClick}
        />
      );
    }
  });

  module.exports = Remove;
