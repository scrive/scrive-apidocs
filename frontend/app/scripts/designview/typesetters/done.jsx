var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");

  var Done = React.createClass({
    propTypes: {
      field: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onDone: React.PropTypes.func.isRequired
    },

    handleClick: function () {
      var self = this;
      var field = self.props.field;

      var done = field.name() != undefined && field.name() != "";
      done = done && _.all(field.signatory().fields(), function (f) {
        return f.name() != field.name() || f.type() != field.type() || f == field;
      });

      if (done) { self.props.onDone(); }
    },

    render: function () {
      return (
        <Button
          size="tiny"
          text={localization.designview.textFields.done}
          type="action"
          className="fieldTypeSetter-button"
          onClick={this.handleClick}
        />
      );
    }
  });

  module.exports = Done;
