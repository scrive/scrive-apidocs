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
      var hasName = field.isSignature() || field.isCheckbox() || field.isCustom();

      var done = !hasName || (field.name() && _.all(field.signatory().fields(), function (f) {
        return f.name() != field.name() || f == field;
      }));
      console.log(done);
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
