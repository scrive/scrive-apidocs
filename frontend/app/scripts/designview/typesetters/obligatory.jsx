var Backbone = require("backbone");
var React = require("react");
var Checkbox = require("../../common/checkbox");

  var Obligatory = React.createClass({
    propTypes: {
      field: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    handleChange: function () {
      var field = this.props.field;

      if (field.isObligatory()) {
        field.makeOptional();
      } else {
        field.makeObligatory();
      }
    },

    render: function () {
      var field = this.props.field;
      var label = localization.designview.textFields.obligatory;

      if (field.isCheckbox()) {
        label = localization.designview.checkboxes.obligatory;
      }

      return (
        <div className="fieldTypeSetter-option">
          <Checkbox
            label={label}
            checked={field.isObligatory()}
            onChange={this.handleChange}
          />
        </div>
      );
    }
  });

  module.exports = Obligatory;
