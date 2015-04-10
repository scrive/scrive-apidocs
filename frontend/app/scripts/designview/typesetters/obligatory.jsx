/** @jsx React.DOM */

define(["Backbone", "React", "common/checkbox", "legacy_code"], function (Backbone, React, Checkbox) {
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
      var label = field.isCheckbox() ? localization.designview.checkboxes.obligatory : localization.designview.textFields.obligatory;

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

  return Obligatory;
});
