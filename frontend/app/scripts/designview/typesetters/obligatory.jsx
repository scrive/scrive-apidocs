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

      return (
        <div className="fieldTypeSetter-option">
          <Checkbox
            label={localization.designview.textFields.obligatory}
            checked={field.isObligatory()}
            onChange={this.handleChange}
          />
        </div>
      );
    }
  });

  return Obligatory;
});
