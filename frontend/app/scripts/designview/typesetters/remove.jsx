/** @jsx React.DOM */

define(["Underscore", "Backbone", "React", "common/button", "legacy_code"], function (_, Backbone, React, Button) {
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

  return Remove;
});
