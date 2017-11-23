var React = require("react");

var ValidationErrorMessageView = React.createClass({
  render: function () {
    return (
      <div className="failed-validation validation-failed-msg">
        {this.props.children}
      </div>
    );
  }
});

module.exports = ValidationErrorMessageView;
