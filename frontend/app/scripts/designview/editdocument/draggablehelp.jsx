define(["legacy_code", "React"], function (legacy_code, React) {
  return React.createClass({
    propTypes: {
      text: React.PropTypes.string.isRequired,
      className: React.PropTypes.string
    },

    render: function () {
      var className = "design-view-action-document-draggables-help " + this.props.className;
      return (
        <div className={className}>
          <div className="wrapper">
            <div className="icon" />
            <div className="text-wrapper">
              <span className="text">
                {this.props.text}
              </span>
            </div>
          </div>
        </div>
      );
    }
  });
});
