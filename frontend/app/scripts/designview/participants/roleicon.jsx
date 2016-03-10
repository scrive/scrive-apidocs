var React = require("react");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    mixpanel.track("Choose participant role", {
      Where: "icon"
    });
    if (!sig.signs()) {
      sig.makeSignatory();
    } else {
      sig.makeViewer();
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (!sig.signs()) {
      return "design-view-action-participant-icon-role-icon-viewer";
    } else {
      return "design-view-action-participant-icon-role-icon-signatory";
    }
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-role"
        onClick={function (e) {self.onClick(); e.stopPropagation();}}
      >
        <div className="design-view-action-participant-icon-role-inner">
          <div className={"design-view-action-participant-icon-role-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
