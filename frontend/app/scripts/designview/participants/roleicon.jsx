var React = require("react");
var Track = require("../../common/track");
var Subscription = require("../../account/subscription");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose participant role", {
      Where: "icon"
    });
    if (!sig.signs()) {
      var currFF = Subscription.currentSubscription().currentUserFeatures();
      var args = {
        authenticationToView: currFF.firstAllowedAuthenticationToView(),
        authenticationToSign: currFF.firstAllowedAuthenticationToSign()
      };
      sig.makeSignatory(args);
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
  title: function () {
    var title = [localization.designview.addParties.role];

    if (this.props.model.signs()) {
      title.push(localization.designview.addParties.roleSignatory);
    } else {
      title.push(localization.designview.addParties.roleViewer);
    }

    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-role"
        onClick={function (e) { self.onClick(); e.stopPropagation(); }}
        title={this.title()}
      >
        <div className="design-view-action-participant-icon-role-inner">
          <div className={"design-view-action-participant-icon-role-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
