var React = require("react");
var Track = require("../../common/track");
var Subscription = require("../../account/subscription");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose participant role", {
      Where: "icon"
    });
    if (sig.approves() || (sig.views() && sig.author())) {
      var currFF = Subscription.currentSubscription().currentUserFeatures();
      var args = {
        deliveryMethod: currFF.firstAllowedInvitationDelivery(),
        authenticationToView: currFF.firstAllowedAuthenticationToView(),
        authenticationToSign: currFF.firstAllowedAuthenticationToSign()
      };
      sig.makeSignatory(args);
    } else if (sig.signs()) {
      sig.makeViewer();
    } else {
      sig.makeApprover();
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.signs()) {
      return "design-view-action-participant-icon-role-icon-signatory";
    } else if (sig.approves()) {
      return "design-view-action-participant-icon-role-icon-approver";
    } else {
      return "design-view-action-participant-icon-role-icon-viewer";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.role];

    if (this.props.model.signs()) {
      title.push(localization.designview.addParties.roleSignatory);
    } else if (this.props.model.approves()) {
      title.push(localization.designview.addParties.roleApprover);
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
