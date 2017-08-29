var React = require("react");
var Track = require("../../common/track");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose confirmation delivery method", {
      Where: "icon"
    });
    if (sig.confirmationdelivery() == "email") {
      sig.setConfirmationDelivery("mobile");
    } else if (sig.confirmationdelivery() == "mobile") {
      sig.setConfirmationDelivery("email_mobile");
    } else if (sig.confirmationdelivery() == "email_mobile" && !sig.isLastViewer()) {
      sig.setConfirmationDelivery("none");
    } else {
      sig.setConfirmationDelivery("email");
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.confirmationdelivery() == "email") {
      return "design-view-action-participant-confirmation-icon-email";
    } else if (sig.confirmationdelivery() == "mobile") {
      return "design-view-action-participant-confirmation-icon-phone";
    } else if (sig.confirmationdelivery() == "email_mobile") {
      return "design-view-action-participant-confirmation-icon-email-mobile";
    } else if (sig.confirmationdelivery() == "none") {
      return "design-view-action-participant-confirmation-icon-empty";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.confirmation];

    var confirmationMethod = this.props.model.confirmationdelivery();
    if (confirmationMethod == "email") {
      title.push(localization.designview.addParties.confirmationEmail);
    } else if (confirmationMethod == "mobile") {
      title.push(localization.designview.addParties.confirmationSMS);
    } else if (confirmationMethod == "email_mobile") {
      title.push(localization.designview.addParties.confirmationEmailSMS);
    } else if (confirmationMethod == "none") {
      title.push(localization.designview.addParties.confirmationNone);
    }

    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-confirmation"
        onClick={function (e) { self.onClick(); e.stopPropagation(); }}
        title={this.title()}
      >
        <div className="design-view-action-participant-icon-device-inner">
          <div className={"design-view-action-participant-confirmation-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
