var React = require("react");
var Track = require("../../common/track");

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose confirmation delivery method", {
      Where: "secondary_icon"
    });
    var next = sig.confirmationdelivery();
    if (sig.emailConfirmationDelivery()) {
      next = "email_link";
    } else if (sig.emailLinkConfirmationDelivery()) {
      next = "email";
    } else if (sig.emailMobileConfirmationDelivery()) {
      next = "email_link_mobile";
    } else if (sig.emailLinkMobileConfirmationDelivery()) {
      next = "email_mobile";
    }
    sig.setConfirmationDelivery(next);
    if (sig.isDeliverySynchedWithConfirmationDelivery()) {
      sig.synchDelivery();
    }
  },

  icon: function () {
    var sig = this.props.model;
    if (sig.hasConfirmationEmailLink() || sig.mobileConfirmationDelivery()) {
      return "design-view-action-participant-confirmation-icon-link";
    } else if (sig.hasConfirmationEmailAttachments()) {
      return "design-view-action-participant-confirmation-icon-attachments";
    } else {
      return "design-view-action-participant-confirmation-icon-empty";
    }
  },

  title: function () {
    var title = [localization.designview.addParties.secondaryConfirmation];
    var sig = this.props.model;

    if (sig.hasConfirmationEmailLink() || sig.mobileConfirmationDelivery()) {
      title.push(localization.designview.addParties.confirmationLink);
    } else if (sig.hasConfirmationEmailAttachments()) {
      title.push(localization.designview.addParties.confirmationAttachments);
    } else {
      title.push(localization.designview.addParties.confirmationNone);
    }

    return title.join(": ");
  },

  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-confirmation-secondary"
        onClick={function (e) { self.onClick(); e.stopPropagation(); }}
        title={this.title()}
      >
        <div className="design-view-action-participant-icon-device-inner">
          <div className={"design-view-action-participant-confirmation-secondary-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
