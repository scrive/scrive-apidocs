var React = require("react");
var Track = require("../../common/track");
var Subscription = require("../../account/subscription");

module.exports = React.createClass({
  isAllowedConfirmationMethod: function (dm) {
    var sig = this.props.model;
    if (sig.isLastViewer() && sig.confirmationdelivery() == "none") {
      return false;
    } else if (!Subscription.currentSubscription().currentUserFeatures().canUseSMSConfirmations()) {
      return dm != "mobile" && dm != "email_mobile";
    } else {
      return true;
    }
  },
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose confirmation delivery method", {
      Where: "icon"
    });
    var cdms = ["email", "mobile", "email_mobile", "none"];
    var i = (_.indexOf(cdms, sig.confirmationdelivery()) + 1) || 0;
    while (!this.isAllowedConfirmationMethod(cdms[i % cdms.length])) {
      i++;
    }
    sig.setConfirmationDelivery(cdms[i % cdms.length]);
    if (sig.isDeliverySynchedWithConfirmationDelivery()) {
      this.synchDelivery();
    }
  },
  synchDelivery: function () {
    var sig = this.props.model;
    if (sig.emailConfirmationDelivery() &&
        Subscription.currentSubscription().currentUserFeatures().canUseEmailInvitations()) {
      sig.setDeliverySynchedWithConfirmationDelivery("email");
    } else if (sig.mobileConfirmationDelivery() &&
        Subscription.currentSubscription().currentUserFeatures().canUseSMSInvitations()) {
      sig.setDeliverySynchedWithConfirmationDelivery("mobile");
    } else if (sig.emailMobileConfirmationDelivery() &&
        Subscription.currentSubscription().currentUserFeatures().canUseSMSInvitations() &&
        Subscription.currentSubscription().currentUserFeatures().canUseEmailInvitations()) {
      sig.setDeliverySynchedWithConfirmationDelivery("email_mobile");
    } else if (sig.noneConfirmationDelivery() &&
        Subscription.currentSubscription().currentUserFeatures().canUseEmailInvitations()) {
      sig.setDeliverySynchedWithConfirmationDelivery("email");
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
