var React = require("react");
var Track = require("../../common/track");
var Subscription = require("../../account/subscription");

module.exports = React.createClass({
  isAllowedConfirmationMethod: function (dm) {
    var sig = this.props.model;
    if (sig.isLastViewer() && dm === "none") {
      return false;
    } else if (!Subscription.currentSubscription().currentUserFeatures().canUseSMSConfirmations()) {
      return dm != "mobile" && dm != "email_mobile" && dm != "email_link_mobile";
    } else if (!Subscription.currentSubscription().currentUserFeatures().canUseEmailConfirmations()) {
      return dm != "email" && dm != "email_mobile" && dm != "email_link_mobile";
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
    var current = sig.confirmationdelivery();
    if (current == "email_link") {
      current = "email";
    } else if (current == "email_link_mobile") {
      current = "email_mobile";
    }
    var i = (_.indexOf(cdms, current) + 1) || 0;
    // <100 is just to make sure that in case of bugs we will not spin forever
    while (!this.isAllowedConfirmationMethod(cdms[i % cdms.length]) && i < 100) {
      i++;
    }
    sig.setConfirmationDelivery(cdms[i % cdms.length]);
    if (sig.isDeliverySynchedWithConfirmationDelivery()) {
      sig.synchDelivery();
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.anyEmailConfirmationDelivery()) {
      return "design-view-action-participant-confirmation-icon-email";
    } else if (sig.mobileConfirmationDelivery()) {
      return "design-view-action-participant-confirmation-icon-phone";
    } else if (sig.anyEmailMobileConfirmationDelivery()) {
      return "design-view-action-participant-confirmation-icon-email-mobile";
    } else if (sig.noneConfirmationDelivery()) {
      return "design-view-action-participant-confirmation-icon-empty";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.confirmation];
    var sig = this.props.model;

    if (sig.anyEmailConfirmationDelivery()) {
      title.push(localization.designview.addParties.confirmationEmail);
    } else if (sig.mobileConfirmationDelivery()) {
      title.push(localization.designview.addParties.confirmationSMS);
    } else if (sig.anyEmailMobileConfirmationDelivery()) {
      title.push(localization.designview.addParties.confirmationEmailSMS);
    } else if (sig.noneConfirmationDelivery()) {
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
