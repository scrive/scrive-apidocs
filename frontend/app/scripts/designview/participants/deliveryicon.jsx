var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Subscription = require("../../account/subscription");

module.exports = React.createClass({
  isAllowedDeliveryMethod: function (dm) {
    if (!Subscription.currentSubscription().canUseSMSInvitations()) {
      return dm != "mobile" && dm != "email_mobile";
    } else {
      return true;
    }
  },
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose delivery method", {
      Where: "icon"
    });
    if (sig.isLastViewer()) {
      new FlashMessage({type: "error", content: localization.designview.lastViewerOnlyGetsConfirmation});
    } else {
      var dms = ["email", "pad", "mobile", "email_mobile", "api"];
      var i = (_.indexOf(dms, sig.delivery()) + 1) || 0;
      while (!this.isAllowedDeliveryMethod(dms[i % dms.length])) {
        i++;
      }
      sig.setDelivery(dms[i % dms.length]);
      if (sig.isConfirmationDeliverySynchedWithDelivery()) {
        this.synchConfirmationDelivery();
      }
    }
  },
  synchConfirmationDelivery: function () {
    var sig = this.props.model;
    if (sig.emailDelivery()) {
      sig.setConfirmationDeliverySynchedWithDelivery("email");
    } else if (sig.mobileDelivery()  && Subscription.currentSubscription().canUseSMSConfirmations()) {
      sig.setConfirmationDeliverySynchedWithDelivery("mobile");
    } else if (sig.emailMobileDelivery() && Subscription.currentSubscription().canUseSMSConfirmations()) {
      sig.setConfirmationDeliverySynchedWithDelivery("email_mobile");
    } else if (sig.padDelivery()) {
      sig.setConfirmationDeliverySynchedWithDelivery("email");
    } else if (sig.apiDelivery()) {
      sig.setConfirmationDeliverySynchedWithDelivery("email");
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.isLastViewer()) {
      return "design-view-action-participant-icon-device-icon-empty";
    } else if (sig.delivery() == "email") {
      return "design-view-action-participant-icon-device-icon-email";
    } else if (sig.delivery() == "pad") {
      return "design-view-action-participant-icon-device-icon-pad";
    } else if (sig.delivery() == "api") {
      return "design-view-action-participant-icon-device-icon-api";
    } else if (sig.delivery() == "mobile") {
      return "design-view-action-participant-icon-device-icon-phone";
    } else if (sig.delivery() == "email_mobile") {
      return "design-view-action-participant-icon-device-icon-email-mobile";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.invitation];

    var deliveryMethod = (
      this.props.model.isLastViewer() ? "standard" : this.props.model.delivery()
    );

    if (deliveryMethod == "none") {
      title.push(localization.designview.addParties.invitationNone);
    } else if (deliveryMethod == "email") {
      title.push(localization.designview.addParties.invitationEmail);
    } else if (deliveryMethod == "pad") {
      title.push(localization.designview.addParties.invitationInPerson);
    } else if (deliveryMethod == "mobile") {
      title.push(localization.designview.addParties.invitationSMS);
    } else if (deliveryMethod == "email_mobile") {
      title.push(localization.designview.addParties.invitationEmailSMS);
    } else if (deliveryMethod == "api") {
      title.push(localization.designview.addParties.invitationLink);
    }

    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-device"
        onClick={function (e) { self.onClick(); e.stopPropagation(); }}
        title={this.title()}
      >
        <div className="design-view-action-participant-icon-device-inner">
          <div className={"design-view-action-participant-icon-device-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
