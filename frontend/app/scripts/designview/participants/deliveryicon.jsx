var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose delivery method", {
      Where: "icon"
    });
    if (sig.isLastViewer()) {
      new FlashMessage({type: "error", content: localization.designview.lastViewerOnlyGetsConfirmation});
    } else if (sig.delivery() == "email") {
      sig.setDelivery("pad");
    } else if (sig.delivery() == "pad") {
      sig.setDelivery("mobile");
    } else if (sig.delivery() == "mobile") {
      sig.setDelivery("email_mobile");
    } else if (sig.delivery() == "email_mobile") {
      sig.setDelivery("api");
    } else {
      sig.setDelivery("email");
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
      title.push(localization.designview.addParties.invitationAPI);
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
