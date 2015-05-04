/** @jsx React.DOM */

define(["legacy_code", "React"], function (_Legacy, React) {

return React.createClass({
  onClick: function () {
    var sig = this.props.model;
    mixpanel.track("Choose delivery method", {
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
      return "design-view-action-participant-icon-device-icon-pad";
    } else if (sig.delivery() == "mobile") {
      return "design-view-action-participant-icon-device-icon-phone";
    } else if (sig.delivery() == "email_mobile") {
      return "design-view-action-participant-icon-device-icon-email-mobile";
    }
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div
        className="design-view-action-participant-icon-device"
        onClick={function (e) {self.onClick(); e.stopPropagation();}}
      >
        <div className="design-view-action-participant-icon-device-inner">
          <div className={"design-view-action-participant-icon-device-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});

});
