var React = require("react");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    mixpanel.track("Choose auth", {
      Where: "icon"
    });
    if (!sig.signs()) {
      new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
    } else if (sig.standardAuthenticationToSign()) {
      sig.setAuthenticationToSign("eleg");
    } else if (sig.seBankIDAuthenticationToSign()) {
      sig.setAuthenticationToSign("sms_pin");
    } else if (sig.smsPinAuthenticationToSign()) {
      sig.setAuthenticationToSign("standard");
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.standardAuthenticationToSign() || !sig.signs()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-noauth";
    } else if (sig.seBankIDAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-se-bankid";
    } else if (sig.smsPinAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-sms-pin";
    }
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-icon-auth-to-sign"
           onClick={function (e) {self.onClick(); e.stopPropagation();}}
      >
        <div className="design-view-action-participant-icon-auth-to-sign-inner">
          <div className={"design-view-action-participant-icon-auth-to-sign-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
