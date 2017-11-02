var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

module.exports = React.createClass({
  isAllowedAuthenticationMethod: function (am) {
    if (!Subscription.currentSubscription().canUseSEAuthenticationToSign() && am == "se_bankid") {
      return false;
    } else if (!Subscription.currentSubscription().canUseSMSPinAuthenticationToSign() && am == "sms_pin") {
      return false;
    } else {
      return true;
    }
  },
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose auth", {
      Where: "icon"
    });
    if (!sig.signs()) {
      new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
    } else {
      var ams = ["standard", "se_bankid", "sms_pin"];
      var i = (_.indexOf(ams, sig.authenticationToSign()) + 1) || 0;
      while (!this.isAllowedAuthenticationMethod(ams[i % ams.length])) {
        i++;
      }

      var newAuthToView = ams[i % ams.length];
      if (sig.authenticationToSign() == newAuthToView && newAuthToView == "standard") {
        this.refs.blockingModal.openContactUsModal();
      } else {
        sig.setAuthenticationToSign(newAuthToView);
      }

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
  title: function () {
    var title = [localization.designview.addParties.authenticationToSign];

    var authMethod = (
      this.props.model.signs() ? this.props.model.authenticationToSign() : "standard"
    );

    if (authMethod == "standard") {
      title.push(
        localization.designview.addParties.authenticationToSignStandard
      );
    } else if (authMethod == "se_bankid") {
      title.push(
        localization.designview.addParties.authenticationToSignSEBankID
      );
    } else if (authMethod == "sms_pin") {
      title.push(
        localization.designview.addParties.authenticationToSignSMSPin
      );
    }

    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-icon-auth-to-sign"
           onClick={function (e) { self.onClick(); e.stopPropagation(); }}
           title={this.title()}
      >
        <div className="design-view-action-participant-icon-auth-to-sign-inner">
          <div className={"design-view-action-participant-icon-auth-to-sign-icon " + self.icon()}>
          </div>
        </div>
        <BlockingModal ref="blockingModal"/>
      </div>
    );
  }
});
