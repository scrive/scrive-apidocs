var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

module.exports = React.createClass({
  isAllowedAuthenticationMethod: function (am) {
    var ff = Subscription.currentSubscription().currentUserFeatures();
    if (!ff.canUseStandardAuthenticationToSign() && am == "standard") {
      return false;
    } else if (!ff.canUseSEAuthenticationToSign() && am == "se_bankid") {
      return false;
    } else if (!ff.canUseNOAuthenticationToSign() && am == "no_bankid") {
      return false;
    } else if (!ff.canUseDKAuthenticationToSign() && am == "dk_nemid") {
      return false;
    } else if (!ff.canUseSMSPinAuthenticationToSign() && am == "sms_pin") {
      return false;
    } else if (!ff.canUseIDINAuthenticationToSign() && am == "nl_idin") {
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
      new FlashMessage(
        {type: "error",
         content: localization.designview.notAllowedToChangeAuthenticationMethod
        });
    } else {
      var superthis = this;
      var ams = [
        "standard", "se_bankid", "no_bankid", "dk_nemid", "nl_idin", "sms_pin"
      ]
      .filter(function (am) {
          return superthis.isAllowedAuthenticationMethod(am);
      });
      if (ams.length <= 1) {
        // if no auth methods are enabled, tell customer, that they can purchase them
        this.refs.blockingModal.openContactUsModal();
      } else {
        ams = ams.filter(function (am) {
            return sig.authenticationMethodsCanMix(sig.authenticationToView(), am, sig.authenticationToViewArchived());
         });
        // newIndex will be 0 ("standard") if the selected method is not enabled anymore
        var newIndex = _.indexOf(ams, sig.authenticationToSign()) + 1;
        // the new auth can stay the same ("standard"), if the other methods are not compatible
        // with auth to view
        sig.setAuthenticationToSign(ams[newIndex % ams.length]);
      }
    }

  },
  icon: function () {
    var sig = this.props.model;
    if (sig.standardAuthenticationToSign() || !sig.signs()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-noauth";
    } else if (sig.seBankIDAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-se-bankid";
    } else if (sig.noBankIDAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-no-bankid";
    } else if (sig.dkNemIDAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-dk-nemid";
    } else if (sig.smsPinAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-sms-pin";
    } else if (sig.nlIDINAuthenticationToSign()) {
      return "design-view-action-participant-icon-auth-to-sign-icon-idin";
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
    } else if (authMethod == "no_bankid") {
      title.push(
        localization.designview.addParties.authenticationToSignNOBankID
      );
    } else if (authMethod == "dk_nemid") {
      title.push(
        localization.designview.addParties.authenticationToSignDKNemID
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
