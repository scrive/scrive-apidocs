var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

module.exports = React.createClass({
  isAllowedAuthenticationMethod: function (am) {
    var ff = Subscription.currentSubscription().currentUserFeatures();
    if (!ff.canUseStandardAuthenticationToView() && am == "standard") {
      return false;
    } else  if (!ff.canUseSEAuthenticationToView() && am == "se_bankid") {
      return false;
    } else if (!ff.canUseNOAuthenticationToView() && am == "no_bankid") {
      return false;
    } else if (!ff.canUseDKAuthenticationToView() && am == "dk_nemid") {
      return false;
    } else if (!ff.canUseFIAuthenticationToView() && am == "fi_tupas") {
      return false;
    } else if (!ff.canUseSMSPinAuthenticationToView() && am == "sms_pin") {
      return false;
    } if (!ff.canUseVerimiAuthenticationToView() && am == "verimi") {
      return false;
    } else {
      return true;
    }
  },
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose auth to view", {
      Where: "icon"
    });

    var superthis = this;
    var ams = ["standard", "se_bankid", "no_bankid", "dk_nemid", "fi_tupas", "sms_pin", "verimi"]
              .filter(function (am) { return superthis.isAllowedAuthenticationMethod(am); });
    if (ams.length <= 1) {
      // if no auth methods are enabled, tell customer, that they can purchase them
      this.refs.blockingModal.openContactUsModal();
    } else {
      ams = ams.filter(function (am) {
        return sig.authenticationMethodsCanMix(am, sig.authenticationToSign(), sig.authenticationToViewArchived());
      });
      // newIndex will be 0 ("standard") if the selected auth method is not enabled anymore
      var newIndex = _.indexOf(ams, sig.authenticationToView()) + 1;
      // the new auth can stay the same ("standard"), if the other methods are not compatible
      // with auth to view
      sig.setAuthenticationToView(ams[newIndex % ams.length]);
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.standardAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-noauth";
    } else if (sig.seBankIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-se-bankid";
    } else if (sig.noBankIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-no-bankid";
    } else if (sig.dkNemIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-dk-nemid";
    } else if (sig.fiTupasAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-fi-tupas";
    } else if (sig.smsPinAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-sms-pin";
    } else if (sig.verimiAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-verimi";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.authenticationToView];

    var authMethod = (
      this.props.model.signs() ? this.props.model.authenticationToView() : "standard"
    );

    if (authMethod == "standard") {
      title.push(
        localization.designview.addParties.authenticationToViewStandard
      );
    } else if (authMethod == "se_bankid") {
      title.push(
        localization.designview.addParties.authenticationToViewSEBankID
      );
    } else if (authMethod == "no_bankid") {
      title.push(
        localization.designview.addParties.authenticationToViewNOBankID
      );
    } else if (authMethod == "dk_nemid") {
      title.push(
        localization.designview.addParties.authenticationToViewDKNemID
      );
    } else if (authMethod == "fi_tupas") {
      title.push(
        localization.designview.addParties.authenticationToViewFITupas
      );
    } else if (authMethod == "sms_pin") {
      title.push(
        localization.designview.addParties.authenticationToViewSMSPin
      );
    } else if (authMethod == "verimi") {
      title.push(
        localization.designview.addParties.authenticationToViewVerimi
      );
    }
    return title.join(": ");
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-icon-auth-to-view"
           onClick={function (e) { self.onClick(); e.stopPropagation(); }}
           title={this.title()}
      >
        <div className="design-view-action-participant-icon-auth-to-view-inner">
          <div className={"design-view-action-participant-icon-auth-to-view-icon " + self.icon()}>
          </div>
        </div>
        <BlockingModal ref="blockingModal"/>
      </div>
    );
  }
});
