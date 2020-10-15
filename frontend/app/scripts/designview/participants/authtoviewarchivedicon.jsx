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
    } else if (!ff.canUseDKCPRAuthenticationToView() && am == "dk_nemid_cpr") {
      return false;
    } else if (!ff.canUseDKPIDAuthenticationToView() && am == "dk_nemid_pid") {
      return false;
    } else if (!ff.canUseDKCVRAuthenticationToView() && am == "dk_nemid_cvr") {
      return false;
    } else if (!ff.canUseFIAuthenticationToView() && am == "fi_tupas") {
      return false;
    } else if (!ff.canUseSMSPinAuthenticationToView() && am == "sms_pin") {
      return false;
    } else if (!ff.canUseVerimiAuthenticationToView() && am == "verimi") {
      return false;
    } else if (!ff.canUseIDINAuthenticationToView() && am == "nl_idin") {
      return false;
    } else {
      return true;
    }
  },
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose auth to view archived", {
      Where: "icon"
    });
    var superthis = this;
    var ams = ["standard", "se_bankid", "no_bankid", "dk_nemid_pid", "dk_nemid_cpr",
               "dk_nemid_cvr", "fi_tupas", "sms_pin", "verimi", "nl_idin"]
              .filter(function (am) { return superthis.isAllowedAuthenticationMethod(am); });
    if (ams.length <= 1) {
      // if no auth methods are enabled, tell customer, that they can purchase them
      this.refs.blockingModal.openContactUsModal();
    } else {
      ams = ams.filter(function (am) {
        return sig.authenticationMethodsCanMix(sig.authenticationToView(), sig.authenticationToSign(), am);
      });
      // newIndex will be 0 ("standard") if the selected auth method is not enabled anymore
      var newIndex = _.indexOf(ams, sig.authenticationToViewArchived()) + 1;
      // the new auth can stay the same ("standard"), if the other methods are not compatible
      // with auth to view
      sig.setAuthenticationToViewArchived(ams[newIndex % ams.length]);
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.standardAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-noauth";
    } else if (sig.seBankIDAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-se-bankid";
    } else if (sig.noBankIDAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-no-bankid";
    } else if (sig.dkNemIDCPRAuthenticationToViewArchived()
              || sig.dkNemIDPIDAuthenticationToViewArchived()
              || sig.dkNemIDCVRAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-dk-nemid";
    } else if (sig.fiTupasAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-fi-tupas";
    } else if (sig.smsPinAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-sms-pin";
    } else if (sig.verimiAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-verimi";
    } else if (sig.idinAuthenticationToViewArchived()) {
      return "design-view-action-participant-icon-auth-to-view-icon-idin";
    }
  },
  title: function () {
    var title = [localization.designview.addParties.authenticationToViewArchived];

    var authMethod = (
      this.props.model.signs() ? this.props.model.authenticationToViewArchived() : "standard"
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
    } else if (authMethod == "dk_nemid_cpr") {
      title.push(
        localization.designview.addParties.authenticationToViewDKNemIDCPR
      );
    } else if (authMethod == "dk_nemid_pid") {
      title.push(
        localization.designview.addParties.authenticationToViewDKNemIDPID
      );
   } else if (authMethod == "dk_nemid_cvr") {
      title.push(
        localization.designview.addParties.authenticationToViewDKNemIDCVR
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
    } else if (authMethod == "nl_idin") {
      title.push(
        localization.designview.addParties.authenticationToViewIDIN
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
