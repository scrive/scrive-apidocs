var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

module.exports = React.createClass({
  isAllowedAuthenticationMethod: function (am) {
    if (!Subscription.currentSubscription().canUseSEAuthenticationToView() && am == "se_bankid") {
      return false;
    } else if (!Subscription.currentSubscription().canUseNOAuthenticationToView() && am == "no_bankid") {
      return false;
    } else if (!Subscription.currentSubscription().canUseDKAuthenticationToView() && am == "dk_nemid") {
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
    if (!sig.signs()) {
      new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
    } else {
      var ams = ["standard", "se_bankid", "no_bankid", "dk_nemid"];
      var i = (_.indexOf(ams, sig.authenticationToView()) + 1) || 0;
      while (!this.isAllowedAuthenticationMethod(ams[i % ams.length])) {
        i++;
      }
      var newAuthToView = ams[i % ams.length];
      if (sig.authenticationToView() == newAuthToView && newAuthToView == "standard") {
        this.refs.blockingModal.openContactUsModal();
      } else {
        sig.setAuthenticationToView(newAuthToView);
      }
    }
  },
  icon: function () {
    var sig = this.props.model;
    if (sig.standardAuthenticationToView() || !sig.signs()) {
      return "design-view-action-participant-icon-auth-to-view-icon-noauth";
    } else if (sig.seBankIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-se-bankid";
    } else if (sig.noBankIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-no-bankid";
    } else if (sig.dkNemIDAuthenticationToView()) {
      return "design-view-action-participant-icon-auth-to-view-icon-dk-nemid";
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
