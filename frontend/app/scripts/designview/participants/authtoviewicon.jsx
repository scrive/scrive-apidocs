var React = require("react");
var Track = require("../../common/track");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

module.exports = React.createClass({
  onClick: function () {
    var sig = this.props.model;
    Track.track("Choose auth to view", {
      Where: "icon"
    });
    if (!sig.signs()) {
      new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
    } else if (sig.standardAuthenticationToView()) {
      sig.setAuthenticationToView("se_bankid");
    } else if (sig.seBankIDAuthenticationToView()) {
      sig.setAuthenticationToView("no_bankid");
    } else if (sig.noBankIDAuthenticationToView()) {
       sig.setAuthenticationToView("dk_nemid");
    } else if (sig.dkNemIDAuthenticationToView()) {
      sig.setAuthenticationToView("standard");
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
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-icon-auth-to-view"
           onClick={function (e) { self.onClick(); e.stopPropagation(); }}
      >
        <div className="design-view-action-participant-icon-auth-to-view-inner">
          <div className={"design-view-action-participant-icon-auth-to-view-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});
