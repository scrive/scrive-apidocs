/** @jsx React.DOM */

define(['legacy_code', 'React'], function(_Legacy, React) {

return React.createClass({
  onClick: function() {
    var sig = this.props.model;
    mixpanel.track('Choose auth', {
      Where: 'icon'
    });
    if(sig.isLastViewer()) {
       new FlashMessage({type: "error", content : localization.designview.lastViewerOnlyGetsConfirmation});
    } else if (sig.authentication() == "standard") {
      sig.setAuthentication('eleg');
    } else if (sig.authentication() == "eleg") {
      sig.setAuthentication('sms_pin');
    } else if (sig.authentication() == "sms_pin") {
      sig.setAuthentication('standard');
    }
  },
  icon: function() {
    var sig = this.props.model;
    if (sig.authentication() == "standard") {
      return "design-view-action-participant-icon-auth-icon-noauth";
    } else if (sig.authentication() == "eleg") {
      return  "design-view-action-participant-icon-auth-icon-eleg";
    } else if (sig.authentication() == "sms_pin") {
      return "design-view-action-participant-icon-auth-icon-sms-pin";
    }
  },
  render: function() {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-icon-auth" onClick={function(e) {self.onClick(); e.stopPropagation();}}>
        <div className="design-view-action-participant-icon-auth-inner">
          <div className={"design-view-action-participant-icon-auth-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});

});
