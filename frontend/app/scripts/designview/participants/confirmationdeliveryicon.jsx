/** @jsx React.DOM */

define(['legacy_code', 'React'], function(_Legacy, React) {

return React.createClass({
  onClick: function() {
    var sig = this.props.model;
    mixpanel.track('Choose confirmation delivery method', {
      Where: 'icon'
    });
    if (sig.confirmationdelivery() == "email") {
      sig.setConfirmationDelivery("mobile");
    } else if (sig.confirmationdelivery() == "mobile") {
      sig.setConfirmationDelivery("email_mobile");
    } else if (sig.confirmationdelivery() == "email_mobile") {
      sig.setConfirmationDelivery("none");
    } else {
      sig.setConfirmationDelivery('email');
    }
  },
  icon: function() {
    var sig = this.props.model;
    if (sig.confirmationdelivery() == "email") {
      return  "design-view-action-participant-confirmation-icon-email";
    } else if (sig.confirmationdelivery() == "mobile") {
      return "design-view-action-participant-confirmation-icon-phone";
    } else if (sig.confirmationdelivery() == "email_mobile") {
      return "design-view-action-participant-confirmation-icon-email-mobile";
    } else if (sig.confirmationdelivery() == "none") {
      return "design-view-action-participant-confirmation-icon-empty";
    } 
  },
  render: function() {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-confirmation" onClick={function(e) {self.onClick(); e.stopPropagation();}}>
        <div className="design-view-action-participant-icon-device-inner">
          <div className={"design-view-action-participant-confirmation-icon " + self.icon()}>
          </div>
        </div>
      </div>
    );
  }
});

});
