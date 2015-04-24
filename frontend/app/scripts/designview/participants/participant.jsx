/** @jsx React.DOM */

define(['legacy_code', 'React', 'designview/participants/ordericon', 'designview/participants/roleicon', 'designview/participants/deliveryicon', 'designview/participants/authicon', 'designview/participants/confirmationdeliveryicon'], function(_Legacy, React,OrderIcon,RoleIcon,DeliveryIcon, AuthIcon, ConfirmationDeliveryIcon) {

return React.createClass({
  toogleView: function() {
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;
    if(viewmodel.participantDetail() === sig) {
       mixpanel.track('Close participant detail');
       viewmodel.setParticipantDetail(undefined);
    } else {
       mixpanel.track('Open participant detail');
       viewmodel.setParticipantDetail(sig);
    }
  },
  name: function() {
    if(this.props.model.isCsv()) {
      return localization.csv.title;
    } else {
      return this.props.model.name();
    }
  },
  render: function() {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant">
        <div className="design-view-action-participant-inner">
          <div className="design-view-action-participant-info-box" onClick={function() {self.toogleView();}}>
            <div className={"design-view-action-participant-info-color " + ("participant-" +  ((sig.participantIndex() -1 ) % 6 + 1))} />
            <div className="design-view-action-participant-info-name">
              <div className="design-view-action-participant-info-name-inner">
                {self.name()}
              </div>
            </div>
            <div className="design-view-action-participant-info-email">
              <div className="design-view-action-participant-info-email-inner">
                {sig.email()}
              </div>
            </div>
            <div className="design-view-action-participant-info-company">
              <div className="design-view-action-participant-info-company-inner">
                {sig.company()}
              </div>
            </div>
            <OrderIcon model={sig}/>
            <DeliveryIcon model={sig}/>
            <RoleIcon model={sig}/>
            <AuthIcon model={sig}/>
            <ConfirmationDeliveryIcon model={sig}/>
          </div>
        </div>
      </div>
    );
  }
});

});


