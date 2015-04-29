/** @jsx React.DOM */

define(['legacy_code', 'React', 'designview/participants/ordericon', 'designview/participants/roleicon', 'designview/participants/deliveryicon', 'designview/participants/authicon', 'designview/participants/confirmationdeliveryicon','designview/participants/participantsettings','designview/participants/participantfields'], function(_Legacy, React, OrderIcon,RoleIcon,DeliveryIcon, AuthIcon, ConfirmationDeliveryIcon,ParticipantSettings,ParticipantFields) {

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
  onRemove: function() {
     var sig = this.props.model;
     var viewmodel = this.props.viewmodel;
     viewmodel.setParticipantDetail(undefined);
     _.each(sig.fields(), function(field) {
        field.removeAllPlacements();
     });
     sig.document().removeSignatory(sig);
  },
  signatoryHasProblems: function() {
    return _.any( this.props.model.fields(), function(field) {
      return !field.isValid();
    });
  },
  render: function() {
    var self = this;
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;

    return (
      <div className={"design-view-action-participant"}>
        {/* if */ !sig.author() &&
          <div className="design-view-action-participant-close" onClick={function() {self.onRemove();} }/>
        }

        <div className={"design-view-action-participant-inner " + (viewmodel.participantDetail() === sig ? "expanded " : "")  + (self.signatoryHasProblems() ? "is-has-problems" : "")}>
          <div className="design-view-action-participant-info-box" onClick={function() {self.toogleView();}}>
            <div className={"design-view-action-participant-info-color " + ("participant-" +  ((sig.participantIndex() -1 ) % 6 + 1))} />
            <div className="design-view-action-participant-info-name">
              <div className="design-view-action-participant-info-name-inner">
                {sig.isCsv() ? localization.csv.title : sig.name()}
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
          <div className="design-view-action-participant-details">
            <ParticipantFields model={sig} viewmodel={this.props.viewmodel}/>
            <ParticipantSettings model={sig}/>
          </div>
        </div>
      </div>
    );
  }
});

});


