/** @jsx React.DOM */

define(["legacy_code", "React", "designview/participants/ordericon", "designview/participants/roleicon",
        "designview/participants/deliveryicon", "designview/participants/authicon",
        "designview/participants/confirmationdeliveryicon", "designview/participants/participantsettings",
        "designview/participants/participantfields"],
function (_Legacy, React, OrderIcon, RoleIcon,
          DeliveryIcon, AuthIcon,
          ConfirmationDeliveryIcon, ParticipantSettings,
          ParticipantFields) {

return React.createClass({
  toogleView: function () {
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;
    if (viewmodel.participantDetail() === sig) {
      mixpanel.track("Close participant detail");
      viewmodel.setParticipantDetail(undefined);
    } else {
      mixpanel.track("Open participant detail");
      viewmodel.setParticipantDetail(sig);
    }
  },
  onRemove: function () {
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;
    viewmodel.setParticipantDetail(undefined);
    _.each(sig.fields(), function (field) {
      field.removeAllPlacements();
    });
    sig.document().removeSignatory(sig);
  },
  signatoryHasProblems: function () {
    return _.any(this.props.model.fields(), function (field) {
      return !field.isValid();
    });
  },
  // We need to compute height in JS - height auto in css would be enought
  // but it will not work with hide/show css transitions.
  height: function () {
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;
    var heightOfUnexpandedSignatory = 46;  // Height each signatory description when signatory is not expanded
    var heightOfField = 48; // Height each field row
    var heightOfParticipantSettings = 112; // Height of 5 selects at bottom of signatory
    var height = heightOfUnexpandedSignatory;

    if (viewmodel.participantDetail() === sig) {
      height += heightOfParticipantSettings;
      var fields = 0;
      var nameIncluded = false;
      _.each(sig.fields(), function (f) {
        if (f.isFstName() || f.isSndName()) {
          if (!nameIncluded) {
            nameIncluded = true;
            fields++;
          }
        } else if (f.isText() || f.isBlank()) {
          fields++;
        }
      });

      height += Math.ceil((fields + 1) / 3) * heightOfField;
    }

    return height;
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    var viewmodel = this.props.viewmodel;

    return (
      <div className={"design-view-action-participant"}>
        {/* if */ !sig.author() &&
          <div
            className="design-view-action-participant-close"
            ref="remove-icon"
            onClick={function () {self.onRemove();} }
          />
        }

        <div
          className={
            "design-view-action-participant-inner " +
            (viewmodel.participantDetail() === sig ? "expanded " : "")  +
            (self.signatoryHasProblems() ? "is-has-problems" : "")}
            style={{height: this.height()}}
        >
          <div
            ref="participant-details"
            className="design-view-action-participant-info-box"
            onClick={function () {self.toogleView();}}
          >
            <div
              className={
                "design-view-action-participant-info-color " +
                ("participant-" +  ((sig.participantIndex() - 1) % 6 + 1))
              }
            />
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
