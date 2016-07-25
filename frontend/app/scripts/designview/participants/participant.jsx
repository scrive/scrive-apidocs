var React = require("react");
var OrderIcon = require("./ordericon");
var RoleIcon = require("./roleicon");
var AuthToViewIcon = require("./authtoviewicon");
var DeliveryIcon = require("./deliveryicon");
var AuthToSignIcon = require("./authtosignicon");
var ConfirmationDeliveryIcon = require("./confirmationdeliveryicon");
var ParticipantSettings = require("./participantsettings");
var ParticipantFields = require("./participantfields");
var _ = require("underscore");

module.exports = React.createClass({
  toogleView: function () {
    var sig = this.props.model;
    if (this.props.currentParticipantDetail === sig) {
      mixpanel.track("Close participant detail");
      this.props.setParticipantDetail(undefined);
    } else {
      mixpanel.track("Open participant detail");
      var oldSig = this.props.currentParticipantDetail;
      this.props.setParticipantDetail(sig);
      if (this.props.onExpand !== undefined) {
        this.props.onExpand(oldSig);
      }
    }
  },
  onRemove: function () {
    var sig = this.props.model;
    this.props.setParticipantDetail(undefined);
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
  // We need to compute height in JS - height auto in css would be enough
  // but it will not work with hide/show css transitions.
  height: function () {
    var sig = this.props.model;
    var heightOfParticipantBorder = 4;
    var heightOfUnexpandedSignatory = 46;  // Height each signatory description when signatory is not expanded
    var heightOfField = 50; // Height each field row
    var heightOfParticipantSettings = 172; // Height of 6 selects at bottom of signatory
    var heightOfExpandedSignatoryHeader = 58;
    var height = heightOfUnexpandedSignatory;

    if (this.props.currentParticipantDetail === sig) {
      height = heightOfExpandedSignatoryHeader + heightOfParticipantBorder;
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
            (self.props.currentParticipantDetail === sig ? "expanded " : "")  +
            (self.signatoryHasProblems() ? "is-has-problems" : "")}
            style={{height: self.height()}}
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
            <AuthToViewIcon model={sig}/>
            <RoleIcon model={sig}/>
            <AuthToSignIcon model={sig}/>
            <ConfirmationDeliveryIcon model={sig}/>
          </div>
          <div className="design-view-action-participant-details">
            <ParticipantFields
              model={sig}
              document={this.props.document}
              setParticipantDetail={this.props.setParticipantDetail}
            />
            <ParticipantSettings model={sig}/>
          </div>
        </div>
      </div>
    );
  }
});
