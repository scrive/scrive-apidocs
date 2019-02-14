var React = require("react");
var OrderIcon = require("./ordericon");
var RoleIcon = require("./roleicon");
var AuthToViewIcon = require("./authtoviewicon");
var DeliveryIcon = require("./deliveryicon");
var AuthToSignIcon = require("./authtosignicon");
var ConfirmationDeliveryIcon = require("./confirmationdeliveryicon");
var SecondaryConfirmationDeliveryIcon = require("./secondaryconfirmationdeliveryicon");
var AuthToViewArchivedIcon = require("./authtoviewarchivedicon");
var ParticipantSettings = require("./participantsettings");
var ParticipantFields = require("./participantfields");
var Subscription = require("../../account/subscription");
var Track = require("../../common/track");
var _ = require("underscore");

module.exports = React.createClass({
  toogleView: function () {
    var sig = this.props.model;
    if (this.props.currentParticipantDetail === sig) {
      Track.track("Close participant detail");
      this.props.setParticipantDetail(undefined);
    } else {
      Track.track("Open participant detail");
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
    var heightOfUnexpandedSignatory = 42;  // Height each signatory description when signatory is not expanded
    var verticalMarginOfFields = 16;
    var heightOfField = 50; // Height each field row
    // Height of 4 rows of selects at bottom of signatory
    var userFeatures = Subscription.currentSubscription().currentUserFeatures();
    var heightOfParticipantSettings;
    if (userFeatures.canUseDocumentPartyNotifications()) {
      heightOfParticipantSettings = 323;
    } else {
      heightOfParticipantSettings = 285;
    }
    var heightOfExpandedSignatoryHeader = 42;
    var height = heightOfUnexpandedSignatory;

    if (this.props.currentParticipantDetail === sig) {
      height = heightOfExpandedSignatoryHeader + heightOfParticipantBorder;
      height += verticalMarginOfFields;
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
            onClick={function () { self.onRemove(); } }
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
            onClick={function () { self.toogleView(); }}
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
            <SecondaryConfirmationDeliveryIcon model={sig}/>
            <AuthToViewArchivedIcon model={sig}/>
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
