var _ = require("underscore");
var React = require("react");
var Button = require("../../common/button");
var Track = require("../../common/track");
var Signatory = require("../../../js/signatories.js").Signatory;
var BlockingModal = require("../../blocking/blockingmodal");
var Subscription = require("../../account/subscription");
var CSVSignatoryDesignModal = require("./csvsignatorydesignmodal");

module.exports = React.createClass({
  getInitialState: function () {
    return {
      showCSVSignatoryDesignModal: false
    };
  },
  onDone: function () {
    Track.track("Close participant");
    this.props.setParticipantDetail(undefined);
  },
  addSingleParticipant: function () {
    var currFF = Subscription.currentSubscription().currentUserFeatures();
    var sig = new Signatory({
      document: this.props.document,
      signs: true,
      delivery_method: currFF.firstAllowedInvitationDelivery(),
      confirmation_delivery_method: currFF.firstAllowedConfirmationDelivery(),
      authentication_method_to_view: currFF.firstAllowedAuthenticationToView(),
      authentication_method_to_sign: currFF.firstAllowedAuthenticationToSign()
    });
    this.props.document.addExistingSignatory(sig);
    this.props.setParticipantDetail(sig);
    this.props.onAddSingle();
  },
  addMultisendParticipant: function () {
    if (!Subscription.currentSubscription().currentUserFeatures().canUseMassSendout())  {
      this.refs.blockingModal.openContactUsModal();
    } else {
      Track.track("Click add CSV");
      this.setState({showCSVSignatoryDesignModal: true});
    }
  },
  onCSVSignatoryDesignModalClose: function () {
    this.setState({showCSVSignatoryDesignModal: false});
  },

  render: function () {

    return (
      <div className="design-view-action-participant-new-box-buttons">
        {/* if */ this.props.currentParticipantDetail != undefined &&
          <div className="design-view-action-participant-done">
            <Button
               ref="close-button"
               type="action"
               text={localization.designview.addParties.close}
               onClick={this.onDone}
            />
          </div>
        }
        {/* else */ this.props.currentParticipantDetail == undefined &&
          <div>
            {/* if */ !_.any(this.props.document.signatories(), function (x) { return x.isCsv(); }) &&
              <div className="design-view-action-participant-new-multi">
                <Button
                  ref="add-multi-button"
                  text={localization.designview.addMultisend}
                  onClick={this.addMultisendParticipant}
                  locked={!Subscription.currentSubscription().currentUserFeatures().canUseMassSendout()}
                />
                <BlockingModal ref="blockingModal"/>
              </div>
            }

            <div className="design-view-action-participant-new-single">
              <Button
                ref="add-single-button"
                type="action"
                text={localization.designview.addParty}
                onClick={this.addSingleParticipant}
              />
            </div>

          </div>
        }

        <CSVSignatoryDesignModal
          active={this.state.showCSVSignatoryDesignModal}
          document={this.props.document}
          setParticipantDetail={this.props.setParticipantDetail}
          onClose={this.onCSVSignatoryDesignModalClose}
        />
      </div>
    );
  }
});
