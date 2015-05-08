/** @jsx React.DOM */

define(["legacy_code", "React", "common/backbone_mixin",
        "designview/participants/participant", "designview/participants/addparticipants"],
function (_Legacy, React, BackboneMixin,
          Participant, AddParticipants) {

return React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function () {
    return [this.props.model, this.props.model.document()];
  },
  getInitialState: function () {
    return {needsToScrollToBottom: false};
  },
  maxHeight: function () {
    var heightOfNavigationTabs = 350;  // Height of navigation tabs on page.
    return Math.max(250, $(window).height() - heightOfNavigationTabs);
  },
  // Height of participants needs to be computed for scrollbar.
  // Height of whole secton and each field is hardcoded
  participantsHeight: function () {
    var heightOfUnexpandedSignatory = 60;  // Height each signatory description when signatory is not expanded
    var heightOfField = 48; // Height each field row
    var heightOfParticipantSettings = 112; // Height of 5 selects at bottom of signatory
    var height = 0;

    height += this.props.model.document().signatories().length * heightOfUnexpandedSignatory;

    if (this.props.model.participantDetail() != undefined) {
      height += heightOfParticipantSettings;
      var fields = 0;
      var nameIncluded = false;
      _.each(this.props.model.participantDetail().fields(), function (f) {
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
  currentHeight: function () {
    return Math.min(this.participantsHeight(), this.maxHeight()) + "px";
  },
  hasScrollbar: function () {
    return this.maxHeight() < this.participantsHeight();
  },
  // Whenever we update - we check if we need to scroll to the bottom.
  // This is used when adding new signatories
  componentDidUpdate: function () {
    if (this.state.needsToScrollToBottom && this.hasScrollbar()) {
      if (this.isMounted() && this.refs["participants-box"] != undefined) {
        $(this.refs["participants-box"].getDOMNode()).scrollTop(this.participantsHeight() - this.maxHeight());
      }
      this.setState({needsToScrollToBottom: false});
    }
  },
  render: function () {
    var self = this;
    var model = this.props.model;
    var doc = model.document();
    if (!doc.ready()) {
      return <div/>;
    } else {
      return (
        <div className="design-view-action-participant-container">
          <div
            ref="participants-box"
            className="design-view-action-participant-container-participants-box"
            style={{
              height: self.currentHeight(),
              overflow: self.hasScrollbar() ? "auto" : "hidden" // Can't use auto due to transitions
            }}
          >
            {
              _.map(doc.signatories(), function (s, i) {
                return (
                  <Participant
                    key={s.cid}
                    model={s}
                    number={i}
                    viewmodel={model}
                  />
                );
              })
            }
          </div>
          <div className="design-view-action-participant-new-box">
            <AddParticipants
              ref="add-participants"
              model={model}
              onAddSingle={function () {self.setState({needsToScrollToBottom: true});}}
              />
          </div>
        </div>
      );
    }
  }
});

});
