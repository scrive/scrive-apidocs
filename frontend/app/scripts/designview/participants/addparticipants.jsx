/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/button'], function(_Legacy, React, Button) {

return React.createClass({
  onDone : function() {
    mixpanel.track('Close participant');
    this.props.model.setParticipantDetail(undefined);
  },
  addSingleParticipant : function() {
    var model = this.props.model;
    var doc = model.document();
    var sig = new Signatory({
                        document:doc,
                        signs:true
                    });
    doc.addExistingSignatory(sig);
    model.setParticipantDetail(sig);
  },
  addMultisendParticipant : function() {
    mixpanel.track('Click add CSV');
    new CsvSignatoryDesignPopup({
      designview : this.props.model
    });
  },
  render: function() {
    var self = this;
    var model = this.props.model;
    var doc = model.document();

    return (
      <div className="design-view-action-participant-new-box-buttons">
        {/* if */ model.participantDetail() != undefined &&
          <div className="design-view-action-participant-done">
            <Button
               type="action"
               text={localization.designview.addParties.close}
               onClick={function() {self.onDone();}}
            />
          </div>
        }
        {/* else */ model.participantDetail() == undefined &&
          <div>
          
            {/* if */ !_.any(model.document().signatories(), function(x) { return x.isCsv(); }) &&
              <div className="design-view-action-participant-new-multi">
                <Button
                  text={localization.designview.addMultisend}
                  onClick={function() {self.addMultisendParticipant();}}
                />
              </div>
            }

            <div className="design-view-action-participant-new-single">
              <Button
                type="action"
                text={localization.designview.addParty}
                onClick={function() {self.addSingleParticipant();}}
              />
            </div>

          </div>
        }
      </div>
    );
  }
});

});
