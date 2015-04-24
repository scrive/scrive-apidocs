/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin', 'designview/participants/participant', 'designview/participants/addparticipants' ], function(_Legacy, React, BackboneMixin, Participant, AddParticipants) {

return React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels : function() {
    return [this.props.model,this.props.model.document()];
  },
  hideAllCalendars :  function() {
    if (this.refs.basicSettings) {
      this.refs.basicSettings.hideAllCalendars();
    }
  },
  render: function() {
    var self = this;
    var model = this.props.model;
    var doc = model.document();
    if (!doc.ready()) {
      return <div/>;
    } else {
      return (
        <div className="design-view-action-participant-container">
          <div className="design-view-action-participant-container-participants-box">
            {
              $.map(doc.signatories(), function(s,i) {
                return (
                  <Participant
                    model={s}
                    number={i}
                    viewmodel={model}
                  />
                );
              })
            }
          </div>
          <div className="design-view-action-participant-new-box">
            <AddParticipants model={model}/>
          </div>
        </div>
      );
    }
  }
});

});


