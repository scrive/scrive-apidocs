/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin',  'gemini-scrollbar', 'designview/participants/participant', 'designview/participants/addparticipants' ], function(_Legacy, React, BackboneMixin, _GeminiScrollbar, Participant, AddParticipants) {

return React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels : function() {
    return [this.props.model,this.props.model.document()];
  },
  hideAllCalendars:  function() {
    if (this.refs.basicSettings) {
      this.refs.basicSettings.hideAllCalendars();
    }
  },

  componentDidMount:  function()  {
    if (this.refs["scroll-area"] != undefined) {
      this.scrollbar = new GeminiScrollbar({
        element: this.refs["scroll-area"].getDOMNode(),
        autoshow: true,
        createElements: false
      }).create();
    }
  },
  componentDidUpdate:  function()  {
    if (this.scrollbar != undefined) {
      this.scrollbar.update();
    } else if (this.refs["scroll-area"] != undefined) {
      this.scrollbar = new GeminiScrollbar({
        element: this.refs["scroll-area"].getDOMNode(),
        autoshow: false,
        createElements: false
      }).create();

    }
  },
  componentWillUnmount:  function()  {
    if (this.scrollbar != undefined) {
      this.scrollbar.destroy();
      this.scrollbar = null;
    }
  },
  height: function() {
    var maxHeight = $(window).height() - 350;
    if( maxHeight<250 ) {
       maxHeight = 250;
    }
    var height = this.props.model.document().signatories().length * 60;
    if (this.props.model.participantDetail() != undefined) {
      height += 220;
    }

    return Math.min(height,maxHeight) + "px";
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
          <div className="design-view-action-participant-container-participants-box gm-scrollbar-container" ref="scroll-area"  style={{"height" : self.height()}}>
            <div className='gm-scrollbar -vertical'>
              <div className='thumb'></div>
            </div>
            <div className='gm-scrollbar -horizontal'>
              <div className='thumb'></div>
            </div>
            <div className='gm-scroll-view'>
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


