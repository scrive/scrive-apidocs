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
  updateScrollbar : function() {
    if (this.refs["scroll-area"] != undefined) {
       if (this.scrollbar != undefined && this.scrollbar.element == this.refs["scroll-area"].getDOMNode()) {
         this.scrollbar.update();
       } else {
         if (this.scrollbar != undefined) {
           this.destroyScrollbar();
         }
         this.scrollbar = new GeminiScrollbar({
          element: this.refs["scroll-area"].getDOMNode(),
          autoshow: false,
          createElements: false
         }).create();
      }
    }
  },
  destroyScrollbar : function() {
    if (this.scrollbar != undefined) {
      this.scrollbar.destroy();
      this.scrollbar = null;
    }
  },
  componentDidMount:  function()  {
    this.updateScrollbar();
  },
  componentDidUpdate:  function()  {
    this.updateScrollbar();

  },
  componentWillUnmount:  function()  {
    this.destroyScrollbar();
  },
  // height of participants vie needs to be computed for scrollbar.
  // Height of whole secton and each field is hardcoded
  participantsHeight: function() {
    var heightOfNavigationTabs = 350; // Height of navigation tabs on page.
    var maxHeight = Math.max(250,$(window).height() - heightOfNavigationTabs); // Max height of scroll bar
    var heightOfUnexpandedSignatory = 60;  // Height each signatory description when signatory is not expanded
    var heightOfField = 48; // Height each field row
    var heightOfParticipantSettings = 114; // Height of 5 selects at bottom of signatory
    var height = 0;

    height += this.props.model.document().signatories().length * heightOfUnexpandedSignatory;

    if (this.props.model.participantDetail() != undefined) {
      height += heightOfParticipantSettings;
      var fields = 0;
      var nameIncluded = false;
      _.each(this.props.model.participantDetail().fields(), function(f) {
          if (f.isFstName() || f.isSndName()) {
            if (!nameIncluded) {
              nameIncluded = true;
              fields++;
            }
          } else  if (f.isText() || f.isBlank()) {
            fields++;
          }
      });

      height += Math.ceil((fields + 1) / 3) * heightOfField;
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
          <div className="design-view-action-participant-container-participants-box gm-scrollbar-container" ref="scroll-area"  style={{"height" : self.participantsHeight()}}>
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


