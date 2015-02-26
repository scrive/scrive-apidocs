/** @jsx React.DOM */

define(['legacy_code', 'React', 'common/backbone_mixin','designview/processsettings/basicsettings','designview/processsettings/messagesettings', 'designview/processsettings/attachmentsandsignviewsettings'], function(_Legacy, React, BackboneMixin, BasicSettings, MessageSettings, AttachmentAndSignviewSettings) {

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
    var doc = self.props.model.document();
    if (!doc.ready()) {
      return <div/>;
    } else {
      return (
        <div className="design-view-action-process">
          <div className="design-view-action-process-left-column">
            <BasicSettings
              ref="basicSettings"
              document = {doc}
            />
          </div>
          <div className="design-view-action-process-middle-column">
            <MessageSettings
              document = {doc}
            />
          </div>
          <div className="design-view-action-process-right-column">
            <AttachmentAndSignviewSettings
              model = {this.props.model}
            />
          </div>
        </div>
      );
    }
  }
});

});


