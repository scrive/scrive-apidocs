/** @jsx React.DOM */

define(['React', 'common/backbone_mixin','designview/messagesettings','legacy_code'], function(React, BackboneMixin, MessageSettings) {

return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model,this.props.model.document()];
    },
    hideAllCalendars :  function() {

    },
    render: function() {
      var self = this;
      var doc = this.props.model.document();
      if (!doc.ready()) {
        return <div/>;
      } else {
      return (
        <div className="design-view-action-process">
          <div className="design-view-action-process-left-column">
          </div>
          <div className="design-view-action-process-middle-column">
            <MessageSettings
              document = {doc}
            />
          </div>
          <div className="design-view-action-process-right-column">
          </div>
        </div>
      );
    }
  }
});


});


