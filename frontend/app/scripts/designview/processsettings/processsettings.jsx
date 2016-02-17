var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var BasicSettings = require("./basicsettings");
var MessageSettings = require("./messagesettings");
var AttachmentAndSignviewSettings = require("./attachmentsandsignviewsettings");


module.exports = React.createClass({
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
