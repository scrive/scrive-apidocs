var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var BasicSettings = require("./basicsettings");
var MessageSettings = require("./messagesettings");
var AttachmentAndSignviewSettings = require("./attachmentsandsignviewsettings");
var Document = require("../../../js/documents.js").Document;


module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels : function() {
    return [this.props.document];
  },
  componentWillUnmount: function () {
    this.hideAllCalendars();
  },
  hideAllCalendars :  function() {
    if (this.refs.basicSettings) {
      this.refs.basicSettings.hideAllCalendars();
    }
  },
  render: function() {
    if (!this.props.document.ready()) {
      return <div/>;
    } else {
      return (
        <div className="design-view-action-process">
          <div className="design-view-action-process-left-column">
            <BasicSettings
              ref="basicSettings"
              document={this.props.document}
            />
          </div>
          <div className="design-view-action-process-middle-column">
            <MessageSettings
              document={this.props.document}
            />
          </div>
          <div className="design-view-action-process-right-column">
            <AttachmentAndSignviewSettings
              document={this.props.document}
            />
          </div>
        </div>
      );
    }
  }
});
