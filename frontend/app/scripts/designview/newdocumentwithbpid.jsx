var React = require("react");

var Track = require("../common/track");

var InfoTextInput = require("../common/infotextinput");
var Button = require("../common/button");
var Submit = require("../../js/submits.js").Submit;

var NewDocumentWithBPID = React.createClass({
  propTypes: {
  },
  componentWillMount: function () {
    mixpanel.register({ Context: "New document with BPID Page" });
    Track.track("New document with BPID Page");
  },
  getInitialState: function () {
    return {
      newDocumentBPID: ""
    };
  },
  onNewDocumentBPIDChange: function (value) {
    this.setState({ newDocumentBPID: value });
  },
  onNewDocument: function () {
    const newDocumentBPID = this.state.newDocumentBPID;
    if (newDocumentBPID.length > 0 && /^[a-z0-9]+$/i.test(newDocumentBPID)) {
      new Submit({
        method: "POST",
        url: "/newdocumentwithbpid",
        bpid: newDocumentBPID
      }).send();
    }
  },
  render: function () {
    var self = this;
    return (
      <div>
        <div className="short-input-section accept-tos s-accept-tos">
          <div className="short-input-container">
            <div className="short-input-container-body-wrapper">
              <div className="short-input-container-body">
                <div className="position first">
                  <label>BusinessPartner ID</label>
                  <InfoTextInput
                    ref="newdocumentbpid"
                    name="newdocumentbpid"
                    value={self.state.newDocumentBPID}
                    onChange={this.onNewDocumentBPIDChange}
                  />
                </div>
                <div className="position">
                  <Button
                    ref="save"
                    type="action"
                    className="save"
                    text="New document with this BPID"
                    onClick={this.onNewDocument}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
});

module.exports = NewDocumentWithBPID;
