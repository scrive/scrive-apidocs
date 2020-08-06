var React = require("react");
var Modal = require("../common/modal");
var Button = require("../common/button");
var LocalStorage = require("../../js/storage.js").LocalStorage;

module.exports = React.createClass({
  propTypes: {
      onDismiss: React.PropTypes.func.isRequired,
      siglinkid: React.PropTypes.isRequired,
      flowBacklink: React.PropTypes.string.isRequired
  },

  goBack: function () {
    LocalStorage.set("seenpostsignview", this.props.siglinkid.toString(), true);
    window.location = this.props.flowBacklink;
  },

  doDismiss: function () {
    LocalStorage.set("seenpostsignview", this.props.siglinkid.toString(), true);
    this.props.onDismiss();
  },

  render: function () {
    return <div><Modal.Container active={true}>
        <Modal.Header
          title={localization.postSignview.title}
        />
        <Modal.Content>
          <div>
            Go back to the document list to see all documents.
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.AcceptButton
                text="Back to document list"
                onClick={this.goBack}
              />
          <Modal.ExtraButtons>
                <Button
                  text={localization.postSignview.dismissButton}
                  onClick={this.doDismiss}
                />
          </Modal.ExtraButtons>
        </Modal.Footer>
      </Modal.Container></div>;
  }});
