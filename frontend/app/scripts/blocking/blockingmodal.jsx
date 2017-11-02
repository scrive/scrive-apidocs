var $ = require("jquery");
var React = require("react");
var Modal = require("../common/modal");
var SubscriptionSelectionPanel = require("../account/subscription/subscriptionselectionpanel");

module.exports = React.createClass({
  getInitialState: function () {
    return {
      showContactUsModal: false
    };
  },
  openContactUsModal: function () {
    this.setState({showContactUsModal: true});
  },
  closeContactUsModal: function () {
    this.setState({showContactUsModal: false});
  },
  render: function () {
    var self = this;
      return (
        <Modal.Container
          width={1080}
          active={self.state.showContactUsModal}
        >
          <Modal.Header
            title={localization.blocking.free.create.title}
            showClose={true}
            onClose={this.closeContactUsModal}
          />
          <Modal.Content>
            <div className="subscription">
              <SubscriptionSelectionPanel/>
            </div>
          </Modal.Content>
          <Modal.Footer>
            <Modal.CancelButton onClick={self.closeContactUsModal} />
          </Modal.Footer>
        </Modal.Container>
      );
  }
});
