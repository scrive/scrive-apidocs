var $ = require("jquery");
var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var Modal = require("../common/modal");
var SubscriptionPanelModel = require("../account/subscription/subscriptionpanelmodel");
var SubscriptionSelectionPanel = require("../account/subscription/subscriptionselectionpanel");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],
  getInitialState: function () {
    return {
      showContactUsModal: false,
      model: new SubscriptionPanelModel()
    };
  },
  getBackboneModels: function () {
    return [this.state.model];
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
              { this.state.model.ready() ? <SubscriptionSelectionPanel key="sub_done"
                fstname={this.state.model.user().fstname()}
                sndname={this.state.model.user().sndname()}
                email={this.state.model.user().email()}/> :
              <SubscriptionSelectionPanel key="sub_notdone" /> }
            </div>
          </Modal.Content>
          <Modal.Footer>
            <Modal.CancelButton onClick={self.closeContactUsModal} />
          </Modal.Footer>
        </Modal.Container>
      );
  }
});
