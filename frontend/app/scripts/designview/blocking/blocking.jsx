var React = require("react");
var _ = require("underscore");
var $ = require("jquery");
var classNames = require("classnames");
var Subscription = require("../../account/subscription");
var BackboneMixin = require("../../common/backbone_mixin");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Modal = require("../../common/modal");
var SubscriptionSelectionPanel = require("../../account/subscription/subscriptionselectionpanel");


module.exports = React.createClass({
  propTypes: {
    subscription: React.PropTypes.instanceOf(Subscription).isRequired
  },
  mixins: [BackboneMixin.BackboneMixin],
  getBackboneModels: function () {
    return [this.props.subscription];
  },
  getInitialState: function () {
    return {
      showContactUsModal: false
    };
  },
  isVisible: function () {
    var subscription = this.props.subscription;
    return subscription.ready() && (
      subscription.hasFreePlan() ||
      (subscription.hasTeamPlan() && subscription.isOverLimit())
    );
  },
  onBlockingBoxClicked: function () {
    if (this.props.subscription.hasFreePlan()) {
      this.openContactUsModal();
    } else {
      window.location = "mailto:support@scrive.com";
    }
  },
  openContactUsModal: function () {
    this.setState({showContactUsModal: true});
  },
  closeContactUsModal: function () {
    this.setState({showContactUsModal: false});
  },
  render: function () {
    var self = this;
    var subscription = this.props.subscription;
    if (!this.isVisible()) {
      return (<div className="hidden" />);
    } else {
      return (
        <div
          className={classNames({
            "blocking-box": true,
            "over-limit": subscription.isOverLimit()
          })}
          onClick={this.onBlockingBoxClicked}
        >
          <div className="blocking-box-top-shadow"/>
          <div className="blocking-box-content">
            <div className="blocking-box-headline">
              {/* if */ subscription.hasFreePlan() && !subscription.isOverLimit() &&
                <span>
                  <HtmlTextWithSubstitution
                    secureText={localization.blocking.free.has.headline}
                    subs={{".put-docs-used-here": subscription.startedLastMonth()}}
                  />
                  <div className="blocking-box-subheadline">
                    <a>
                      {localization.blocking.free.has.subtext1}
                    </a>
                  </div>
                </span>
              }
              {/* else if */ subscription.hasFreePlan() && subscription.isOverLimit() &&
                <span>
                  <HtmlTextWithSubstitution
                    secureText={localization.blocking.free.hasNot.headline}
                  />
                  <div className="blocking-box-subheadline">
                    <a>
                      {localization.blocking.free.hasNot.subtext1}
                    </a>
                  </div>
                </span>
              }
              {/* if */ subscription.hasTeamPlan() && subscription.isOverLimit() &&
                <span>
                  <HtmlTextWithSubstitution
                    secureText={localization.blocking.usedall.headline}
                  />
                  <div className="blocking-box-subheadline">
                    <HtmlTextWithSubstitution
                      secureText={localization.blocking.usedall.subtext1}
                    />
                  </div>
                </span>
              }
            </div>
          </div>


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

        </div>
      );
    }
  }
});
