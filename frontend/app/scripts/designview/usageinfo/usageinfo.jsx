var React = require("react");
var _ = require("underscore");
var $ = require("jquery");
var classNames = require("classnames");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Modal = require("../../common/modal");
var BlockingModal = require("../../blocking/blockingmodal");
var SubscriptionSelectionPanel = require("../../account/subscription/subscriptionselectionpanel");
var Subscription = require("../../account/subscription");


module.exports = React.createClass({
  isVisible: function () {
    var subscription = Subscription.currentSubscription();
    return subscription && subscription.ready() && subscription.hasFreePlan();
  },
  onBlockingBoxClicked: function () {
    var subscription = Subscription.currentSubscription();
    if (subscription.hasFreePlan()) {
      this.refs.blockingModal.openContactUsModal();
    } else {
      window.location = "mailto:support@scrive.com";
    }
  },
  render: function () {
    var self = this;
    var subscription = Subscription.currentSubscription();
    if (!this.isVisible()) {
      return (<div className="hidden" />);
    } else {
      return (
        <div
          className={classNames({
            "usage-info-box": true,
            "over-limit": subscription.isOverLimit()
          })}
          onClick={this.onBlockingBoxClicked}
        >
          <div className="usage-info-box-top-shadow" />
          <div className="usage-info-box-content">
            <div className="usage-info-box-headline">
              {/* if */ subscription.hasFreePlan() && !subscription.isOverLimit() &&
                <span>
                  <HtmlTextWithSubstitution
                    secureText={subscription.freeDocumentTokens() == 1 ?
                      localization.blocking.free.has.headlineWithOne
                      : localization.blocking.free.has.headlineWithMany
                    }
                    subs={{ ".put-docs-left-here": subscription.freeDocumentTokens() }}
                  />
                  <div className="usage-info-box-subheadline">
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
                  <div className="usage-info-box-subheadline">
                    <a>
                      {localization.blocking.free.hasNot.subtext1}
                    </a>
                  </div>
                </span>
              }
            </div>
          </div>
          <BlockingModal ref="blockingModal" />
        </div>
      );
    }
  }
});
