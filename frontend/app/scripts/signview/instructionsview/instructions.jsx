var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Document = require("../../../js/documents.js").Document;
var classNames = require("classnames");

module.exports = React.createClass({
  propTypes: {
    model: React.PropTypes.instanceOf(Document).isRequired
  },

  contextTypes: {
    goToCurrentTask: React.PropTypes.func
  },

  headlineText: function () {
    var doc = this.props.model;
    var sig = doc.currentSignatory();
    var docIsSigningOrApproving = doc.isSigning() || doc.isApproving();
    var welcomeUser = sig && sig.name() != "" && sig.canSignOrApprove();

    if (docIsSigningOrApproving && welcomeUser) {
      return localization.docsignview.followTheArrowWithUserName;
    } else if (docIsSigningOrApproving) {
      return localization.docsignview.followTheArrow;
    } else if (doc.isReviewing()) {
      return localization.docsignview.reviewDocument;
    } else if (sig.signs() && doc.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosed;
    } else if (sig.signs() && doc.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosed;
    } else if (sig.approves() && sig.hasSigned()) {
      return localization.docsignview.approved;
    } else if (doc.isUnavailableForSignOrApprove()) {
      return localization.docsignview.unavailableForSign;
    } else {
      return localization.docsignview.unavailableForSign;
    }
  },

  handleArrowTextClick: function () {
    mixpanel.track("Click arrow text");
    this.context.goToCurrentTask();
  },

  continueUrl: function () {
    var doc = this.props.model;
    var sig = doc.currentSignatory();
    if (sig.hasSigned()) {
      return sig.signsuccessredirect();
    } else if (sig.rejecteddate()) {
      return sig.rejectredirect();
    }
  },

  render: function () {
    var self = this;
    var doc = this.props.model;
    var sig = doc.currentSignatory();
    var continueUrl = this.continueUrl();

    var seleniumClass = "";
    if (doc.isSignedAndClosed()) {
      seleniumClass = "s-header-doc-signed";
    } else if (doc.isUnavailableForSignOrApprove()) {
      seleniumClass = "s-header-doc-cancelled";
    }

    return (
      <div className={classNames("instructions", seleniumClass)} >
          <h1 className="follow">
            <HtmlTextWithSubstitution
              secureText={self.headlineText()}
              subs={{".signatory-name": sig.name()}}
              onClicks={{".arrowtext": function () { self.handleArrowTextClick(); }}}
            />
          </h1>
          {/* if */ continueUrl &&
            <div className="continue-button-wrapper">
              <Button
                text={localization.docsignview.continueButtonText}
                href={continueUrl}
              />
            </div>
            }
      </div>
    );
  }
});
