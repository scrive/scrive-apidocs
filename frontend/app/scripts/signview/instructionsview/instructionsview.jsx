var Backbone = require("backbone");
var React = require("react");
var Button = require("../../common/button");
var HtmlTextWithSubstitution = require("../../common/htmltextwithsubstitution");
var Document = require("../../../js/documents.js").Document;
var classNames = require("classnames");

  var Menu = React.createClass({
    getInitialState: function () {
      return {open: false};
    },

    toggleOpen: function () {
      this.setState({open: !this.state.open});
    },

    render: function () {
      var open = this.state.open;
      var openStyle = {height: open ? "64px" : "0"};

      return (
        <span className="menu">
          <Button
            className="transparent-button download-button"
            text={this.props.title}
            onClick={this.toggleOpen}
          />
          <div style={openStyle} className="menu-options">
            <Button onClick={this.props.onDownload}
                    text={localization.docsignview.downloadDocumentButtonText}
                    href={this.props.downloadUrl}
            />
          </div>
        </span>
      );
    }
  });

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired,
      loggedInAsAuthor: React.PropTypes.bool.isRequired,
      arrow: React.PropTypes.func.isRequired
    },

    headlineText: function () {
      var doc = this.props.model;
      var sig = doc.currentSignatory();
      var welcomeUser = sig && sig.name() != "" && sig.canSign();

      if (doc.isSigning() && welcomeUser) {
        return localization.docsignview.followTheArrowWithUserName;
      } else if (doc.isSigning()) {
        return localization.docsignview.followTheArrow;
      } else if (doc.isReviewing()) {
        return localization.docsignview.reviewDocument;
      } else if (doc.isSignedAndClosed()) {
        return localization.docsignview.signedAndClosed;
      } else if (doc.isSignedNotClosed()) {
        return localization.docsignview.signedNotClosed;
      } else if (doc.isUnavailableForSign()) {
        return localization.docsignview.unavailableForSign;
      } else {
        return localization.docsignview.unavailableForSign;
      }
    },

    handleArrowTextClick: function () {
      var arrow = this.props.arrow;
      mixpanel.track("Click arrow text");
      arrow().goToCurrentTask();
    },

    handleDownloadClick: function () {
      var doc = this.props.model;
      var sig = doc.currentSignatory();

      mixpanel.track("Download pdf", {
        "Can sign": doc.currentSignatoryCanSign() ? "yes" : "no",
        "Delivery method": sig.delivery()
      });
    },

    render: function () {
      var self = this;
      var doc = this.props.model;
      var sig = doc.currentSignatory();
      var hasDownloadButton = doc.showpdfdownload();

      var sectionClass = classNames({
        "section": true,
        "instructions": true
      });

      var downloadUrl = doc.mainfile().downloadLinkForMainFile(doc.title(), true);

      return (
        <div className={sectionClass}>
            <h1 className="follow">
              <HtmlTextWithSubstitution
                secureText={self.headlineText()}
                subs={{".signatory-name": sig.name()}}
                onClicks={{".arrowtext": function () {self.handleArrowTextClick();}}}
              />
            </h1>
            {/* if */ hasDownloadButton &&
              <Menu
                title={doc.title()}
                onDownload={this.handleDownloadClick}
                downloadUrl={downloadUrl}
              />
            }
        </div>
      );
    }
  });
