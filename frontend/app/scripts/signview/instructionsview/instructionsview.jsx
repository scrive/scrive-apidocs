define(["legacy_code", "Backbone", "React", "common/button",
        "signview/instructionsview/padgivetonextview", "common/htmltextwithsubstitution"],
function (legacy_code, Backbone, React, Button, PadGiveToNextView, HtmlTextWithSubstitution) {

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
        <span>
          <Button
            className="transparent-button download-button"
            text={this.props.title}
            onClick={this.toggleOpen}
          />
          <div style={openStyle} className="menu-options button-group">
            <Button onClick={this.props.onDownload} text="Download" />
          </div>
        </span>
      );
    }
  });

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Document).isRequired,
      arrow: React.PropTypes.func.isRequired
    },

    componentDidMount: function () {
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

    hasPadSigning: function () {
      var doc = this.props.model;
      var sig = doc.currentSignatory();

      return doc.currentSignatory().padDelivery() &&
        doc.isSignedNotClosed() &&
        doc.signatoriesThatCanSignNowOnPad().length > 0;
    },

    handleArrowTextClick: function () {
      var arrow = this.props.arrow;
      mixpanel.track("Click arrow text");
      arrow().goToCurrentTask();
    },

    handleDownloadClick: function () {
      var doc = this.props.model;
      var sig = doc.currentSignatory();
      var downloadUrl = doc.mainfile().downloadLinkForMainFile(doc.title());

      mixpanel.track("Download pdf", {
        "Can sign": doc.currentSignatoryCanSign() ? "yes" : "no",
        "Delivery method": sig.delivery()
      });

      window.open(downloadUrl, "_blank");
    },

    render: function () {
      var self = this;
      var doc = this.props.model;
      var sig = doc.currentSignatory();
      var isSmallScreen = BrowserInfo.isSmallScreen();
      var hasDownloadButton = doc.showpdfdownload() && !isSmallScreen;
      var downloadUrl = doc.mainfile().downloadLinkForMainFile(doc.title());

      var sectionClass = React.addons.classSet({
        "section": true,
        "instructions": true,
        "small-screen": isSmallScreen
      });

      return (
        <div className={sectionClass}>
            <h1 className="follow" ref="headline">
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
              />
            }
            {/* if */ this.hasPadSigning() &&
              <PadGiveToNextView sigs={doc.signatoriesThatCanSignNowOnPad()} />
            }
        </div>
      );
    }
  });
});
