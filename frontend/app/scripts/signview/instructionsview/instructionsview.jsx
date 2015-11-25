define(["legacy_code", "Backbone", "React", "common/button", "signview/instructionsview/padgivetonextview"],
  function (legacy_code, Backbone, React, Button, PadGiveToNextView) {

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
            <Button onClick={this.props.onDownload} text={localization.docsignview.downloadDocumentButtonText} />
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
      this.setupJqueryListeners();
    },

    setupJqueryListeners: function () {
      if (this.refs.headline) {
        var $headline = $(this.refs.headline.getDOMNode());
        $headline.find(".arrowtext").off().click(this.handleArrowTextClick);
      }
    },

    headline: function () {
      var doc = this.props.model;
      var sig = doc.currentSignatory();
      var welcomeUser = sig && sig.name() != "" && sig.canSign();

      var $el = null;
      if (doc.isSigning() && welcomeUser) {
        $el = $("<span>" + localization.docsignview.followTheArrowWithUserName + "</span>");
        $el.find(".signatory-name").text(sig.name());
      } else if (doc.isSigning()) {
        $el = $("<span>" + localization.docsignview.followTheArrow + "</span>");
      } else if (doc.isReviewing()) {
        $el = $("<span>" + localization.docsignview.reviewDocument + "</span>");
      } else if (doc.isSignedAndClosed()) {
        $el = $("<span>" + localization.docsignview.signedAndClosed + "</span>");
      } else if (doc.isSignedNotClosed()) {
        $el = $("<span>" + localization.docsignview.signedNotClosed + "</span>");
      } else if (doc.isUnavailableForSign()) {
        $el = $("<span>" + localization.docsignview.unavailableForSign + "</span>");
      } else {
        $el = $("<span>" + localization.docsignview.unavailableForSign + "</span>");
      }

      return {__html: $el.html()};
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
            <h1 className="follow" ref="headline" dangerouslySetInnerHTML={this.headline()} />
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
