define(["legacy_code", "Backbone", "React", "signview/instructionsview/padgivetonextview"],
  function (legacy_code, Backbone, React, PadGiveToNextView) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    componentDidMount: function () {
      this.setupJqueryListeners();
    },

    componentDidUpdate: function () {
      this.setupJqueryListeners();
    },

    setupJqueryListeners: function () {
      if (this.refs.headline) {
        var $headline = $(this.refs.headline.getDOMNode());
        $headline.find(".arrowtext").off().click(this.handleArrowTextClick);
      }
    },

    headline: function () {
      var doc = this.props.model.document();
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

    subtext: function () {
      var doc = this.props.model.document();
      var sig = doc.currentSignatory();

      if (!(sig.hasSigned() && (doc.pending() || doc.closed()))) {
        return "";
      }

      var text = "";

      if (doc.closed()) {
        if (sig.emailConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedClosedByEmail;
        } else if (sig.mobileConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedClosedByMobile;
        } else if (sig.emailMobileConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedClosedByEmailMobile;
        }
      } else {
        if (sig.emailConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedNotClosedByEmail;
        } else if (sig.mobileConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedNotClosedByMobile;
        } else if (sig.emailMobileConfirmationDelivery()) {
          text = localization.docsignview.subtitleSignedNotClosedByEmailMobile;
        }
      }

      return text;
    },

    hasArrowLegend: function () {
      var doc = this.props.model.document();
      var sig = doc.currentSignatory();
      var hasOptionalFields = _.any(sig.fields(), function (f) {
        return f.hasPlacements() && !f.obligatory() && !f.isClosed();
      });
      var hasAttachments = doc.authorattachments().length > 0;

      return doc.currentSignatoryCanSign() && (hasOptionalFields || hasAttachments);
    },

    hasPadSigning: function () {
      var doc = this.props.model.document();
      var sig = doc.currentSignatory();

      return doc.currentSignatory().padDelivery() &&
        doc.isSignedNotClosed() &&
        doc.signatoriesThatCanSignNowOnPad().length > 0;
    },

    handleArrowTextClick: function () {
      var model = this.props.model;
      mixpanel.track("Click arrow text");
      model.arrow().goToCurrentTask();
    },

    handleDownloadClick: function () {
      var doc = this.props.model.document();
      var sig = doc.currentSignatory();

      mixpanel.track("Download pdf", {
        "Can sign": doc.currentSignatoryCanSign() ? "yes" : "no",
        "Delivery method": sig.delivery()
      });
    },

    render: function () {
      var doc = this.props.model.document();
      var sig = doc.currentSignatory();
      var isSmallScreen = BrowserInfo.isSmallScreen();
      var hasDownloadButton = doc.showpdfdownload() && !isSmallScreen;
      var downloadUrl = doc.mainfile().downloadLinkForMainFile(doc.title());

      var divClass = React.addons.classSet({
        "instructions": true,
        "section": true,
        "spacing": true,
        "small-screen": isSmallScreen
      });

      return (
        <div className={divClass}>
          <div ref="headline" className="headline" dangerouslySetInnerHTML={this.headline()} />
          <div className="subheadline">
            {this.subtext()}
          </div>
          {/* if */ this.hasPadSigning() &&
            <PadGiveToNextView sigs={doc.signatoriesThatCanSignNowOnPad()} />
          }
          <div className="subheadline">
            <div className="smaller-bits">
              {/* if */ hasDownloadButton &&
                <a target="_blank" className="download" href={downloadUrl} onClick={this.handleDownloadClick}>
                  {doc.title() + ".pdf"}
                </a>
              }
            </div>
          </div>
          {/* if */ this.hasArrowLegend() &&
            <div className="arrow-legend">
              <p className="row">
                <span className="icon-legend mandatory"></span>
                <span className="copy">{localization.docsignview.mandatoryAction}</span>
              </p>
              <p className="row">
                <span className="icon-legend optional"></span>
                <span className="copy">{localization.docsignview.optionalAction}</span>
              </p>
            </div>
          }
        </div>
      );
    }
  });
});
