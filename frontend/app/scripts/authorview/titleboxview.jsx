var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var Button = require("../common/button");
var Confirmation = require("../../js/confirmations.js").Confirmation;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var LoadingDialog = require("../../js/loading.js").LoadingDialog;
var LocalStorage = require("../../js/storage.js").LocalStorage;
var ProlongModal = require("../../js/authorview/prolongmodal.js").ProlongModal;
var Select = require("../common/select");
var Submit = require("../../js/submits.js").Submit;
var trackTimeout = require("../common/track_timeout");

var Document = require("../../js/documents.js").Document;

var GiveToNextSignatoryPadModalContent = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  getInitialState: function () {
    return {
      padNextSignatory: this.props.document.signatoriesThatCanSignNowOnPad()[0]
    };
  },
  render: function () {
    var self = this;
    var options = [];
    var signatories = this.props.document.signatoriesThatCanSignNowOnPad();
    var firstSignatoryName = null;

    if (signatories.length > 1) {
      _.each(signatories, function (sig) {
        options.push({
          name: (sig.smartname().trim() !== "" ? sig.smartname() : sig.nameInDocument()),
          onSelect: function () {
            self.setState({padNextSignatory: sig});
            self.forceUpdate();
            return true;
          },
          selected: (self.state.padNextSignatory == sig)
        });
      });

      return (
        <div className="give-to-next-signatory-pad-modal-content">
          <label className="message">{localization.pad.giveForSigningThisDevice}</label>
          <span>
            <Select className="float-left" options={options} />
          </span>
        </div>
      );
    } else {
      firstSignatoryName = signatories[0].smartname();
      if (firstSignatoryName == "") {
        firstSignatoryName = localization.pad.notNamedParty.toLowerCase();
      }

      return (
        <div className="give-to-next-signatory-pad-modal-content">
          <label className="message">
            {localization.pad.giveForSigningThisDevice} <strong>{firstSignatoryName}</strong>
          </label>
        </div>
      );
    }
  }
});

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    authorview: React.PropTypes.object.isRequired // Can check for inslance because it will create loop
  },
  canBeRestarted: function () {
    return (
      (
        this.props.document.timedout() ||
        this.props.document.canceled() ||
        this.props.document.rejected()
      ) &&
      this.props.document.currentViewerIsAuthor()
    );
  },
  canBeProlonged: function () {
    return (
      this.props.document.timedout() &&
      this.props.document.currentViewerIsAuthor()
    );
  },
  canBeWithdrawn: function () {
    return (
      this.props.document.pending() && (
        this.props.document.currentViewerIsAuthor() ||
        this.props.document.currentViewerIsAuthorsCompanyAdmin()
      )
    );
  },
  canGoToSignView: function () {
    return (
      this.props.document.currentSignatoryCanSign() &&
      this.props.document.pending()
    );
  },
  canGiveToNextSignatoryPad: function () {
    return (
      !this.canGoToSignView() &&
      this.props.document.currentViewerIsAuthor() &&
      this.props.document.pending() &&
      this.props.document.signatoriesThatCanSignNowOnPad().length > 0
    );
  },
  withdraw: function () {
    var self = this;

    LoadingDialog.open();
    var errorcallback = function () {
      new FlashMessage({
        content: localization.cannotWithdrawAlreadySignedDocument,
        type: "error"
      });

      self.props.authorview.triggerReload();
    };

    this.props.document.cancel().sendAjax(
      function () {
          self.props.authorview.triggerReload();
      },
      errorcallback
    );
  },
  onRestartButtonClick: function () {
    mixpanel.track("Click restart button");
    this.props.document.restart().sendAjax(function (resp) {
       var newdocdata = JSON.parse(resp);
       new FlashMessage({
        type: "success",
        content: localization.flashDocumentRestarted,
        withRedirect: true,
        redirect: "/d/" + newdocdata.id
      });
    });
  },
  onProlongButtonClick: function () {
    mixpanel.track("Click prolong button");
    new ProlongModal({authorview: this.props.authorview});
  },
  onWithdrawButtonClick: function () {
    mixpanel.track("Click withdraw button");
    var somebodysigned = _.any(this.props.document.signatories(), function (s) {
      return s.hasSigned() && !s.author();
    });

    var modalcontent = localization.process.cancelmodaltext;
    if (somebodysigned) {
      modalcontent = localization.process.cancelmodaltextwithsignatures;
    }

    var self = this;
    new Confirmation({
      title: localization.process.cancelmodaltitle,
      content: $("<p>" + modalcontent + "</p>"),
      width: 533,
      acceptText: localization.process.cancelbuttontext,
      rejectText: localization.cancel,
      acceptType: "action",
      extraClass: "s-withdraw-confirmation",
      onAccept: function () {
        trackTimeout(
          "Accept",
          {"Accept": "withdraw document"},
          function () {
            self.withdraw();
          }
        );
        return true;
      }
    });
  },
  onGoToSignViewButtonClick: function () {
    mixpanel.track("Click go to sign view");
    LocalStorage.set("backlink", "target", "document");
    new Submit({
      method:"'GET",
      url: "/d/signview/" + this.props.document.documentid()
    }).send();
  },
  onGiveToNextSignatoryPadButtonClick: function () {
    var self = this;
    var document_ = this.props.document;

    var sig = document_.signatoriesThatCanSignNowOnPad()[0];
    if (sig == undefined) {
      return;
    }

    mixpanel.track("Give for pad signing to some pad signatory - opening modal");
    var padNextSignatoryModalContent = $("<div/>");
    var modalContentComponent = React.render(
      React.createElement(GiveToNextSignatoryPadModalContent, {document: document_}),
      padNextSignatoryModalContent[0]
    );

    new Confirmation({
      title: localization.authorview.goToSignView,
      content: padNextSignatoryModalContent,
      onAccept: function () {
          mixpanel.track("Give for pad signing to some pad signatory - opening signview");
          LocalStorage.set("backlink", "target", "document");
          if (modalContentComponent.state.padNextSignatory != undefined) {
            modalContentComponent.state.padNextSignatory.giveForPadSigning().send();
          }
      }
    });
  },
  render: function () {
    var document_ = this.props.document
    var downloadLink = document_.mainfile().downloadLinkForMainFile(document_.title(), true);

    return (
      <div className="titlebox">
        <div className="headline">{this.props.document.title()}</div>
        <div className="buttonbox">
          { /* if */ (this.canBeRestarted()) &&
            <Button
              type="cancel"
              size="big"
              className="s-restart-button"
              text={localization.process.restartbuttontext}
              oneClick={true}
              onClick={this.onRestartButtonClick}
            />
          }
          { /* if */ (this.canBeProlonged()) &&
            <Button
              type="action"
              size="big"
              className="s-prolong-button"
              text={localization.process.prolongbuttontext}
              onClick={this.onProlongButtonClick}
            />
          }
          { /* if */ (this.canBeWithdrawn()) &&
            <Button
              type="cancel"
              size="big"
              className="s-withdraw-button"
              text={localization.process.cancelbuttontext}
              onClick={this.onWithdrawButtonClick}
            />
          }
          { /* if */ (this.canGoToSignView()) &&
            <Button
              type="action"
              size="big"
              className="s-go-to-sign-view-button"
              text={localization.authorview.goToSignView}
              onClick={this.onGoToSignViewButtonClick}
            />
          }
          { /* if */ (this.canGiveToNextSignatoryPad()) &&
            <Button
              type="action"
              size="big"
              className="s-give-to-next-signatory-pad-button"
              text={localization.authorview.goToSignView}
              onClick={this.onGiveToNextSignatoryPadButtonClick}
            />
          }
          <Button
            size="big"
            className="s-download-button"
            text={localization.authorview.downloadPdf}
            href={downloadLink}
          />
        </div>
      </div>
    );
  }
});
