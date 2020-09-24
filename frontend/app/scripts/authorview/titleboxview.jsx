var React = require("react");
var $ = require("jquery");
var _ = require("underscore");

var Button = require("../common/button");
var FlashMessages = require("../../js/flashmessages.js");
var FlashMessage = FlashMessages.FlashMessage;
var FlashMessageAfterReload = FlashMessages.FlashMessageAfterReload;
var HtmlTextWithSubstitution = require("../common/htmltextwithsubstitution");
var LoadingDialog = require("../../js/loading.js").LoadingDialog;
var LocalStorage = require("../../js/storage.js").LocalStorage;
var Modal = require("../common/modal");
var ProlongModal = require("./prolongmodal");
var Select = require("../common/select");
var Submit = require("../../js/submits.js").Submit;
var Track = require("../common/track");

var Document = require("../../js/documents.js").Document;

var GiveToNextSignatoryPadModalContent = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    onSignatoryChange: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      padNextSignatory: this.props.document.signatoriesThatCanSignOrApproveNowOnPad()[0]
    };
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevState.padNextSignatory != this.state.padNextSignatory) {
      this.props.onSignatoryChange(this.state.padNextSignatory);
    }
  },
  componentDidMount: function () {
    if (this.state.padNextSignatory) {
      this.props.onSignatoryChange(this.state.padNextSignatory);
    }
  },
  render: function () {
    var self = this;
    var options = [];
    var signatories = this.props.document.signatoriesThatCanSignOrApproveNowOnPad();
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

var WithdrawModalContent = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  render: function () {
    var somebodysigned = _.any(this.props.document.signatories(), function (s) {
      return s.hasSigned() && !s.author();
    });

    var content = localization.process.cancelmodaltext;
    if (this.props.somebodysigned) {
      content = localization.process.cancelmodaltextwithsignatures;
    }

    return (
      <p><HtmlTextWithSubstitution secureText={content} /></p>
    );
  }
});

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    authorview: React.PropTypes.object.isRequired // Can check for inslance because it will create loop
  },
  getInitialState: function () {
    return {
      showWithdrawModal: false,
      showGiveToNextSignatoryPadModal: false,
      padNextSignatory: null,
      showProlongModal: false
    };
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
      (this.props.document.pending() || this.props.document.timedout()) && (
        this.props.document.currentViewerIsAuthor() ||
        this.props.document.currentViewerIsAuthorsCompanyAdmin()
      )
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
      this.props.document.signatoriesThatCanSignOrApproveNowOnPad().length > 0
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
    Track.track("Click restart button");
    this.props.document.restart().sendAjax(function (resp) {
      new FlashMessageAfterReload({
        type: "success",
        content: localization.flashDocumentRestarted
      });
      window.location = "/d/" + resp.id;
    });
  },
  onProlongButtonClick: function () {
    Track.track("Click prolong button");
    this.setState({showProlongModal: true});
  },
  onWithdrawButtonClick: function () {
    Track.track("Click withdraw button");
    this.setState({showWithdrawModal: true});
  },
  onWithdrawModalClose: function () {
    this.setState({showWithdrawModal: false});
  },
  onWithdrawModalAccept: function () {
    var self = this;

    Track.track_timeout(
      "Accept",
      {"Accept": "withdraw document"},
      function () {
        self.withdraw();
      }
    );

    this.onWithdrawModalClose();
  },
  onGoToSignViewButtonClick: function () {
    Track.track("Click go to sign view");
    LocalStorage.set("backlink", "target", "document");
    new Submit({
      method: "'GET",
      url: "/d/signview/" + this.props.document.documentid()
    }).send();
  },
  onGiveToNextSignatoryPadButtonClick: function () {
    var self = this;
    var document_ = this.props.document;

    var sig = document_.signatoriesThatCanSignOrApproveNowOnPad()[0];
    if (sig == undefined) {
      return;
    }

    Track.track("Give for pad signing to some pad signatory - opening modal");
    this.setState({showGiveToNextSignatoryPadModal: true});
  },
  onGiveToNextSignatoryPadModalClose: function () {
    this.setState({showGiveToNextSignatoryPadModal: false});
  },
  onGiveToNextSignatoryPadModalSignatoryChange: function (sig) {
    this.setState({padNextSignatory: sig});
  },
  onGiveToNextSignatoryPadModalAccept: function () {
    Track.track("Give for pad signing to some pad signatory - opening signview");
    LocalStorage.set("backlink", "target", "document");
    if (this.state.padNextSignatory != null) {
      this.state.padNextSignatory.giveForPadSigning().send();
    }
  },
  onProlongModalAccept: function (days) {
    var self = this;

    this.setState({showProlongModal: false});
    this.props.document.prolong(days).sendAjax(function () {
      LoadingDialog.open();
      self.props.authorview.triggerReload();
    });
  },
  onProlongModalClose: function () {
    this.setState({showProlongModal: false});
  },
  render: function () {
    var document_ = this.props.document;

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
          { /* if */ (this.canBeProlonged()) &&
            <Button
              type="action"
              size="big"
              className="s-prolong-button"
              text={localization.process.prolongbuttontext}
              onClick={this.onProlongButtonClick}
            />
          }
          { /* if */ (document_.mainfile() !== null) &&
            <Button
              size="big"
              className="s-download-button"
              text={document_.downloadLink(true).isZip ? localization.authorview.downloadZip : localization.authorview.downloadPdf}
              href={document_.downloadLink(true).link}
            />
          }
        </div>

        <Modal.Container
          className="s-withdraw-confirmation"
          active={this.state.showWithdrawModal}
          width={533}
        >
          <Modal.Header
            title={localization.process.cancelmodaltitle}
            showClose={true}
            onClose={this.onWithdrawModalClose}
          />
          <Modal.Content>
            <WithdrawModalContent document={this.props.document} />
          </Modal.Content>
          <Modal.Footer>
            <Modal.CancelButton onClick={this.onWithdrawModalClose} />
            <Modal.AcceptButton
              title={localization.process.cancelbuttontext}
              onClick={this.onWithdrawModalAccept}
            />
          </Modal.Footer>
        </Modal.Container>

        { /* if */ (this.canGiveToNextSignatoryPad()) &&
          <Modal.Container active={this.state.showGiveToNextSignatoryPadModal} >
            <Modal.Header
              title={localization.authorview.goToSignView}
              showClose={true}
              onClose={this.onGiveToNextSignatoryPadModalClose}
            />
            <Modal.Content>
              <GiveToNextSignatoryPadModalContent
                document={this.props.document}
                onSignatoryChange={this.onGiveToNextSignatoryPadModalSignatoryChange}
              />
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onGiveToNextSignatoryPadModalClose} />
              <Modal.AcceptButton
                onClick={this.onGiveToNextSignatoryPadModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>
        }

        { /* if */ this.canBeProlonged() &&
          <ProlongModal
            active={this.state.showProlongModal}
            onAccept={this.onProlongModalAccept}
            onClose={this.onProlongModalClose}
            prolongDaysBase={this.props.document.prolongDaysBase()}
          />
        }
      </div>
    );
  }
});
