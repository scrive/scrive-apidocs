/*global BlockingInfo*/

var React = require("react");
var classNames = require("classnames");
var $ = require("jquery");
var _ = require("underscore");

var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var Document = require("../../../js/documents.js").Document;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Submit = require("../../../js/submits.js").Submit;
var DocumentSaveMixin = require("../document_save_mixin");

var CantSignModalContent = require("./cantsignmodalcontent");
var ConfirmationModalAcceptButton = require("./confirmationmodalacceptbutton");
var SignConfirmationModalContent = require("./signconfirmationmodalcontent");
var SendConfirmationModalContent = require("./sendconfirmationmodalcontent");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin, DocumentSaveMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  componentWillMount: function () {
    this._confirmationModal = null;
  },
  getBackboneModels: function () {
    return [this.props.document]
  },
  showCantSignModal: function () {
    var modalContent = $("<div/>");
    React.render(React.createElement(CantSignModalContent, {}), modalContent[0]);

    new Confirmation({
      title: localization.designview.cantSignModal.title,
      cancelVisible: false,
      content: modalContent
    });
  },
  showSignConfirmationModal: function () {
    var modalContent = $("<div/>");
    React.render(
      React.createElement(
        SignConfirmationModalContent,
        {document: this.props.document}
      ),
      modalContent[0]
    );

    var buttonContent = $("<div/>");
    React.render(
      React.createElement(
        ConfirmationModalAcceptButton,
        {
          text: localization.designview.sign,
          onClick: this.onSignConfirmationModalAccept
        }
      ),
      buttonContent[0]
    );

    this._confirmationModal = new Confirmation({
      title: localization.signByAuthor.modalTitle,
      acceptButton: buttonContent,
      rejectText: localization.cancel,
      content: modalContent
    });
  },
  showSendConfirmationModal: function () {
    var otherSignatoriesSignInPerson = _.every(
      this.props.document.signatories(), function (sig) {
        return sig.padDelivery() || sig.author();
      }
    );

    var modalContent = $("<div/>");
    React.render(
      React.createElement(
        SendConfirmationModalContent,
        {
          document: this.props.document,
          otherSignatoriesSignInPerson: otherSignatoriesSignInPerson
        }
      ),
      modalContent[0]
    );

    var buttonText = null;
    if (otherSignatoriesSignInPerson) {
      buttonText = localization.process.startsigningbuttontext;
    } else {
      buttonText = localization.process.sendbuttontext;
    }

    var buttonContent = $("<div/>");
    React.render(
      React.createElement(
        ConfirmationModalAcceptButton,
        {
          text: buttonText,
          onClick: this.onSendConfirmationModalAccept
        }
      ),
      buttonContent[0]
    );

    var modalTitle = null;
    if (otherSignatoriesSignInPerson) {
      modalTitle = localization.process.startsigningtitle;
    } else {
      modalTitle = localization.process.confirmsendtitle;
    }

    this._confirmationModal = new Confirmation({
      title: modalTitle,
      acceptButton: buttonContent,
      rejectText: localization.cancel,
      content: modalContent
    });
  },
  saveTemplateButtonText: function () {
    if (this.props.document.isTemplate() && this.props.document.saved()) {
      return localization.designview.saveTemplateButton;
    } else {
      return localization.designview.saveAsTemplateButton;
    }
  },
  saveDraftButtonText: function () {
    if (this.props.document.saved()) {
      return localization.designview.saveDraftButton;
    } else {
      return localization.designview.saveAsDraftButton;
    }
  },
  removePDF: function () {
    var doc = this.props.document;

    var submit = new Submit({
      method: "POST",
      url:  "/api/frontend/documents/" + doc.documentid() + "/setfile",
      ajax: true,
      onSend: function () {},
      ajaxerror: function (d, a) {
        doc.recall();
      },
      ajaxsuccess: function () {
        doc.recall();
      }
    });

    submit.send();
  },
  onSaveTemplateButtonClick: function () {
    mixpanel.track("Click save as template");
    this.props.document.makeTemplate();
    this.saveDocument();
  },
  onRemoveDocumentButtonClick: function () {
    mixpanel.track("Click remove file");

    var doc = this.props.document;

    doc.markAsNotReady();
    this.saveAndFlashMessageIfAlreadySaved();

    doc.afterSave(this.removePDF);
  },
  onSaveDraftButtonClick: function () {
    mixpanel.track("Click save as draft");
    this.saveDocument();
  },
  onSendButtonClick: function () {
    if (this.props.document.hasProblems()) {
      this.showCantSignModal();
    } else {
      var doc = this.props.document;

      var isSigning = doc.authorCanSignFirst();

      mixpanel.track("Click sign button", {
        "Is Signing": isSigning,
        "Uses eleg": doc.hasEleg(),
        "Uses email delivery": doc.hasEmail(),
        "Uses mobile delivery": doc.hasSMS(),
        "Uses pad delivery": doc.hasPad(),
        "Uses email and mobile delivery": doc.hasEmailAndSMS()
      });

      doc.save();
      if (BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
        mixpanel.track("Open blocking popup");
        mixpanel.people.set({
          "Blocking Popup": new Date()
        });

        BlockingInfo.createPopup();
      } else {
        if (isSigning) {
          this.showSignConfirmationModal();
        } else {
          this.showSendConfirmationModal();
        }
      }
    }
  },
  onSignConfirmationModalAccept: function () {
    mixpanel.track("Click accept sign", {"Button": "sign"});

    if (this._confirmationModal) {
      this._confirmationModal.hideCancel();
      this._confirmationModal.hideClose();
    }

    var self = this;
    var doc = this.props.document;

    doc.takeSigningScreenshot(function () {
      doc.afterSave(function () {
        self.signWithCSV(doc, 1, doc.isCsv() ? doc.csv().length - 1 : undefined);
      });
    });
  },
  onSendConfirmationModalAccept: function () {
    if (this._confirmationModal) {
      this._confirmationModal.hideCancel();
      this._confirmationModal.hideClose();
    }

    var self = this;
    var doc = this.props.document;

    mixpanel.track("Click accept sign", {"Button": "send"});
    doc.takeSigningScreenshot(function () {
      self.sendWithCSV(doc, 1, doc.isCsv() ? doc.csv().length - 1 : undefined);
    });
  },
  signWithCSV: function (doc, index, totalCount) {
    var self = this;
    if (doc.csv() != undefined && doc.csv().length > 2) {
      doc.clone(function (doc2) {
        var name = doc.normalizeWithFirstCSVLine();

        var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
        $(".put-person-name", copyDocFor).text(name);

        var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
        $(".put-current-index", copyPartOf).text(index);
        $(".put-total-count", copyPartOf).text(totalCount);

        LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});

        doc.save();
        doc.afterSave(function () {
          doc.makeReadyForSigning().add("author_signs_now", "true").sendAjax(function (docdata) {
            var newdoc = new Document(new Document({}).parse(docdata));
            newdoc.set({"screenshots": doc.get("screenshots")});
            newdoc.sign().sendAjax(
              function () {
                doc2.dropFirstCSVLine();
                doc2.save();
                doc2.afterSave(function () {
                  self.signWithCSV(doc2, index + 1, totalCount);
                });
              }
            );
          });
        });
      }).sendAjax();
    } else {
      var name = doc.normalizeWithFirstCSVLine();

      var singleDocument = !doc.isCsv();
      if (!singleDocument) {
        var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
        $(".put-person-name", copyDocFor).text(name);

        var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
        $(".put-current-index", copyPartOf).text(index);
        $(".put-total-count", copyPartOf).text(totalCount);

        LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
      }

      doc.save();
      doc.afterSave(function () {
        doc.makeReadyForSigning().add("author_signs_now", "true").sendAjax(function (docdata) {
          var newdoc = new Document(new Document({}).parse(docdata));
          newdoc.set({"screenshots": doc.get("screenshots")});
          newdoc.sign().sendAjax(function () {
            window.location = "/d" + (singleDocument ? "/" + doc.documentid() : "");
          });
        });
      });
    }
  },
  sendWithCSV: function (doc, index, totalCount) {
    var self = this;
    if (doc.csv() != undefined && doc.csv().length > 2) {
      doc.clone(function (doc2) {
        var name = doc.normalizeWithFirstCSVLine();

        var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
        $(".put-person-name", copyDocFor).text(name);

        var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
        $(".put-current-index", copyPartOf).text(index);
        $(".put-total-count", copyPartOf).text(totalCount);

        LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});

        doc.save();
        doc.afterSave(function () {
          doc.makeReadyForSigning().sendAjax(function () {
            doc2.dropFirstCSVLine();
            doc2.save();
            doc2.afterSave(function () {
              self.sendWithCSV(doc2, index + 1, totalCount);
            });
          });
        });
      }).sendAjax();
    } else {
      var name = doc.normalizeWithFirstCSVLine();

      var singleDocument = !doc.isCsv();
      if (!singleDocument) {
        var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
        $(".put-person-name", copyDocFor).text(name);

        var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
        $(".put-current-index", copyPartOf).text(index);
        $(".put-total-count", copyPartOf).text(totalCount);

        LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});
      }

      doc.save();
      doc.afterSave(function () {
        doc.makeReadyForSigning().sendAjax(function () {
          window.location = "/d" + (singleDocument ? "/" + doc.documentid() : "");
        });
      });
    }
  },
  render: function () {
    return (
      <div className="design-view-button-bar">
        <div className="design-view-button-bar-inner">
          { /* if */ (!this.props.document.isTemplate()) &&
            <Button
              className="button-save-draft"
              text={this.saveDraftButtonText()}
              onClick={this.onSaveDraftButtonClick}
            />
          }

          <Button
            className={(this.props.document.isTemplate() ? "button-save-template" : "button-save-as-template")}
            text={this.saveTemplateButtonText()}
            onClick={this.onSaveTemplateButtonClick}
          />

          { /* if */ (this.props.document.mainfile()) &&
            <Button
              className="button-remove-pdf"
              text={localization.designview.removeThisDocument}
              onClick={this.onRemoveDocumentButtonClick}
            />
          }

          { /* if */ (!this.props.document.isTemplate()) &&
            <Button
              text={localization.designview.startSigning}
              className={classNames("sendButton", {
                "disabled": (
                  !this.props.document.ready() || this.props.document.hasProblems()
                )
              })}
              onClick={this.onSendButtonClick}
            />
          }
        </div>
      </div>
    );
  }
});
