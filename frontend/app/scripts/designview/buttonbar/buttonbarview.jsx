var React = require("react");
var classNames = require("classnames");
var $ = require("jquery");
var _ = require("underscore");

var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var Track = require("../../common/track");
var Document = require("../../../js/documents.js").Document;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var Submit = require("../../../js/submits.js").Submit;
var DocumentSaveMixin = require("../document_save_mixin");
var Modal = require("../../common/modal");
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

var CantSignModalContent = require("./cantsignmodalcontent");
var ConfirmationModalAcceptButton = require("./confirmationmodalacceptbutton");
var SignConfirmationModalContent = require("./signconfirmationmodalcontent");
var SendConfirmationModalContent = require("./sendconfirmationmodalcontent");
var VerimiQesErrors = require("./verimiQesErrors");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin, DocumentSaveMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  componentWillMount: function () {
    this._confirmationModal = null;
  },
  getBackboneModels: function () {
    return [this.props.document];
  },
  getInitialState: function () {
    return {
      showCantSignModal: false,
      showSignConfirmationModal: false,
      acceptedDocument: false,
      showSendConfirmationModal: false,
      showVerimiQesProblemModal: false,
      canbestarted_errors: []
    };
  },
  showCantSignModal: function () {
    this.setState({showCantSignModal: true});
  },
  showSignConfirmationModal: function () {
    this.setState({showSignConfirmationModal: true});
  },
  showSendConfirmationModal: function () {
    this.setState({showSendConfirmationModal: true});
  },
  showVerimiQesProblemModal: function () {
    this.setState({showVerimiQesProblemModal: true});
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
      url: "/api/frontend/documents/" + doc.documentid() + "/setfile",
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
    if (!Subscription.currentSubscription().currentUserFeatures().canUseTemplates())  {
      this.refs.blockingModal.openContactUsModal();
    } else {
      Track.track("Click save as template");
      this.props.document.makeTemplate();
      this.saveDocument();
    }
  },
  onRemoveDocumentButtonClick: function () {
    Track.track("Click remove file");

    var doc = this.props.document;

    doc.markAsNotReady();
    this.saveAndFlashMessageIfAlreadySaved();

    doc.afterSave(this.removePDF);
  },
  onSaveDraftButtonClick: function () {
    Track.track("Click save as draft");
    this.saveDocument();
  },
  showSignOrSendConfirmationModal: function () {
    var doc = this.props.document;

    var isSigning = doc.authorCanSignFirst();

    Track.track("Click sign button", {
      "Is Signing": isSigning,
      "Uses eleg": doc.hasEleg(),
      "Uses email delivery": doc.hasEmail(),
      "Uses mobile delivery": doc.hasSMS(),
      "Uses pad delivery": doc.hasPad(),
      "Uses email and mobile delivery": doc.hasEmailAndSMS()
    });

    doc.save();
    var numberOfDocs = doc.isCsv() ? doc.csv().length - 1 : 1;
    if (Subscription.currentSubscription().isOverLimit(numberOfDocs)) {
      this.refs.blockingModal.openContactUsModal();
    } else if (isSigning) {
      this.showSignConfirmationModal();
    } else {
      this.showSendConfirmationModal();
    }
  },

  onSendButtonClick: function () {
    const document = this.props.document;
    if (document.hasProblems()) {
      this.showCantSignModal();
    } else if (document.usesVerimiQes()) {
      var self = this;
        document.save(function () {
          return new Submit({
            url: "/api/frontend/documents/" + document.documentid() +  "/canbestarted",
            method: "GET",
            ajax: true,
            ajaxsuccess: function (resp) {
                if (resp.can_start) {
                  self.showSignOrSendConfirmationModal();
                } else {
                  self.setState({canbestarted_errors: resp.errors});
                  self.showVerimiQesProblemModal();
                }
            }
          }).send();
        });
    } else {
      this.showSignOrSendConfirmationModal();
    }
  },
  onCantSignModalAcceptClose: function () {
    this.setState({showCantSignModal: false});
  },
  onSignConfirmationModalAccept: function () {
    Track.track("Click accept sign", {"Button": "sign"});

    this.setState({acceptedDocument: true});

    var self = this;
    var doc = this.props.document;

    doc.takeSigningScreenshot(function () {
      doc.afterSave(function () {
        self.signWithCSV(doc, 1, doc.isCsv() ? doc.csv().length - 1 : undefined);
      });
    });
  },
  onSignConfirmationModalCancel: function () {
    this.setState({showSignConfirmationModal: false});
  },
  onSendConfirmationModalAccept: function () {
    this.setState({acceptedDocument: true});

    var self = this;
    var doc = this.props.document;

    Track.track("Click accept sign", {"Button": "send"});
    doc.takeSigningScreenshot(function () {
      self.sendWithCSV(doc, 1, doc.isCsv() ? doc.csv().length - 1 : undefined);
    });
  },
  onVerimiQesProblemModalCancel: function () {
    this.setState({showVerimiQesProblemModal: false});
  },
  onSendConfirmationModalCancel: function () {
    this.setState({showSendConfirmationModal: false});
  },
  signWithCSV: function (doc, index, totalCount) {
    var self = this;
    if (doc.csv() != undefined && doc.csv().length > 2) {
      doc.clone(function (doc2) {
        var name = doc.normalizeWithFirstCSVLine();
        doc.setTitle(doc.title() + " - " + name);

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
                doc2.adjustAuthorFieldsAfterCloning(doc);
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
      var singleDocument = !doc.isCsv();
      var name = doc.normalizeWithFirstCSVLine();

      if (!singleDocument) {
        doc.setTitle(doc.title() + " - " + name);
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
        doc.setTitle(doc.title() + " - " + name);

        var copyDocFor = $("<span>" + localization.designview.preparingDocument + "</span>");
        $(".put-person-name", copyDocFor).text(name);

        var copyPartOf = $("<span>" + localization.designview.numPartOfNum + "</span>");
        $(".put-current-index", copyPartOf).text(index);
        $(".put-total-count", copyPartOf).text(totalCount);

        LoadingDialog.open({header: copyDocFor, subheader: copyPartOf});

        doc.save();
        doc.afterSave(function () {
          doc.makeReadyForSigning().sendAjax(function () {
            doc2.adjustAuthorFieldsAfterCloning(doc);
            doc2.dropFirstCSVLine();
            doc2.save();
            doc2.afterSave(function () {
              self.sendWithCSV(doc2, index + 1, totalCount);
            });
          });
        });
      }).sendAjax();
    } else {
      var singleDocument = !doc.isCsv();
      var name = doc.normalizeWithFirstCSVLine();

      if (!singleDocument) {
        doc.setTitle(doc.title() + " - " + name);
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
    var otherSignatoriesSignInPerson = _.every(
      this.props.document.signatories(), function (sig) {
        return sig.padDelivery() || sig.author();
      }
    );

    var sendConfirmationModalTitle = null;
    var sendConfirmationModalAcceptText = null;

    if (otherSignatoriesSignInPerson) {
      sendConfirmationModalTitle = localization.process.startsigningbuttontext;
      sendConfirmationModalAcceptText = localization.process.startsigningbuttontext;
    } else {
      sendConfirmationModalTitle = localization.process.sendbuttontext;
      sendConfirmationModalAcceptText = localization.process.sendbuttontext;
    }

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
            locked={!Subscription.currentSubscription().currentUserFeatures().canUseTemplates()}
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

          { /* if */ (this.props.document.ready()) &&
            <Modal.Container active={this.state.showCantSignModal}>
              <Modal.Header
                title={localization.designview.cantSignModal.title}
                onClose={this.onCantSignModalAcceptClose}
                showClose={false}
              />
              <Modal.Content>
                <CantSignModalContent />
              </Modal.Content>
              <Modal.Footer>
                <Modal.AcceptButton onClick={this.onCantSignModalAcceptClose} />
              </Modal.Footer>
            </Modal.Container>
          }

          { /* if */ (this.props.document.ready()) &&
            <Modal.Container active={this.state.showSignConfirmationModal} >
              <Modal.Header
                title={localization.signByAuthor.modalTitle}
                onClose={this.onSignConfirmationModalCancel}
                showClose={!this.state.acceptedDocument}
              />
              <Modal.Content>
                <SignConfirmationModalContent document={this.props.document} />
              </Modal.Content>
              <Modal.Footer>
                { /* if */ (this.props.document.ready()) &&
                  <Modal.CancelButton onClick={this.onSignConfirmationModalCancel} />
                }
                <ConfirmationModalAcceptButton
                  onClick={this.onSignConfirmationModalAccept}
                  text={localization.designview.sign}
                />
              </Modal.Footer>
            </Modal.Container>
          }

          { /* if */ (this.props.document.ready()) &&
            <Modal.Container active={this.state.showSendConfirmationModal} >
              <Modal.Header
                title={sendConfirmationModalTitle}
                onClose={this.onSendConfirmationModalCancel}
                showClose={!this.state.acceptedDocument}
              />
              <Modal.Content>
                <SendConfirmationModalContent
                  document={this.props.document}
                  otherSignatoriesSignInPerson={otherSignatoriesSignInPerson}
                />
              </Modal.Content>
              <Modal.Footer>
                { /* if */ (!this.state.acceptedDocument) &&
                  <Modal.CancelButton onClick={this.onSendConfirmationModalCancel} />
                }
                <ConfirmationModalAcceptButton
                  onClick={this.onSendConfirmationModalAccept}
                  text={sendConfirmationModalAcceptText}
                />
              </Modal.Footer>
            </Modal.Container>
          }
          { /* if */ (this.props.document.ready()) &&
            <Modal.Container active={this.state.showVerimiQesProblemModal} >
              <Modal.Header
                title={"Verimi QES Problems"}
                onClose={this.onVerimiQesProblemModalCancel}
              />
              <Modal.Content>
              <VerimiQesErrors errors={this.state.canbestarted_errors}/>
              </Modal.Content>
              <Modal.Footer>
                <Button
                  type="action"
                  text={"Ok"}
                  oneClick={false}
                  onClick={this.onVerimiQesProblemModalCancel}
                />
              </Modal.Footer>
            </Modal.Container>
          }
          <BlockingModal ref="blockingModal"/>
        </div>
      </div>
    );
  }
});
