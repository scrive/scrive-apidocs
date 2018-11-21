var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var TransitionMixin = require("./transition_mixin");
var TaskMixin = require("../navigation/task_mixin");
var SignFinish = require("./signfinishview");
var SignSign = require("./signsignview");
var SignApprove = require("./signapproveview");
var SignReject = require("./signrejectview");
var SignSigning = require("./signsigningview");
var SignApproving = require("./signapprovingview");
var SignProcess = require("./signprocessview");
var SignPin = require("./signpin");
var SignInputPin = require("./signinputpinview");
var SignEID = require("./signeidview");
var SignNetsEID = require("./signeidnetsview");
var SignEIDProcess = require("./signeidprocessview");
var SignEIDNetsProcess = require("./signeidnetsprocessview");
var ErrorModal = require("../errormodal");
var ReloadManager = require("../../../js/reloadmanager.js").ReloadManager;
var $ = require("jquery");
var ScreenBlockingDialog = require("../../../js/dialog.js").ScreenBlockingDialog;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var FlashMessagesCleaner = require("../../../js/flashmessages.js").FlashMessagesCleaner;
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;
var Track = require("../../common/track");
var classNames = require("classnames");
var Task = require("../navigation/task");

  module.exports = React.createClass({
    displayName: "SignSectionView",

    mixins: [TransitionMixin, TaskMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      className: React.PropTypes.string,
      pixelWidth: React.PropTypes.number.isRequired,
      enableOverlay: React.PropTypes.func.isRequired,
      disableOverlay: React.PropTypes.func.isRequired,
      showLegalText: React.PropTypes.bool.isRequired,
      highlighting: React.PropTypes.bool.isRequired
    },

    contextTypes: {
      showArrow: React.PropTypes.func,
      hideArrow: React.PropTypes.func,
      blinkArrow: React.PropTypes.func
    },

    getInitialState: function () {
      var model = this.props.model;
      var initialStep = this.getInitialStep();
      var currentSignatory = model.document().currentSignatory();

      var thisDevice = true;
      if (currentSignatory.seBankIDAuthenticationToSign()) {
        thisDevice = !currentSignatory.padDelivery();
      }

      return {
        initialStep: initialStep,
        step: initialStep,
        signedStatus: 0,
        eidThisDevice: thisDevice,
        askForPhone: model.askForPhone(),
        askForSSN: model.askForSSN()
      };
    },

    createTasks: function () {
      var self = this;
      return [new Task({
        type: "overlay",
        tipSide: "none",
        isComplete: function () {
          return !self.shouldHaveOverlay();
        },
        el: $(self.getDOMNode())
      })];
    },

    shouldTransition: function (prevProps, prevState) {
      return prevState.step !== this.state.step;
    },

    getInitialStep: function () {
      var document = this.props.model.document();
      var signatory = document.currentSignatory();
      var isApprover = signatory.approves();
      var hasPinAuth = signatory.smsPinAuthenticationToSign();
      var hasEIDAuth = signatory.seBankIDAuthenticationToSign();
      var hasEIDNets = signatory.noBankIDAuthenticationToSign() || signatory.dkNemIDAuthenticationToSign();

      if (isApprover) {
        return "approve";
      }

      if (hasPinAuth) {
        return "pin";
      }

      if (hasEIDAuth) {
        return "eid";
      }

      if (hasEIDNets) {
        return "eid-nets";
      }

      if (signatory.hasPlacedObligatorySignatures()) {
        return "finish";
      }

      return "sign";
    },

    isValidStep: function (step) {
      var steps = [
        "sign", "finish", "signing", "process", "eid", "eid-process", "pin",
        "input-pin", "reject", "eid-nets", "eid-nets-process", "approve",
        "approving"
      ];

      var valid = steps.indexOf(step) > -1;

      if (!valid) {
        throw new Error(step + " is not a valid step in SignSection");
      }
    },

    componentDidUpdate: function (prevProps, prevState) {
      var model = this.props.model;
      var hadOverlay = this.shouldHaveOverlay(prevState.step);
      var shouldHaveOverlay = this.shouldHaveOverlay();
      var disableArrow = prevState.step !== "reject" && this.state.step === "reject";
      var enableArrow = prevState.step === "reject" && this.state.step !== "reject";

      if (disableArrow) {
        setTimeout(() => {
          this.context.hideArrow();
        });
      }

      if (enableArrow) {
        setTimeout(() => {
          this.context.showArrow();
        });
      }

      if (!hadOverlay && shouldHaveOverlay) {
        this.props.enableOverlay();
      }

      if (hadOverlay && !shouldHaveOverlay) {
        this.props.disableOverlay();
      }
    },

    isOnStep: function (step) {
      this.isValidStep(step);

      return this.state.step === step;
    },

    setStep: function (step) {
      this.isValidStep(step);

      this.setState({step: step});
    },

    setSignedStatus: function (status) {
      this.setState({signedStatus: status});
    },

    handleSetStep: function (step) {
      return function (e) {
        this.setStep(step);
      }.bind(this);
    },

    shouldHaveOverlay: function (step) {
      step = step || this.state.step;
      var noOverlayStep = ["sign", "approve", "finish", "pin", "eid", "eid-nets"];
      return !(noOverlayStep.indexOf(step) > -1);
    },

    canSignDocument: function () {
      return this.shouldHaveOverlay() || this.props.model.canSignDocument();
    },

    errorModal: function (xhr) {
      var document = this.props.model.document();
      var signatory = document.currentSignatory();
      new ErrorModal(xhr, {"Document ID": document.documentid(), "Signatory ID": signatory.signatoryid()});
    },

    handleReject: function (text) {
      var model = this.props.model;
      var doc = model.document();

      Track.track_timeout("Accept", {"Accept": "reject document"}, function () {
        doc.currentSignatory().reject(text).sendAjax(function () {
          var shouldRedirect = doc.currentSignatory().rejectredirect() != undefined &&
            doc.currentSignatory().rejectredirect() != "";
          ReloadManager.stopBlocking();
          if (shouldRedirect) {
            window.location = doc.currentSignatory().rejectredirect();
          } else {
            $(window).on("beforeunload pagehide", function () {
              $(window).scrollTop(0);
            });
            window.location.reload();
          }
        }, function (xhr) {
          if (xhr.status == 403) {
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
          } else {
            new FlashMessage({
              type: "error",
              content: localization.signviewSigningFailed,
              className: "flash-signview",
              withReload: true
            });
          }
        });
      });
    },
    handleSignSwedishEID: function (bankIDSigning) {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();

      if (!self.canSignDocument()) {
        return this.context.blinkArrow();
      }

      var successCallback = function () {
        new FlashMessagesCleaner();
        var timeout = window.SIGN_TIMEOUT || 0;
        setTimeout(function () {
          document.sign(errorCallback("sign"), function (newDocument, oldDocument) {
            self.handleWaitingForSwedishBankID(bankIDSigning);
          }, {}).send();
        }, timeout);
      };

      var errorCallback = function (apicall) {
        return function (xhr) {
          if (xhr.status == 403) {
            ReloadManager.stopBlocking();
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
          } else if (xhr.status === undefined || xhr.status === 0) {
            // let's retry in a second
            setTimeout(function () {
              if (apicall === "check") {
                document.checksign(successCallback, errorCallback("check"), {}).send();
              } else {
                document.sign(errorCallback("sign"), function (newDocument, oldDocument) {
                  self.handleWaitingForSwedishBankID(bankIDSigning);
                }, {}).send();
              }
            }, 1000);
          } else {
            ReloadManager.stopBlocking();
            self.errorModal(xhr);
          }
        };
      };

      document.checksign(successCallback, errorCallback("check"), {}).send();
    },

    handleWaitingForSwedishBankID: function (bankIDSigning) {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var checkSigning = function () {
        document.checkingSigning(
          function () { self.handleAfterSignOrApproveRedirectOrReload(); },
          function (s) {
            bankIDSigning.setStatus(s);
            setTimeout(checkSigning, 1000);
          },
          function (xhr) {
            if (xhr.status === 0) {
              setTimeout(function () {
                checkSigning();
              }, 1000);
              return;
            }
            bankIDSigning.setStatus(xhr);
            bankIDSigning.triggerFail();
            self.handleFinishAfterSwedishBankIDFailed();
          }
        ).send();
       };
      checkSigning();
    },
    handleFinishAfterSwedishBankIDFailed: function () {
      if (!this.isOnStep("eid")) {
        this.setStep("eid");
      }
    },
    handleCancelSwedishBankID: function () {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();
       document.cancelSigning(
          function () { self.setStep("eid"); },
          function (xhr) { self.errorModal(xhr); }
        ).send();
    },
    handleSignNets: function (netsSigning) {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();

      if (!self.canSignDocument()) {
        return this.context.blinkArrow();
      }

      var errorCallback = function (xhr) {
        if (xhr.status == 403) {
          ReloadManager.stopBlocking();
          ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
        } else {
          ReloadManager.stopBlocking();
          self.errorModal(xhr);
        }
      };

      document.checksign(function () {
        new FlashMessagesCleaner();
        var timeout = window.SIGN_TIMEOUT || 0;
        setTimeout(function () {
          document.sign(errorCallback, function (newDocument, oldDocument) {
            self.handleWaitingForNets(netsSigning);
          }, {}).send();
        }, timeout);
      }, errorCallback, {}).send();
    },

    handleWaitingForNets: function (netsSigning) {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var checkSigning = function () {
        document.checkingSigning(
          function () { self.handleAfterSignOrApproveRedirectOrReload(); },
          function (s) {
            netsSigning.setStatus(s);
            setTimeout(checkSigning, 1000);
          },
          function (s) {
            netsSigning.setStatus(s);
            netsSigning.triggerFail();
            self.handleFinishAfterNetsFailed();
          }
        ).send();
       };
      checkSigning();
    },
    handleFinishAfterNetsFailed: function () {
      if (!this.isOnStep("eid-nets")) {
        this.setStep("eid-nets");
      }
    },
    handleCancelNets: function () {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();
       document.cancelSigning(
          function () { self.setStep("eid-nets"); },
          function (xhr) { self.errorModal(xhr); }
        ).send();
    },
    handleSign: function (pin) {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();

      if (!self.canSignDocument()) {
        return this.context.blinkArrow();
      }

      if (signatory.smsPinAuthenticationToSign() && !pin) {
        return new FlashMessage({
          type: "error",
          content: localization.docsignview.pinSigning.noPinProvided,
          className: "flash-signview"
        });
      }

      var errorCallback = function (xhr) {
        var data = {};
        try {
          data = JSON.parse(xhr.responseText);
        } catch (e) {
          Track.track_timeout("check call response parse error");
        }

        if (xhr.status == 400 &&
            data.error_message === "The parameter 'sms_pin' had the following problems: invalid SMS PIN") {
          self.setStep("input-pin");
          new FlashMessage({
            content: localization.docsignview.pinSigning.invalidPin,
            className: "flash-signview", type: "error"
          });
        } else {
          if (xhr.status == 403) {
            ReloadManager.stopBlocking();
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
          } else {
            ReloadManager.stopBlocking();
            self.errorModal(xhr);
          }
        }
      };

      var pinParam = signatory.smsPinAuthenticationToSign() ? {sms_pin: pin} : {};

      document.checksign(function () {
        new FlashMessagesCleaner();
        document.takeSigningScreenshot(function () {
          self.setStep("process");
          self.setSignedStatus(1);

          Track.track_timeout("Accept", {"Accept": "sign document"});

          var timeout = window.SIGN_TIMEOUT || 0;
          setTimeout(function () {
            document.sign(errorCallback, function (newDocument, oldDocument) {
              self.setSignedStatus(2);
              self.handleAfterSignOrApproveRedirectOrReload();
            }, pinParam).send();
          }, timeout);
        });
      }, errorCallback, pinParam).send();
    },
    handleApprove: function () {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var signatory = document.currentSignatory();

      var errorCallback = function (xhr) {
          if (xhr.status == 403) {
            ReloadManager.stopBlocking();
            ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
          } else {
            ReloadManager.stopBlocking();
            self.errorModal(xhr);
          }
      };


      document.approve(errorCallback, function (newDocument, oldDocument) {
        self.handleAfterSignOrApproveRedirectOrReload();
      }).send();

    },

    handleAfterSignOrApproveRedirectOrReload: function () {
      var self = this;
      var model = self.props.model;
      var document = self.props.model.document();
      var redirect = document.currentSignatory().signsuccessredirect();
      ReloadManager.stopBlocking();
      if (redirect) {
        window.location = redirect;
      } else {
        $(window).on("beforeunload pagehide", function () {
          $(window).scrollTop(0);
        });
        window.location.reload();
      }
    },
    handlePin: function () {
      var self = this;
      var model = self.props.model;
      var doc = model.document();
      var sig = doc.currentSignatory();
      var phoneField = sig.mobileField();
      var askForPhone = model.askForPhone();
      var phone = phoneField.value();

      if (askForPhone) {
        return new FlashMessage({
          type: "error",
          content: localization.docsignview.pinSigning.invalidPhone,
          className: "flash-signview"
        });
      }

      Track.track("Requesting SMS PIN", {
        documentid: doc.documentid(),
        signatoryid: sig.signatoryid(),
        phone: phone
      });

      doc.requestPinToSign(function () {
        self.setStep("input-pin");
      }, function (xhr) {
        ReloadManager.stopBlocking();
        self.errorModal(xhr);
      }).send();
    },

    handleStartEID: function (thisDevice) {
      var self = this;
      var model = self.props.model;

      if (!self.canSignDocument()) {
        return this.context.blinkArrow();
      }

      self.setState({eidThisDevice: thisDevice}, function () {
        self.setStep("eid-process");
      });
    },

    handleStartNets: function (thisDevice) {
      var self = this;
      var model = self.props.model;

      if (!self.canSignDocument()) {
        return this.context.blinkArrow();
      }

      self.setState({eidThisDevice: thisDevice}, function () {
        self.setStep("eid-nets-process");
      });
    },

    render: function () {
      var self = this;
      var model = this.props.model;
      var doc = model.document();
      var sig = doc.currentSignatory();
      var queryPart = doc.mainfile().queryPart({pixelwidth: this.props.pixelWidth});
      var imgUrl = "/pages/" + doc.mainfile().fileid() + "/1" + queryPart;

      var phoneField = sig.mobileField();
      var ssnField = sig.personalnumberField();

      var sectionClass = classNames({
        "section": true,
        "sign": true,
        "small-screen": BrowserInfo.isSmallScreen(),
        "above-overlay": this.shouldHaveOverlay()
      });

      return (
        <div className={sectionClass}>
          {/* if */ this.isOnStep("sign") &&
            <SignSign
              model={this.props.model}
              canSign={this.canSignDocument() && !this.props.highlighting}
              onSign={this.handleSetStep("signing")}
              onReject={this.handleSetStep("reject")}
            />
          }
          {/* if */ this.isOnStep("approve") &&
            <SignApprove
              model={this.props.model}
              canApprove={true}
              onApprove={this.handleSetStep("approving")}
              onReject={this.handleSetStep("reject")}
            />
          }
          {/* if */ this.isOnStep("finish") &&
            <SignFinish
              model={this.props.model}
              title={doc.title()}
              name={sig.name()}
              canSign={this.canSignDocument() && !this.props.highlighting}
              onSign={this.handleSign}
              onReject={this.handleSetStep("reject")}
              showLegalText={this.props.showLegalText}
            />
          }
          {/* if */ this.isOnStep("signing") &&
            <SignSigning
              model={this.props.model}
              title={doc.title()}
              name={sig.name()}
              canSign={this.canSignDocument()}
              onBack={this.handleSetStep("sign")}
              onSign={this.handleSign}
              showLegalText={this.props.showLegalText}
            />
          }
          {/* if */ this.isOnStep("approving") &&
            <SignApproving
              model={this.props.model}
              title={doc.title()}
              name={sig.name() || localization.process.nonsignatoryname}
              canApprove={this.canSignDocument()}
              onApprove={this.handleApprove}
              onBack={this.handleSetStep("approve")}
            />
          }
          {/* if */ this.isOnStep("process") &&
            <SignProcess
              imgUrl={imgUrl}
              pageHeight={doc._firstPageHeight}
              pageWidth={this.props.pixelWidth}
              docTitle={doc.title()}
              status={this.state.signedStatus}
            />
          }
          {/* if */ this.isOnStep("eid") &&
            <SignEID
              model={this.props.model}
              field={ssnField}
              name={sig.name()}
              askForSSN={this.state.askForSSN}
              canSign={this.canSignDocument()}
              ssn={sig.personalnumber()}
              thisDevice={this.state.eidThisDevice}
              onReject={this.handleSetStep("reject")}
              onSign={ function (thisDevice) {
                  doc.takeSigningScreenshot(function () {
                    self.handleStartEID(thisDevice);
                  }, {});
                }
              }
              showLegalText={this.props.showLegalText}
            />
          }
          {/* if */ this.isOnStep("eid-process") &&
            <SignEIDProcess
              ssn={sig.personalnumber()}
              signatory={sig}
              thisDevice={this.state.eidThisDevice}
              onBack={this.handleCancelSwedishBankID}
              onInitiated={function (bankIDSigning) { self.handleSignSwedishEID(bankIDSigning); }}
            />
          }
          {/* if */ this.isOnStep("eid-nets") &&
            <SignNetsEID
              model={this.props.model}
              fieldSSN={ssnField}
              name={sig.name()}
              askForSSN={this.state.askForSSN}
              canSign={this.canSignDocument()}
              ssn={sig.personalnumber()}
              onReject={this.handleSetStep("reject")}
              onSign={ function () {
                  doc.takeSigningScreenshot(function () {
                    self.handleStartNets();
                  }, {});
                }
              }
              showLegalText={this.props.showLegalText}
            />
          }
          {/* if */ this.isOnStep("eid-nets-process") &&
            <SignEIDNetsProcess
              ssn={sig.personalnumber()}
              signatory={sig}
              onBack={this.handleCancelNets}
              onInitiated={function (netsSigning) { self.handleSignNets(netsSigning); }}
            />
          }
          {/* if */ this.isOnStep("pin") &&
            <SignPin
              model={this.props.model}
              canSign={this.canSignDocument()}
              askForPhone={this.state.askForPhone}
              field={phoneField}
              onReject={this.handleSetStep("reject")}
              onNext={this.handlePin}
            />
          }
          {/* if */ this.isOnStep("input-pin") &&
            <SignInputPin
              title={doc.title()}
              name={sig.name()}
              onBack={this.handleSetStep("pin")}
              onSign={this.handleSign}
              showLegalText={this.props.showLegalText}
            />
          }
          {/* if */ this.isOnStep("reject") &&
            <SignReject
              onBack={this.handleSetStep(this.state.initialStep)}
              onReject={this.handleReject}
              allowRejectionReason={doc.allowrejectreason()}
            />
          }
        </div>
      );
    }
  });
