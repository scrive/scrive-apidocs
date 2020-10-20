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
var SignForward = require("./signforwardview");
var SignSigning = require("./signsigningview");
var SignApproving = require("./signapprovingview");
var SignProcess = require("./signprocessview");
var SignPin = require("./signpin");
var SignInputPin = require("./signinputpinview");
var SignEID = require("./signeidview");
var SignNetsEID = require("./signeidnetsview");
var SignEIDProcess = require("./signeidprocessview");
var SignEIDNetsProcess = require("./signeidnetsprocessview");
var IDINSignModel = require("../../eleg/idinsigning");
var EIDNOBankIDSignModel = require("../../eleg/eidnobankidsigning");
var SignIDINAuth = require("./signidinauth");
var SignEIDNOBankIDAuth = require("./signeidnobankidauth");
var SignEIDNOBankIDAuthChoose = require("./signeidnobankidauthchoose");
var FITupasSignModel = require("../../eleg/fitupassigning");
var SignFITupasAuth = require("./signfitupasauth");
var DKNemIDSignModel = require("../../eleg/dknemidsigning");
var SignDKNemIDAuth = require("./signeiddknemidauth");
var SignDKNemIDAuthChoose = require("./signeiddknemidauthchoose");
var SignDKNemIDAuthProcess = require("./signeiddknemidprocess");
var OnfidoSignModel = require("../../eleg/onfidosigning");
var SignOnfidoAuth = require("./signonfidoauth");
var VerimiQesSignModel = require("../../eleg/verimiqessigning");
var SignVerimiQes = require("./signverimiqes");
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

      return {
        initialStep: initialStep,
        step: initialStep,
        signedStatus: 0,
        eidThisDevice: false,
        askForPhone: model.askForPhone(),
        askForSSN: model.askForSSN(),
        iframeUrl: ""
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
      var useEIDHubForNOBankIDSign;
      if (window.fromTemplate !== undefined && fromTemplate.useEIDHubForNOBankIDSign !== undefined) {
        useEIDHubForNOBankIDSign = fromTemplate.useEIDHubForNOBankIDSign;
      } else {
        // workaround for tests
        useEIDHubForNOBankIDSign = false;
      }
      var document = this.props.model.document();
      var signatory = document.currentSignatory();
      var isApprover = signatory.approves();
      var hasPinAuth = signatory.smsPinAuthenticationToSign();
      var hasEIDAuth = signatory.seBankIDAuthenticationToSign();
      var hasEIDNets = !useEIDHubForNOBankIDSign && signatory.noBankIDAuthenticationToSign();
      var hasEIDNOBankID = useEIDHubForNOBankIDSign && signatory.noBankIDAuthenticationToSign();
      var hasSEBankIDAuth = signatory.seBankIDAuthenticationToSign();
      var hasIDINAuth = signatory.nlIDINAuthenticationToSign();
      var hasFITupasAuth = signatory.fiTupasAuthenticationToSign();
      var hasOnfidoAuth = signatory.onfidoDocumentCheckAuthenticationToSign()
                          || signatory.onfidoDocumentAndPhotoCheckAuthenticationToSign();
      var hasDKNemIDAuth = signatory.dkNemIDCPRAuthenticationToSign()
                            || signatory.dkNemIDPIDAuthenticationToSign()
                            || signatory.dkNemIDCVRAuthenticationToSign();
      var hasVerimiQesSign = signatory.verimiQesAuthenticationToSign();

      if (isApprover) {
        return "approve";
      }

      if (hasPinAuth) {
        return "pin";
      }

      if (hasSEBankIDAuth) {
        return "eid";
      }

      if (hasEIDNets) {
        return "eid-nets";
      }

      if (hasIDINAuth) {
        return "eid-idin-auth";
      }

      if (hasEIDNOBankID) {
        return "eid-nobankid-auth";
      }

      if (hasFITupasAuth) {
        return "eid-fi-tupas-auth";
      }

      if (hasOnfidoAuth) {
        return "eid-onfido-auth";
      }

      if (hasDKNemIDAuth) {
        return "eid-dknemid-auth";
      }

      if (hasVerimiQesSign) {
        return "eid-verimi-qes-sign";
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
        "approving", "forward", "eid-idin-auth", "eid-fi-tupas-auth",
        "eid-onfido-auth", "eid-nobankid-auth", "eid-nobankid-auth-choose",
        "eid-dknemid-auth", "eid-dknemid-auth-choose", "eid-dknemid-auth-process",
        "eid-verimi-qes-sign"
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
      var prevStep = prevState.step;
      var currStep = this.state.step;
      var stepChanged = prevStep !== currStep;
      var disableArrow = stepChanged && _.contains(["reject", "forward"], currStep);
      var enableArrow = stepChanged && _.contains(["reject", "forward"], prevStep);

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
      var noOverlayStep = [
        "sign", "approve", "finish", "pin", "eid", "eid-nets", "eid-idin-auth",
        "eid-fi-tupas-auth", "eid-onfido-auth", "eid-nobankid-auth",
        "eid-verimi-qes-sign", "eid-dknemid-auth"
      ];
      return !(noOverlayStep.indexOf(step) > -1);
    },

    canSignDocument: function () {
      return this.shouldHaveOverlay() || this.props.model.canSignDocument();
    },

    errorModal: function (xhr) {
      var document = this.props.model.document();
      var signatory = document.currentSignatory();
      var details = {"Document ID": document.documentid(),
                     "Signatory ID": signatory.signatoryid()};
      new ErrorModal(xhr, details);
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
    stringifiedForwardedDocumentData: function (doc, fieldsWithValues) {
      var name = doc.currentSignatory().name();
      var nfs = _.filter(fieldsWithValues, function (f) { return f.field.isName(); });
      if (nfs.length > 0) {
        var snfs = _.sortBy(nfs, function (f) { return f.field.order(); });
        name = _.reduce(snfs, function (acc, nf) { return acc + " " + nf.newValue; }, "");
      }
      name = name.trim() || localization.notNamedParty;
      var values = {
        name: name,
        title: doc.title(),
        delivery_method: doc.currentSignatory().get("delivery_method"),
        forsigning: doc.currentSignatory().signs()
      };
      return JSON.stringify(values);
    },
    handleForward: function (messageText, fieldsWithValues) {
      var model = this.props.model;
      var doc = model.document();
      var sdata = this.stringifiedForwardedDocumentData(doc, fieldsWithValues);
      doc.currentSignatory().forward(messageText, fieldsWithValues).sendAjax(function () {
        Cookies.set("forwarded_data", sdata);
        window.location = window.location.protocol + "//" + window.location.host +
          "/" + doc.lang() + "/afterforward/" + doc.documentid();
      }, function (xhr) {
        if (xhr.status == 403) {
          ScreenBlockingDialog.open({header: localization.sessionTimedoutInSignview});
        } else {
          new FlashMessage({
            type: "error",
            content: localization.signviewForwardingFailed,
            className: "flash-signview",
            withReload: true
          });
        }
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
          function (lastStatus, xhr) {
            if (xhr.status === 0) {
              setTimeout(function () {
                checkSigning();
              }, 1000);
              return;
            }
            bankIDSigning.setStatus(lastStatus);
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
            if (!self.isOnStep("eid-nets-process")) {
              // must have been cancelled, no reason to continue
              return;
            }
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

    handleIDINAuth: function (errorHandler) {
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new IDINSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler
      }).sign();
    },

    handleEIDNOBankIDAuth: function (errorHandler, useMobile) {
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new EIDNOBankIDSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler
      }).sign(useMobile);
    },

    handleFITupasAuth: function (errorHandler) {
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new FITupasSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler
      }).sign();
    },

    handleDKNemIDAuth: function (errorHandler, dkNemIDCVRMethod) {
      var self = this;
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new DKNemIDSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler,
        dkNemIDCVRMethod: dkNemIDCVRMethod
      }).startSignTransaction(function (accessUrl) {
         self.setState({iframeUrl: accessUrl}, function () {
           self.setStep("eid-dknemid-auth-process");
        });
      });
    },

    handleOnfidoAuth: function (errorHandler) {
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new OnfidoSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler
      }).sign();
    },

    handleVerimiQesSign: function (errorHandler) {
      if (!this.canSignDocument()) {
        errorHandler();
        return this.context.blinkArrow();
      }

      var document = this.props.model.document();
      var signatory = document.currentSignatory();

      new VerimiQesSignModel({
        doc: document,
        siglinkid: signatory.signatoryid(),
        errorHandler: errorHandler
      }).sign();
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
              onForward={this.handleSetStep("forward")}
              onReject={this.handleSetStep("reject")}
            />
          }
          {/* if */ this.isOnStep("approve") &&
            <SignApprove
              model={this.props.model}
              canApprove={true}
              onApprove={this.handleSetStep("approving")}
              onForward={this.handleSetStep("forward")}
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
              onForward={this.handleSetStep("forward")}
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
              onForward={this.handleSetStep("forward")}
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
              onForward={this.handleSetStep("forward")}
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
              onForward={this.handleSetStep("forward")}
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
          {/* if */ this.isOnStep("forward") &&
            <SignForward
              model={this.props.model}
              onBack={this.handleSetStep(this.state.initialStep)}
              onForward={this.handleForward}
            />
          }
          {/* if */ this.isOnStep("reject") &&
            <SignReject
              onBack={this.handleSetStep(this.state.initialStep)}
              onReject={this.handleReject}
              allowRejectionReason={doc.allowrejectreason()}
            />
          }
          {/* if */ this.isOnStep("eid-idin-auth") &&
            <SignIDINAuth
              model={this.props.model}
              canSign={this.canSignDocument()}
              field={phoneField}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  doc.takeSigningScreenshot(function () {
                    self.handleIDINAuth(function () {
                      self.setState({signingButtonBlocked: false});
                    });
                  }, {});
                }
              }}
            />
          }
          {/* if */ this.isOnStep("eid-nobankid-auth") &&
            <SignEIDNOBankIDAuth
              model={this.props.model}
              canSign={this.canSignDocument()}
              field={phoneField}
              showLegalText={this.props.showLegalText}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                doc.takeSigningScreenshot(function () {
                  self.setStep("eid-nobankid-auth-choose");
                });
              }}
            />
           }
           {/* if */ this.isOnStep("eid-nobankid-auth-choose") &&
            <SignEIDNOBankIDAuthChoose
              model={this.props.model}
              onBack={function () {
                self.setStep("eid-nobankid-auth");
              }}
              onChoice={function (useMobile) {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  self.handleEIDNOBankIDAuth(function () {
                    self.setState({signingButtonBlocked: false});
                  }, useMobile);
                }
              }}
            />
          }
          {/* if */ this.isOnStep("eid-fi-tupas-auth") &&
            <SignFITupasAuth
              model={this.props.model}
              canSign={this.canSignDocument()}
              field={phoneField}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  doc.takeSigningScreenshot(function () {
                    self.handleFITupasAuth(function () {
                      self.setState({signingButtonBlocked: false});
                    });
                  }, {});
                }
              }}
            />
          }
          {/* if */ this.isOnStep("eid-dknemid-auth") &&
            <SignDKNemIDAuth
              model={this.props.model}
              name={sig.name()}
              ssn={sig.personalnumber()}
              fieldSSN={ssnField}
              canSign={this.canSignDocument()}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                doc.takeSigningScreenshot(function () {
                  if (sig.dkNemIDCVRAuthenticationToSign()) {
                    self.setStep("eid-dknemid-auth-choose");
                  } else {
                    self.setState({signingButtonBlocked: true});
                    self.handleDKNemIDAuth(function () {
                      self.setState({signingButtonBlocked: false});
                    }, null);
                  }
                });
              }}
            />
          }
          {/* if */ this.isOnStep("eid-dknemid-auth-process") &&
            <SignDKNemIDAuthProcess
              model={this.props.model}
              iframeUrl={this.state.iframeUrl}
              onBack={this.handleSetStep("eid-dknemid-auth")}
              />
          }
          {/* if */ this.isOnStep("eid-dknemid-auth-choose") &&
            <SignDKNemIDAuthChoose
              model={this.props.model}
              canSign={this.canSignDocument()}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onChoice={function (dkNemIDCVRMethod) {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  self.handleDKNemIDAuth(function () {
                    self.setState({signingButtonBlocked: false});
                  }, dkNemIDCVRMethod);
                }
              }}
            />
          }

          {/* if */ this.isOnStep("eid-onfido-auth") &&
            <SignOnfidoAuth
              model={this.props.model}
              canSign={this.canSignDocument()}
              field={phoneField}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  doc.takeSigningScreenshot(function () {
                    self.handleOnfidoAuth(function () {
                      self.setState({signingButtonBlocked: false});
                    });
                  }, {});
                }
              }}
            />
          }
          {/* if */ this.isOnStep("eid-verimi-qes-sign") &&
            <SignVerimiQes
              model={this.props.model}
              canSign={this.canSignDocument()}
              field={phoneField}
              onReject={this.handleSetStep("reject")}
              onForward={this.handleSetStep("forward")}
              onSign={function () {
                if (!self.state.signingButtonBlocked) {
                  self.setState({signingButtonBlocked: true});
                  doc.takeSigningScreenshot(function () {
                    self.handleVerimiQesSign(function () {
                      self.setState({signingButtonBlocked: false});
                    });
                  }, {});
                }
              }}
            />
          }
        </div>
      );
    }
  });
