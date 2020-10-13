var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var NetsSigning = require("../../eleg/netssigning");
var ViewSize = require("../viewsize");
var ErrorModal = require("../errormodal");
var Signatory = require("../../../js/signatories.js").Signatory;
var ReloadManager = require("../../../js/reloadmanager.js").ReloadManager;
var BrowserInfo = require("../../../js/utils/browserinfo.js").BrowserInfo;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Button = require("../../common/button");
var $ = require("jquery");
var classNames = require("classnames");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      ssn: React.PropTypes.string.isRequired,
      signatory: React.PropTypes.instanceOf(Signatory).isRequired,
      thisDevice: React.PropTypes.bool.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onInitiated: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      var self = this;

      var model = new NetsSigning({
        signatory: this.props.signatory,
        onStatusChange: function () {
        },
        onInitiated: function () {
          if (self.isMounted()) {
            self.props.onInitiated(model);
          }
        },
        onFail: function () {
          if (self.isMounted()) {
            new FlashMessage({
              type: "error",
              content: model.statusMessage(),
              className: "flash-signview"
            });
            self.setState({error: true});
          }
        },
        onCriticalError: function (xhr) {
          if (self.isMounted()) {
            ReloadManager.stopBlocking();
            var signatory = self.props.signatory;
            var document = signatory.document();
            var details = {"Document ID": document.documentid(),
                           "Signatory ID": signatory.signatoryid()};
            new ErrorModal(xhr, details);
          }
        }
      });

      return {model: model, error: false, signStatus: "choose_signing_method"};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    componentDidMount: function () {
      window.addEventListener("message", this.onIFrameMessage);
    },

    componentWillUnmount: function () {
      window.removeEventListener("message", this.onIFrameMessage);
    },

    netsErrorMsgs: function () {
      var signatory = this.state.model.signatory();
      if (signatory.noBankIDAuthenticationToSign()) {
        return {
          "buypass.certificatemissing": localization.signNOBankIDError.generic,
          "cookienotfound": localization.signNOBankIDError.useragent,
          "errorauthenticating": localization.signNOBankIDError.auth,
          "errorsigningdocument": localization.signNOBankIDError.document,
          "generalerror": localization.signNOBankIDError.generic,
          "insufficientauthpermission": localization.signNOBankIDError.auth,
          "insufficientsignpermission": localization.signNOBankIDError.auth,
          "internalerror": localization.signNOBankIDError.generic,
          "invalidid": localization.signNOBankIDError.auth,
          "javanotfound": localization.signNOBankIDError.useragent,
          "javascriptnotfound": localization.signNOBankIDError.useragent,
          "servicenotavailable": localization.signNOBankIDError.generic,
          "sessiontimeout": localization.signNOBankIDError.timeout,
          "signrefnotfound": localization.signNOBankIDError.document,
          "toooldjava": localization.signNOBankIDError.useragent,
          "tooyoungsigner": localization.signNOBankIDError.age,
          "unsupportedua": localization.signNOBankIDError.useragent,
          "wrongmobdob": localization.signNOBankIDError.mobiledob,
          "canceled": localization.signNOBankIDCanceled
        };
      }
    },

    onIFrameMessage: function (ev) {
      var msg = ev.data;
      var msgType = msg.type;
      var errorMsgs = this.netsErrorMsgs();

      if (msgType === "error" || msgType === "abort" || msgType === "signed") {
        window.removeEventListener("message", this.onIFrameMessage);
      }
      if (msgType === "error") {
        var err = msg.err;
        new FlashMessage({
          type: "error",
          content: errorMsgs[err] || errorMsgs["generalerror"]
        });
        this.props.onBack();
      }
      if (msgType === "abort") {
        new FlashMessage({
          type: "success",
          content: errorMsgs["canceled"]
        });
        this.props.onBack();
      }
      if (msgType === "signed") {
        this.setState({signStatus: "signed"});
      }
    },

    onNOBankIDSigningMethodClassic: function () {
      this.state.model.initiateTransaction("nobankid_classic");
      this.setState({signStatus: "signing_in_progress"});
    },

    onNOBankIDSigningMethodMobile: function () {
      this.state.model.initiateTransaction("nobankid_mobile");
      this.setState({signStatus: "signing_in_progress"});
    },

    onNemIDSigningMethodEmployeeKeycard: function () {
      this.state.model.initiateTransaction("dknemid_employee_keycard");
      this.setState({signStatus: "signing_in_progress"});
    },

    onNemIDSigningMethodEmployeeKeyfile: function () {
      this.state.model.initiateTransaction("dknemid_employee_keyfile");
      this.setState({signStatus: "signing_in_progress"});
    },

    onNemIDSigningMethodPersonalKeycard: function () {
      this.state.model.initiateTransaction("dknemid_personal_keycard");
      this.setState({signStatus: "signing_in_progress"});
    },

    onCancel: function () {
      var errorMsgs = this.netsErrorMsgs();
      new FlashMessage({
        type: "success",
        content: errorMsgs["canceled"]
      });
      this.props.onBack();
    },

    render: function () {
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });
      var bankID = this.state.model;
      var document = this.state.model.document();
      var signatory = this.state.model.signatory();
      var confirmationTitle;
      var logoClass;
      var successText;
      if (signatory.noBankIDAuthenticationToSign()) {
        confirmationTitle = localization.docsignview.eleg.bankid.signNOConfirmationTitle;
        logoClass = classNames({"bankid-logo-nets": true, "no-bankid-logo": true});
        successText = localization.signNOBankIDSuccess;
      }
      var transctionNumberText = localization.docsignview.eleg.bankid.transactionNumber;
      var transactionNumberFirst = String(document.id).slice(0, -4);
      var transactionNumberLast = String(document.id).slice(-4);
      return (
        <div className={divClass}>
          <h1 className="nets-sign-process">
            <span className={logoClass}/>
            {confirmationTitle}
          </h1>
          <p className="eid-process-view-doc-id">
            {transctionNumberText} {transactionNumberFirst}<em>{transactionNumberLast}</em>
          </p>
          {/* if */ (this.state.signStatus == "signing_in_progress") &&
            <div>
              <iframe
                ref="iframe"
                style={{minHeight: "430px", width: "100%", margin: "20px auto auto auto", overflow: "auto"}}
                src={bankID.bankIdUrl()}
                />
              <div className="button-thin">
                <Button
                  text={localization.cancel}
                  className="transparent-button button-block"
                  onClick={this.onCancel}
                />
              </div>
            </div>
          }
          {/* if */ (this.state.signStatus == "choose_signing_method") && signatory.noBankIDAuthenticationToSign() &&
            <div>
              <div className="nets-sign-process">
                {localization.docsignview.eleg.bankid.signChooseMethod}
              </div>
              <Button
                text={localization.docsignview.eleg.bankid.signNOSignMethodClassic}
                onClick={this.onNOBankIDSigningMethodClassic}
                className="button-block"
              />
              <Button
                text={localization.docsignview.eleg.bankid.signNOSignMethodMobile}
                onClick={this.onNOBankIDSigningMethodMobile}
                className="button-block"
              />
              <Button
                text={localization.cancel}
                className="transparent-button button-block"
                onClick={this.onCancel}
              />
            </div>
          }
          {/* if */ (this.state.signStatus == "signed") &&
            <h2>{successText}</h2>
          }
        </div>
      );
    }
  });
