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
          if (self.isMounted()) {
            self.redirectIfItsNeeded();
          }
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
            new ErrorModal(xhr);
          }
        }
      });

      return {model: model, error: false, hasBeenRedirected: false, hasBeenSigned: false};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    componentDidMount: function () {
      this.state.model.initiateTransaction();
      window.addEventListener("message", this.onIFrameMessage);
    },

    componentWillUnmount: function () {
      window.removeEventListener("message", this.onIFrameMessage);
    },

    onIFrameMessage: function (ev) {
      var msg = ev.data;
      var msgType = msg.type;
      if (msgType === "error" || msgType === "abort" || msgType === "signed") {
        window.removeEventListener("message", this.onIFrameMessage);
      }
      if (msgType === "error") {
        var errorMsgs = {
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
          "wrongmobdob": localization.signNOBankIDError.mobiledob
        };
        var err = msg.err;
        new FlashMessage({
          type: "error",
          content: errorMsgs[err] || localization.signNOBankIDError.generic
        });
        this.props.onBack();
      }
      if (msgType === "abort") {
        new FlashMessage({
          type: "success",
          content: localization.signNOBankIDCanceled
        });
        this.props.onBack();
      }
      if (msgType === "signed") {
        this.setState({hasBeenSigned: true});
      }
    },

    redirectIfItsNeeded: function () {
      ReloadManager.stopBlocking();
      var bankID = this.state.model;
      setTimeout(function () {
        ReloadManager.startBlocking();
      }, 5000);
      if (BrowserInfo.isPadDevice()) {
        // on android adn ios9 devices iframes dont work at all
        window.top.location = bankID.bankIdUrl();
        this.setState({hasBeenRedirected: true});
      }
    },

    render: function () {
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });
      var bankID = this.state.model;

      return (
        <div className={divClass}>
          <h1>
            <div className="bankid-logo-wrapper" >
              <span className="bankid-logo no-bankid-logo" />
            </div>
            {localization.docsignview.eleg.bankid.signNOConfirmationTitle}
          </h1>
          {/* if */ (!this.state.hasBeenRedirected && ! this.state.hasBeenSigned) &&
            <iframe
              ref="iframe"
              style={{minHeight: "350px", width: "100%", margin: "auto", scrolling: "no"}}
              src={bankID.bankIdUrl()}
              />
          }
          {/* if */ this.state.hasBeenSigned &&
            <h2>{localization.signNOBankIDSuccess}</h2>
          }
        </div>
      );
    }
  });
