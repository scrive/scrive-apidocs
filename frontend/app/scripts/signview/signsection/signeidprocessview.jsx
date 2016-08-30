var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Button = require("../../common/button");
var BankIDSigning = require("../../eleg/bankidsigning");
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

      var model = new BankIDSigning({
        signatory: this.props.signatory,
        thisDevice: this.props.thisDevice,
        onStatusChange: function () {
          if (self.isMounted()) {
            self.addBankIDIframeIfItsNeeded();
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

      return {model: model, error: false, startTime: new Date(), hasBeenRedirected: false};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    componentDidMount: function () {
      this.state.model.initiateTransaction();
    },

    hasStartNowButton: function () {
      var bankID = this.state.model;
      var timeHasPassed = new Date() - this.state.startTime > 5000;
      return !this.state.error && !bankID.isFaultStatus() &&
             !bankID.isWaitingForToken() && bankID.thisDevice() && timeHasPassed;
    },

    redirectToBankIDApp: function () {
      // when window.onbeforeunload is set causes alert on android devices
      // so disable onbeforeunload for some time
      var bankID = this.state.model;
      ReloadManager.stopBlocking();
      setTimeout(function () {
        ReloadManager.startBlocking();
      }, 5000);
      if (bankID.thisDevice() && BrowserInfo.isChromeiOS()) {
        // Chrome on iOS is very stupid, and after changing window.location to bankid://
        // it throws Cross-Origin Policy errors for pending AJAX requests
        // so we have to wait for them to finish
        setTimeout(function () {
          window.location = bankID.bankIdUrl();
        }, 2000);
      } else {
        window.location = bankID.bankIdUrl();
      }
    },

    addBankIDIframeIfItsNeeded: function () {
      var bankID = this.state.model;
      if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice()) {
        if (BrowserInfo.isPadDevice()) {
          if (!this.state.hasBeenRedirected) {
           // on android adn ios9 devices iframes dont work at all
           this.redirectToBankIDApp(bankID);
           this.setState({hasBeenRedirected: true});
          }
        }
      }
    },

    render: function () {
      var model = this.state.model;
      var title = localization.docsignview.eleg.bankid.faultModalTitle;
      var ssn = this.props.ssn;
      var hasError = this.state.error;

      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1><span className="bankid-logo"/>{localization.docsignview.eleg.bankid.signConfirmationTitle}</h1>
          <p>
            {model.statusMessage()}
          </p>
          <p className="ssn-text">
            {!hasError && <img src={window.cdnbaseurl + "/img/wait30trans.gif"} />}
            {localization.personalNumber} <b>{ssn}</b>
          </p>
          {/* if */ this.hasStartNowButton() &&
            <Button
              type="action"
              className="button-block"
              text={localization.docsignview.eleg.bankid.startAppNow}
              onClick={this.redirectToBankIDApp}
            />
          }
          {/* if */ hasError &&
            <Button
              type="action"
              className="button-block"
              text={localization.ok}
              onClick={this.props.onBack}
            />
          }
          {/* if */ !hasError &&
            <Button
              className="transparent-button button-block"
              text={localization.cancel}
              onClick={this.props.onBack}
            />
          }
        </div>
      );
    }
  });
