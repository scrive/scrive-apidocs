define(["Underscore", "Backbone", "React", "common/backbone_mixin", "common/button",
  "eleg/bankidsigning"],
  function (_, Backbone, React, BackboneMixin, Button, BankIDSigning) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      ssn: React.PropTypes.string.isRequired,
      signatory: React.PropTypes.instanceOf(Signatory).isRequired,
      thisDevice: React.PropTypes.bool.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onSuccess: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      var self = this;

      var model = new BankIDSigning({
        type: "sign",
        signatory: this.props.signatory,
        thisDevice: this.props.thisDevice,
        onStatusChange: function () {
          if (self.isMounted()) {
            self.addBankIDIframeIfItsNeeded();
          }
        },
        onSuccess: function () {
          if (self.isMounted()) {
            self.props.onSuccess();
          }
        },
        onFail: function () {
          if (self.isMounted()) {
            self.setState({error: true});
          }
        },
        onCriticalError: function (xhr) {
          if (self.isMounted()) {
            ReloadManager.stopBlocking();
            new ReloadDueToErrorModal(xhr);
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
      window.location = bankID.bankIdUrl();
    },

    addBankIDIframeIfItsNeeded: function () {
      var bankID = this.state.model;
      if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice()) {
        if (BrowserInfo.isAndroid() || BrowserInfo.isIOS9()) {
          if (!this.state.hasBeenRedirected) {
           // on android adn ios9 devices iframes dont work at all
           this.redirectToBankIDApp(bankID);
           this.setState({hasBeenRedirected: true});
          }
        } else if ($("#bankid-" + bankID.autoStartToken()).size() == 0) {
          $("body").append($("<iframe width='0' height='0' class='bankid-iframe'/>")
            .attr("id", "bankid-" + bankID.autoStartToken()).attr("src", bankID.bankIdUrl()));
        }
      }
    },

    render: function () {
      var model = this.state.model;
      var title = localization.docsignview.eleg.bankid.faultModalTitle;
      var ssn = this.props.ssn;
      var hasError = this.state.error;

      var divClass = React.addons.classSet({
        "col-xs-6": !BrowserInfo.isSmallScreen(),
        "col-xs-12": BrowserInfo.isSmallScreen(),
        "center-block": true
      });

      return (
        <div className={divClass}>
          <h1><img className="inline-img" src="/img/bankid3.png" />{model.statusMessage()}</h1>
          <p className="ssn-text">
            {!hasError && <img src="/img/wait30trans.gif" />}
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
});
