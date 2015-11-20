define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/button",
  "eleg/bankidsigning"],
  function (legacy_code, _, Backbone, React, BackboneMixin, Button, BankIDSigning) {

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      ssn: React.PropTypes.string.isRequired,
      signatory: React.PropTypes.instanceOf(Signatory).isRequired,
      thisDevice: React.PropTypes.bool.isRequired,
      onError: React.PropTypes.func.isRequired,
      onBack: React.PropTypes.func.isRequired,
      onSuccess: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      var self = this;

      model = new BankIDSigning({
        type: "sign",
        signatory: this.props.signatory,
        thisDevice: this.props.thisDevice,
        onStatusChange: function () {
          self.addBankIDIframeIfItsNeeded(model);
        },
        onSuccess: function () {
          self.props.onSuccess();
        },
        onFail: function () {
          self.setState({error: true});
        },
        onCriticalError: function (xhr) {
          ReloadManager.stopBlocking();
          new ReloadDueToErrorModal(xhr);
        }
      });

      return {model: model, error: false, hasBeenRedirected : false};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    componentDidMount: function () {
      this.state.model.initiateTransaction();
    },

    addBankIDIframeIfItsNeeded : function (bankID) {
      if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice()) {
        if (BrowserInfo.isAndroid()) {
          if (!this.state.hasBeenRedirected) {
            // on android devices, dont wait 5s, fall back to the other method immediately
            // since iframes dont work at all, as a bonus, there's no need to click
            // another button

            // but first changing window.location from https:// to bankid:// scheme
            // when window.onbeforeunload is set causes alert on android devices
            // so disable onbeforeunload for some time
            ReloadManager.stopBlocking();
            setTimeout(function() {
              ReloadManager.startBlocking();
            }, 5000);
            window.location = bankID.bankIdUrl();
            this.setState({hasBeenRedirected : true})
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
          {/* if */ hasError &&
            <Button
              type="action"
              className="button-block"
              text={localization.ok}
              onClick={this.props.onError}
            />
          }
          {/* if */ !hasError &&
            <Button
              className="transparent-button button-block"
              text={localization.cancel}
              onClick={this.props.onError}
            />
          }
        </div>
      );
    }
  });
});
