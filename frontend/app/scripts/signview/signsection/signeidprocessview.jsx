define(["legacy_code", "Underscore", "Backbone", "React", "common/backbone_mixin", "common/button",
  "eleg/bankidsigning"],
  function (legacy_code, _, Backbone, React, BackboneMixin, Button, BankIDSigning) {

  var addBankIDIframeIfItsNeeded = function (bankID) {
    if (!bankID.isFaultStatus() && !bankID.isWaitingForToken() && bankID.thisDevice() &&
        ($("#bankid-" + bankID.autoStartToken()).size() == 0) && !BrowserInfo.isAndroid()) {
      $("body").append($("<iframe width='0' height='0' class='bankid-iframe'/>")
        .attr("id", "bankid-" + bankID.autoStartToken()).attr("src", bankID.bankIdUrl()));
    }
  };

  return React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      ssn: React.PropTypes.string.isRequired,
      signatory: React.PropTypes.instanceOf(Signatory).isRequired,
      thisDevice: React.PropTypes.bool.isRequired,
      onError: React.PropTypes.func.isRequired,
      onSuccess: React.PropTypes.func.isRequired
    },

    getInitialState: function () {
      var self = this;

      model = new BankIDSigning({
        type: "sign",
        signatory: this.props.signatory,
        thisDevice: this.props.thisDevice,
        onStatusChange: function () {
          addBankIDIframeIfItsNeeded(model);
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

      return {model: model, error: false};
    },

    getBackboneModels: function () {
      return [this.state.model];
    },

    componentDidMount: function () {
      this.state.model.initiateTransaction();
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
        </div>
      );
    }
  });
});
