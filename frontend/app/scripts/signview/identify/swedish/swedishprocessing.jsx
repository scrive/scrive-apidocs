define(["legacy_code", "Underscore", "Backbone", "React", "common/button",
  "signview/identify/swedish/swedishidentifymodel"],
  function (legacy_code, _, Backbone, React, Button, SwedishIdentifyModel) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SwedishIdentifyModel).isRequired
    },
    getInitialState: function () {
      return {iframe: undefined, redirected: false};
    },
    componentWillUnmount: function () {
      if (this.state.iframe != undefined) {
        this.state.iframe.remove();
      }
    },
    componentDidUpdate: function () {
      var transaction = this.props.model.transaction();
      var canStart = !transaction.isFaultStatus() && !transaction.isWaitingForToken() && transaction.thisDevice();
      var iframeUncapableBrowser = BrowserInfo.isAndroid() || BrowserInfo.isIOS9();
      if (!this.state.iframe && canStart && !iframeUncapableBrowser) {
        var iframe = $("<iframe width='0' height='0' class='bankid-iframe'/>")
                       .attr("src", transaction.bankIdUrlWithRedirectIfNeeded());
        this.setState({iframe: iframe});
        $("body").append(iframe);
      } else if (!this.state.redirected && canStart && iframeUncapableBrowser) {
        ReloadManager.stopBlocking();
        setTimeout(function () {
          ReloadManager.startBlocking();
        }, 5000);
        this.setState({redirected: true});
        window.location = transaction.bankIdUrlWithRedirectIfNeeded();
      }
    },
    handleCancel: function () {
      this.props.model.cancel();
    },
    render: function () {
      var self = this;
      var transaction = this.props.model.transaction();
      return (
        <span>
          <h3 className="identify-box-heading">{this.props.model.statusText()}</h3>
          <img className="identify-box-spinner" src={window.cdnbaseurl + "/img/wait30trans.gif"} />
          { /* if */ transaction.thisDevice() && transaction.activeForAtLeast5Sec() &&
                     (transaction.isStatusOutstanding() || transaction.isStatusNoClient()) &&
            <div className="identify-box-button">
              <Button
                type="action"
                size="big"
                style={{marginBottom: "38px"}}
                text={localization.docsignview.eleg.bankid.rfa18}
                onClick={function () {
                  window.location = self.props.model.transaction().bankIdUrlWithRedirectIfNeeded();
                }}
              />
            </div>
          }
          <div onClick={this.handleCancel} className="identify-box-cancel" ref="identify-box-cancel">
            {localization.cancel}
          </div>
        </span>
      );
    }
  });
});
