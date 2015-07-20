define(["legacy_code", "Underscore", "Backbone", "React", "common/button"],
  function (legacy_code, _, Backbone, React, Button) {

  return React.createClass({
    propTypes: {
      transaction: React.PropTypes.object.isRequired,
      statusText: React.PropTypes.string,
      onCancel: React.PropTypes.func.isRequired
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
      var transaction = this.props.transaction;
      var canStart = !transaction.isFaultStatus() && !transaction.isWaitingForToken() && transaction.thisDevice();
      if (!this.state.iframe && canStart && !BrowserInfo.isAndroid()) {
        var iframe = $("<iframe width='0' height='0' class='bankid-iframe'/>")
                       .attr("src", transaction.bankIdUrlWithRedirectIfNeeded());
        this.setState({iframe: iframe});
        $("body").append(iframe);
      } else if (!this.state.redirected && canStart && BrowserInfo.isAndroid()) {
        ReloadManager.stopBlocking();
        setTimeout(function () {
          ReloadManager.startBlocking();
        }, 5000);
        this.setState({redirected: true});
        window.location = transaction.bankIdUrlWithRedirectIfNeeded();
      }
    },
    handleCancel: function () {
      this.props.onCancel();
    },
    render: function () {
      var self = this;
      var transaction = this.props.transaction;
      return (
        <span>
          <h3 className="identify-box-heading">{this.props.statusText}</h3>
          <img className="identify-box-spinner" src="/img/wait30trans.gif" />
          { /* if */ transaction.thisDevice() && transaction.activeForAtLeast5Sec() &&
                     (transaction.isStatusOutstanding() || transaction.isStatusNoClient()) &&
            <div className="identify-box-button">
              <Button
                type="action"
                size="big"
                style={{marginBottom: "38px"}}
                text={localization.docsignview.eleg.bankid.rfa18}
                onClick={function () {
                  window.location = self.props.transaction.bankIdUrlWithRedirectIfNeeded();
                }}
              />
            </div>
          }
          <div onClick={this.handleCancel} className="identify-box-cancel">{localization.cancel}</div>
        </span>
      );
    }
  });
});
