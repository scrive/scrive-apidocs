var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var Button = require("../../../common/button");
var SwedishIdentifyModel = require("./swedishidentifymodel");
var BrowserInfo = require("../../../../js/utils/browserinfo.js").BrowserInfo;
var $ = require("jquery");
var ReloadManager = require("../../../../js/reloadmanager.js").ReloadManager;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(SwedishIdentifyModel).isRequired
    },
    getInitialState: function () {
      return {redirected: false};
    },
    componentDidUpdate: function () {
      var transaction = this.props.model.transaction();
      var canStart = !transaction.isFaultStatus() && !transaction.isWaitingForToken() && transaction.thisDevice();
      if (!this.state.redirected && canStart) {
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
          <div className="identify-box-spinner" />
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
