var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var NetsSigning = require("../../eleg/netssigning");
var ViewSize = require("../viewsize");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Button = require("../../common/button");
var $ = require("jquery");
var classNames = require("classnames");

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      onCancel: React.PropTypes.func.isRequired,
      iframeUrl: React.PropTypes.string.isRequired
    },

    onCancel: function () {
      new FlashMessage({
        type: "success",
        content: localization.signDKNemIDCanceled
      });
      this.props.onCancel();
    },

    render: function () {
      var self = this;
      var divClass = classNames({
        "col-xs-6": !ViewSize.isSmall(),
        "col-xs-12": ViewSize.isSmall(),
        "center-block": true
      });
      var document = this.props.model.document();
      var iframeUrl = this.props.iframeUrl;
      var confirmationTitle = localization.docsignview.eleg.bankid.signDKConfirmationTitle;
      var transctionNumberText = localization.docsignview.eleg.bankid.transactionNumber;
      var transactionNumberFirst = String(document.id).slice(0, -4);
      var transactionNumberLast = String(document.id).slice(-4);
      var logoClass = classNames({"bankid-logo-nets": true, "dk-bankid-logo": true});
      return (
        <div className={divClass}>
          <h1 className="nets-sign-process">
            <span className={logoClass}/>
            {confirmationTitle}
          </h1>
          <p className="eid-process-view-doc-id">
            {transctionNumberText} {transactionNumberFirst}<em>{transactionNumberLast}</em>
          </p>
            <div>
              <iframe
                ref="iframe"
                style={{minHeight: "430px", width: "100%", margin: "20px auto auto auto", overflow: "auto"}}
                src={iframeUrl}
                />
              <div className="button-thin">
                <Button
                  text={localization.cancel}
                  className="transparent-button button-block"
                  onClick={self.onCancel}
                />
              </div>
            </div>
        </div>
      );
    }
  });
