var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var NorwegianIdentifyModel = require("./norwegianidentifymodel");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },
    onError: function (e) {
      var errorType = e.data;

      var errorMsgs = {
        "identify_none": localization.identifyNOBankIdError.failed,
        "identify_authfailed": localization.identifyNOBankIdError.auth,
        "identify_cancel": localization.identifyNOBankIdError.canceled,
        "identify_ua.nobrowser": localization.identifyNOBankIdError.useragent,
        "identify_ua.nocookies": localization.identifyNOBankIdError.useragent,
        "identify_ua.nojava": localization.identifyNOBankIdError.useragent,
        "identify_ua.nojavascript": localization.identifyNOBankIdError.useragent,
        "identify_ua.noos": localization.identifyNOBankIdError.useragent,
        "identify_ua.oldos": localization.identifyNOBankIdError.useragent,
        "identify_ua.oldjava": localization.identifyNOBankIdError.useragent,
        "identify_ua.oldjs": localization.identifyNOBankIdError.useragent,
        "identify_ua.unsupported.version": localization.identifyNOBankIdError.useragent,
        "identify_ua.unsupported.charset": localization.identifyNOBankIdError.useragent,
        "identify_uid.blocked": localization.identifyNOBankIdError.blocked,
        "identify_uid.revoked": localization.identifyNOBankIdError.revoked,
        "identify_uid.expired": localization.identifyNOBankIdError.expired,
        "identify_wrongmobdob": localization.identifyNOBankIdError.mobile
      };

      if (/^identify_/.test(errorType)) {
        new FlashMessage({
          type: "error",
          content: errorMsgs[errorType] || errorMsgs.none
        });
        this.props.model.setIdentify();
      }
    },
    componentDidMount: function () {
      window.addEventListener("message", this.onError);
    },
    render: function () {
      return (
        <span>
          <iframe ref="iframe" style={{minHeight: "280px", width: "100%"}} src={this.props.model.noBankIDLink()}/>
        </span>
      );
    }
  });
