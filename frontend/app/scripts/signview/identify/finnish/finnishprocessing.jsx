var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var FinnishIdentifyModel = require("./finnishidentifymodel");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(FinnishIdentifyModel).isRequired
    },
    onError: function (e) {
      var errorType = e.data;

      var errorMsgs = {
        "identify_none": localization.identifyFITupasError.failed,
        "identify_authfailed": localization.identifyFITupasError.auth,
        "identify_cancel": localization.identifyFITupasError.canceled,
        "identify_ua.nobrowser": localization.identifyFITupasError.useragent,
        "identify_ua.nocookies": localization.identifyFITupasError.useragent,
        "identify_ua.nojava": localization.identifyFITupasError.useragent,
        "identify_ua.nojavascript": localization.identifyFITupasError.useragent,
        "identify_ua.noos": localization.identifyFITupasError.useragent,
        "identify_ua.oldos": localization.identifyFITupasError.useragent,
        "identify_ua.oldjava": localization.identifyFITupasError.useragent,
        "identify_ua.oldjs": localization.identifyFITupasError.useragent,
        "identify_ua.unsupported.version": localization.identifyFITupasError.useragent,
        "identify_ua.unsupported.charset": localization.identifyFITupasError.useragent,
        "identify_uid.blocked": localization.identifyFITupasError.blocked,
        "identify_uid.revoked": localization.identifyFITupasError.revoked,
        "identify_uid.expired": localization.identifyFITupasError.expired
      };

      new FlashMessage({
        type: "error",
        content: errorMsgs[errorType] || errorMsgs["identify_none"]
      });
      window.removeEventListener("message", this.onError);
      this.props.model.setIdentify();
    },
    componentDidMount: function () {
      window.addEventListener("message", this.onError);
    },
    render: function () {
      return (
        <span>
          <iframe
            ref="iframe"
            style={{minHeight: "350px", width: "100%", margin: "auto"}}
            src={this.props.model.fiTupasLink()}
            />
        </span>
      );
    }
  });
