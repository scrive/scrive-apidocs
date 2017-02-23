var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var DanishIdentifyModel = require("./danishidentifymodel");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;

  module.exports = React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(DanishIdentifyModel).isRequired
    },
    onError: function (e) {
      var errorType = e.data;

      var errorMsgs = {
        "identify_none": localization.identifyDKNemIdError.failed,
        "identify_authfailed": localization.identifyDKNemIdError.auth,
        "identify_cancel": localization.identifyDKNemIdError.canceled,
        "identify_ua.nobrowser": localization.identifyDKNemIdError.useragent,
        "identify_ua.nocookies": localization.identifyDKNemIdError.useragent,
        "identify_ua.nojava": localization.identifyDKNemIdError.useragent,
        "identify_ua.nojavascript": localization.identifyDKNemIdError.useragent,
        "identify_ua.noos": localization.identifyDKNemIdError.useragent,
        "identify_ua.oldos": localization.identifyDKNemIdError.useragent,
        "identify_ua.oldjava": localization.identifyDKNemIdError.useragent,
        "identify_ua.oldjs": localization.identifyDKNemIdError.useragent,
        "identify_ua.unsupported.version": localization.identifyDKNemIdError.useragent,
        "identify_ua.unsupported.charset": localization.identifyDKNemIdError.useragent,
        "identify_uid.blocked": localization.identifyDKNemIdError.blocked,
        "identify_uid.revoked": localization.identifyDKNemIdError.revoked,
        "identify_uid.expired": localization.identifyDKNemIdError.expired
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
          <iframe
            ref="iframe"
            style={{minHeight: "350px", width: "100%", margin: "auto"}}
            src={this.props.model.dkNemIDLink()}
            />
        </span>
      );
    }
  });
