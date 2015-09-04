define(["legacy_code", "Underscore", "Backbone", "React",
  "signview/identify/norwegian/norwegianidentifymodel"],
  function (legacy_code, _, Backbone, React, NorwegianIdentifyModel) {

  return React.createClass({
    propTypes: {
      model: React.PropTypes.instanceOf(NorwegianIdentifyModel).isRequired
    },
    onError: function (e) {
      var errorType = e.data;
      var originOk = e.origin === window.location.origin;

      var errorMsgs = {
        "identify_none": localization.identifyBankIdError.failed,
        "identify_authfailed": localization.identifyBankIdError.auth,
        "identify_cancel": localization.identifyBankIdError.canceled,
        "identify_ua.nobrowser": localization.identifyBankIdError.useragent,
        "identify_ua.nocookies": localization.identifyBankIdError.useragent,
        "identify_ua.nojava": localization.identifyBankIdError.useragent,
        "identify_ua.nojavascript": localization.identifyBankIdError.useragent,
        "identify_ua.noos": localization.identifyBankIdError.useragent,
        "identify_ua.oldos": localization.identifyBankIdError.useragent,
        "identify_ua.oldjava": localization.identifyBankIdError.useragent,
        "identify_ua.oldjs": localization.identifyBankIdError.useragent,
        "identify_ua.unsupported.version": localization.identifyBankIdError.useragent,
        "identify_ua.unsupported.charset": localization.identifyBankIdError.useragent,
        "identify_uid.blocked": localization.identifyBankIdError.blocked,
        "identify_uid.revoked": localization.identifyBankIdError.revoked,
        "identify_uid.expired": localization.identifyBankIdError.expired,
        "identify_wrongmobdob": localization.identifyBankIdError.mobile
      };

      if (originOk && /^identify_/.test(errorType)) {
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
});
