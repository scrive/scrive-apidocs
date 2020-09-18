var IdentifyViewElm = require('./Entry/IdentifyView.elm').Elm.Entry.IdentifyView;

// TODO: Move Cookies_ to a separate file or something.
//       It's only used to bootstrap the Elm app though.
var Cookies_ = {
  get : function (name) {
    var v = document.cookie.match('(^|;) ?' + name + '=([^;]*)(;|$)');
    return v ? v[2] : null;
  },
  delete : function (name) {
    document.cookie = name + "=" + ";path=/;expires=Thu, 01 Jan 1970 00:00:01 GMT"
  }
};

var flashMessageFromCookie = Cookies_.get("flashmessage");
Cookies_.delete("flashmessage");

var app = IdentifyViewElm.init({
    node: document.getElementById('elm-mount')
  , flags: {
      flashMessageFromCookie: flashMessageFromCookie,
      xtoken: Cookies_.get("xtoken"),
      localization: window.localization,
      signatoryLinkId: fromTemplate.sigLinkId,
      documentId: fromTemplate.documentId,
      brandingDomainId: window.brandingdomainid,
      brandingHash: window.brandinghash,
      cdnBaseUrl: window.cdnbaseurl,
      location: window.location.href,
      currentYear: (new Date).getFullYear(),
      browserNeedsSEBankIDRedirect: false,  // todo
      origin: window.location.origin,
      netsConf: {
          netsTrustedDomain: fromTemplate.netsTrustedDomain,
          netsIdentifyUrl: fromTemplate.netsIdentifyUrl,
          netsMerchantIdentifier: fromTemplate.netsMerchantIdentifier
        },
      eidServiceConf :
        { eidUseForFIView: fromTemplate.useEIDHubForFITupasView
        , eidUseForSEView: fromTemplate.useEIDHubForSEBankIDView
        }
      }
});

// Nets communicates status/error messages through "message" events
var messageEventCallback = function (e) { app.ports.messageEventPort.send(e.data); }
window.addEventListener("message", messageEventCallback);

// todo: TrackJS/Mixpanel error tracking
app.ports.errorTraceMsg.subscribe(function(message) {
  console.log(message);
});
