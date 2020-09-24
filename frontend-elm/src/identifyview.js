var IdentifyViewElm = require('./Entry/IdentifyView.elm').Elm.Entry.IdentifyView;

// These cookie functions are only used to init the Elm app.
function getCookie (name) {
  var v = document.cookie.match('(^|;) ?' + name + '=([^;]*)(;|$)');
  return v ? v[2] : null;
}
function deleteCookie (name) {
  document.cookie = name + "=" + ";path=/;expires=Thu, 01 Jan 1970 00:00:01 GMT";
}

var flashMessageFromCookie = getCookie("flashmessage");
deleteCookie("flashmessage");

// TODO: Check if Object.assign is supported in all browsers we care about.
var flags = Object.assign(
  {},
  { flashMessageFromCookie: flashMessageFromCookie,
    xtoken: getCookie("xtoken"),
    localization: window.localization,
    location: window.location.href,
    currentYear: (new Date).getFullYear(),
    origin: window.location.origin
  },
  appConfigFromTemplate
);

var app = IdentifyViewElm.init({
  node: document.getElementById('elm-mount'),
  flags: flags
});

// todo: TrackJS/Mixpanel error tracking
app.ports.errorTraceMsg.subscribe(function(message) {
  console.log(message);
});
