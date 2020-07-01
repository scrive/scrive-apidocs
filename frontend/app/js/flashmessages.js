var Backbone = require("backbone");
var _ = require("underscore");
var $ = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var Submit = require("./submits.js").Submit;
var Track = require("../scripts/common/track");
var React = require("react");

var ReactFlashMessageView = require("../scripts/common/flashmessages");

/* Main and only flash messages module
 * Usage
 *   new FlashMessage({ type : "success" | "error"
 *                    , content: "Text to be displayed"
 *                    })
 *
 * If you need to redirect user to different page, and then he should see a flash message,
 * use "new FlashMessageAfterReload" (with same parameter) and then reload the page. The flash message
 * will be saved in a cookie and displayed after page is reloaded. You still have to reload
 * the page yourself.
 *
 * If you need to clean all flash message do
 *   new FlashMessagesCleaner();
 */

var FlashMessage = exports.FlashMessage = function (args) {
  var globalContainer = document.querySelector("div.flash");
  if (globalContainer == null) {
    globalContainer = document.createElement("div");
    globalContainer.classList.add("flash");
    document.body.appendChild(globalContainer);
  }

  var container = document.createElement("div");
  globalContainer.appendChild(container);

  var onHide = function () {
    try {
      React.unmountComponentAtNode(container);
      document.body.removeChild(container);
      container = null;
    } catch (error) {
      // pass
    }
  };

  var view = React.render(
    React.createElement(
      ReactFlashMessageView,
      {
        type: args.type,
        content: args.content,
        hideTimeout: args.hideTimeout,
        onHide: onHide
      }
    ),
    container
  );
};

exports.FlashMessageAfterReload = function (args) {
  Cookies.set(
    "flashmessage",
    toBase64(JSON.stringify({"type": args.type, "content": args.content}))
  );
};

exports.FlashMessagesCleaner = function () {
  $(".flash").css("display", "none");
};

var toBase64 = function (str) {
  return window.btoa(encodeURIComponent(str));
};

var fromBase64 = function (str) {
  return decodeURIComponent(window.atob(str));
};

var tryUnescapeQuotes = function (s) {
  if (s.charAt(0) === "\"" && s.charAt(s.length - 1) === "\"") {
    return s.substr(1, s.length - 2).replace(/\\"/g, '"');
  } else {
    return s;
  }
};

exports.FlashMessageTryFromCookie = function () {
  if (Cookies.get("flashmessage")) {
    try {
      var cookie = Cookies.get("flashmessage");
      var unescapedCookie = tryUnescapeQuotes(cookie);  // backend wraps string in quotes
      var decodedCookie = fromBase64(decodeURIComponent(unescapedCookie));
      var cleanDecodedCookie = decodedCookie.replace(/(\r\n|\n|\r)/gm, "");  // get rid of newlines
      var jsonFlash = JSON.parse(cleanDecodedCookie);
      jsonFlash["content"] = decodeURIComponent(jsonFlash["content"]);
      new FlashMessage(jsonFlash);
    } catch (err) {
      Track.track("Failed to parse cookie: " + Cookies.get("flashmessage"));
      // swallow the error, it's better to lose a flashmessage,
      // then keep throwing errors everywhere, until user clears cookies
    } finally {
      Cookies.delete("flashmessage");
    }
  }
};
