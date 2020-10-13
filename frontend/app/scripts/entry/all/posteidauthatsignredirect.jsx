var React = require("react");
var $ = require("jquery");
var FlashMessageAfterReload = require("../../../js/flashmessages.js").FlashMessageAfterReload;

var React = require("react");
var $ = require("jquery");
var Submit = require("../../../js/submits").Submit;

$(function () {
  var redirectUrl = decodeURIComponent(window.atob(fromTemplate.redirect));

  var checkIfSigned = function () {
    var doRedirect = function () {
      window.top.location = redirectUrl;
    };
    var onError = function () {
      new FlashMessageAfterReload({
        type : "error",
        content: localization.signviewSigningFailed
      });
      doRedirect();
    };
    new Submit({
      url: "/api/frontend/documents/" + fromTemplate.document_id + "/" +
        fromTemplate.signatory_link_id + "/signing/check?_=" + Math.random(),
      method: "GET",
      ajax: true,
      ajaxsuccess : function(data) {
        if (data.signed) {
          // signing succeeded => show document
          doRedirect();
        } else if (data.in_progress) {
          setTimeout(checkIfSigned, 1000);
        } else {
          // signing failed => back to signview
          onError();
        }
      },
      ajaxerror: function() { onError(); }
    }).send();
  };

  var element =
    <div class="forwarded-view">
      <div class="mainContainer signview">
        <div className="main">
          <div className="section loading">
            <div className="col-xs-12 center">
              <div className="waiting4data-signview">
                <img crossOrigin="anonymous" src={window.cdnbaseurl + "/img/wait30trans.gif"} />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>;

  if (fromTemplate.incorrect_data) {
    new FlashMessageAfterReload({type : "error", content: localization.identify.authorizationDataMismatch});
    window.top.location = redirectUrl; // not signed => back to signview
  } else {
    checkIfSigned(redirectUrl);
    React.render(element, document.getElementById('body'));
  }
});
