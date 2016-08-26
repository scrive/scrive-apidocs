var React = require("react");
var Button = require("../common/button");
var Track = require("../common/track");
var Submit = require("../../js/submits.js").Submit;
var ReloadManager = require("../../js/reloadmanager.js").ReloadManager;
var $ = require("jquery");
var ScreenBlockingDialog = require("../../js/dialog.js").ScreenBlockingDialog;

  var TextComponent = React.createClass({
    render: function () {
     return (
        <div>
          {localization.signingErrorMessage1}
          <br/>
          {localization.signingErrorMessage2}
        </div>
      );
    }
  });

  var buttonParams = {
    color: "action",
    style: {margin: "20px"},
    text: localization.signingErrorReload,
    onClick: function() {
      new Submit().send(); // Same as window.location.reload(), but will reset scrolling
    }
  };

  module.exports = function(xhr) {
    ReloadManager.stopBlocking();
    Track.track('Error', {
      Message : 'Signing failed: reload modal',
      Status  : xhr.status,
      ResponseText: xhr.responseText
    });

    var textDiv = $("<div/>");
    var buttonDiv = $("<div/>");
    React.render(React.createElement(TextComponent,{ }),textDiv[0]);
    React.render(React.createElement(Button,buttonParams),buttonDiv[0]);
    return ScreenBlockingDialog.open({header:textDiv, content: buttonDiv});
  };
