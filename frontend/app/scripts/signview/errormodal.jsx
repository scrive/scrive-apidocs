define(["legacy_code", "React", "common/button"], function(_legacy, React, Button) {

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

  return function(xhr) {
    ReloadManager.stopBlocking();
    mixpanel.track('Error', {
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
});
