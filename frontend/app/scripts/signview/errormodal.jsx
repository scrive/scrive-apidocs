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
          <br/>
          <br/>
          {localization.signingErrorMessage3}
          <br/>
          <br/>
        </div>
      );
    }
  });

  var ErrorMessageComponent = React.createClass({
    propTypes: {
      errorMessage: React.PropTypes.string.isRequired
    },
    render: function () {
      return (
        <div className="errormessage">
          {localization.docsignview.errorMessagePrefix} &quot;{this.props.errorMessage}&quot;
        </div>
      );
    }
  });

  var NetworkErrorMessageComponent = React.createClass({
    render: function () {
      return (
        <div>{localization.docsignview.networkIssueErrorMessage}</div>
      );
    }
  });

  var ErrorDetailsComponent = React.createClass({
    propTypes: {
      details: React.PropTypes.object.isRequired,
      xhr: React.PropTypes.object.isRequired
    },
    render: function () {
      var details = this.props.details || {};
      var xhr = this.props.xhr;
      var requestid = xhr.getResponseHeader("X-Request-ID");
      if (requestid) {
        details["Request ID"] = requestid;
      }
      details["Status"] = xhr.status;
      details["Response"] = xhr.responseText;
      return (
        <div className="errordetails">
          <ul>
            {Object.keys(details).map(function (key) {
              return (<li>{key}: {details[key]}</li>);
            })}
          </ul>
        </div>
      );
    }
  });

  var buttonParams = {
    color: "action",
    style: {margin: "20px"},
    text: localization.signingErrorReload,
    onClick: function() {
      window.location.reload();
    }
  };

  module.exports = function(xhr, details) {
    ReloadManager.stopBlocking();
    Track.track('Error', {
      Message : 'Signing failed: reload modal',
      Status  : xhr.status,
      ResponseText: xhr.responseText
    });

    var textDiv = $("<div/>");
    var contentDiv = $("<div/>").attr("class", "signviewerrormodal");
    React.render(React.createElement(TextComponent, {}), textDiv[0]);

    if (xhr.status === 409 && xhr.responseJSON.error_type == "document_state_error") {
      errorMessage = localization.docsignview.unavailableForSign;

      var errorMessageDiv = $("<div/>");
      React.render(
        React.createElement(ErrorMessageComponent, {errorMessage: errorMessage}),
        errorMessageDiv[0]
      );

      errorMessageDiv.appendTo(contentDiv);
    } else if (typeof xhr.status !== "number" || xhr.status === 0) {
      React.render(
        React.createElement(NetworkErrorMessageComponent, {}),
        textDiv[0]
      );
    }

    var detailsDiv = $("<div/>");
    React.render(React.createElement(ErrorDetailsComponent,
                                     {details: details, xhr: xhr}),
                 detailsDiv[0]);
    detailsDiv.appendTo(contentDiv);

    var buttonDiv = $("<div/>");
    React.render(React.createElement(Button,buttonParams), buttonDiv[0]);
    buttonDiv.appendTo(contentDiv);

    return ScreenBlockingDialog.open({header:textDiv, content: contentDiv});
  };
