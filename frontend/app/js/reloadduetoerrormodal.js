define(['legacy_code'], function() {

  window.ReloadDueToErrorModal = function(xhr) {
    ReloadManager.stopBlocking();
    mixpanel.track('Error', {
      Message : 'Signing failed: reload modal',
      Status  : xhr.status,
      ResponseText: xhr.responseText
    });
    var text = $("<div>").append($("<span/>").text(localization.signingErrorMessage1))
      .append("<BR/>")
      .append($("<span/>").text(localization.signingErrorMessage2));
    var button = new Button({color: 'action',
      style: "margin:20px",
      text: localization.signingErrorReload,
      onClick: function() {
        new Submit().send(); // Same as window.location.reload(), but will reset scrolling
      }
    }).el();
    return ScreenBlockingDialog.open({header:text, content: button});
  };
});
