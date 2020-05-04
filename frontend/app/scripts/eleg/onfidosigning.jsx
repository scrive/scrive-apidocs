var _ = require("underscore");
var Backbone = require("backbone");
var Submit = require("../../js/submits.js").Submit;
var ReloadManager = require("../../js/reloadmanager.js").ReloadManager;
var FlashMessagesCleaner = require("../../js/flashmessages.js").FlashMessagesCleaner;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;

var link = link;

module.exports = Backbone.Model.extend({
  defaults: {
    doc: undefined,
    siglinkid: 0
  },
  initialize: function (args) {
    _.bindAll(this, "sign");
  },
  doc: function () {
    return this.get("doc");
  },
  siglinkid: function () {
    return this.get("siglinkid");
  },
  errorHandlerIfThereIsOne: function () {
    var errorHandler = this.get("errorHandler");
    if (errorHandler) {
      errorHandler();
    }
  },
  sign: function () {
    var self = this;

    var errorCallback = function () {
      new FlashMessage({
        type: "error",
        content: localization.failedToStartNewEIDProcess,
        className: "flash-signview",
        withReload: true
      });
      self.errorHandlerIfThereIsOne();
    };

    // todo: call returned url and _then_ poll (or do redirect in backend?)
    var successCallback = function (resp) {
      self.doc().checksign(function () {
        new FlashMessagesCleaner();
        var timeout = window.SIGN_TIMEOUT || 0;
        setTimeout(function () {
          self.doc().sign(errorCallback, function () {
            ReloadManager.stopBlocking();
            location.href = resp.accessUrl;
          }, {}).send();
        }, timeout);
      }, errorCallback, {}).send();
    };

    new Submit({
      url: "/eid-service/start/onfido/sign/" + this.doc().documentid() + "/" + this.siglinkid(),
      method: "POST",
      ajax: true,
      ajaxsuccess: successCallback,
      ajaxerror: errorCallback
    }).sendAjax();
  }
});
