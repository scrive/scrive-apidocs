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
    siglinkid: 0,
    dkNemIDCVRMethod: undefined,
    errorHandler: undefined
  },
  initialize: function (args) {
    _.bindAll(this, "startSignTransaction");
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
  startSignTransaction: function (onSuccess) {
    var self = this;
    var doc = this.doc();

    var errorCallback = function () {
      new FlashMessage({
        type: "error",
        content: localization.failedToStartNewEIDProcess,
        className: "flash-signview",
        withReload: true
      });
      self.errorHandlerIfThereIsOne();
    };

    var nemIDMethod;
    if (doc.currentSignatory().dkNemIDCVRAuthenticationToSign()) {
      nemIDMethod = this.get("dkNemIDCVRMethod");
    } else if (doc.currentSignatory().dkNemIDCPRAuthenticationToSign()) {
      nemIDMethod = "dk_nemid_cpr";
    } else {
      nemIDMethod = "dk_nemid_pid";
    }

    var personalNumber = doc.currentSignatory().personalnumber();
    var successCallback = function (resp) {
      self.doc().checksign(function () {
        new FlashMessagesCleaner();
        var timeout = window.SIGN_TIMEOUT || 0;
        setTimeout(function () {
          self.doc().sign(errorCallback, function () {
            ReloadManager.stopBlocking();
            onSuccess(resp.accessUrl);
          }, {}).send();
        }, timeout);
      }, errorCallback, {}).send();
    };

    new Submit({
      url: "/eid-service/start/nemid/sign/" + doc.documentid() + "/" + this.siglinkid(),
      method: "POST",
      nemid_method: nemIDMethod,
      personal_number: personalNumber,
      ajax: true,
      ajaxsuccess: successCallback,
      ajaxerror: errorCallback
    }).sendAjax();
  }
});
