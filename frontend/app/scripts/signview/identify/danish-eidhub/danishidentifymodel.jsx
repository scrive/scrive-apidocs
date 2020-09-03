var _ = require("underscore");
var Backbone = require("backbone");
var Submit = require("../../../../js/submits.js").Submit;

var link = link;

// ignore model in coverage for now.
/* istanbul ignore next */
  module.exports = Backbone.Model.extend({
    defaults: {
      doc: undefined,
      siglinkid: 0,
      step: "identify",
      transactionAccessUrl: undefined
    },
    initialize: function (args) {
      _.bindAll(this, "identify");
    },
    doc: function () {
      return this.get("doc");
    },
    siglinkid: function () {
      return this.get("siglinkid");
    },
    isSwedish: function () {
      return false;
    },
    isNorwegian: function () {
      return false;
    },
    isCPR: function() {
      var sig = this.doc().currentSignatory();
      return sig.legacyDkNemIDAuthenticationToView()
             || sig.dkNemIDCPRAuthenticationToView()
             || sig.legacyDkNemIDAuthenticationToViewArchived()
             || sig.dkNemIDCPRAuthenticationToViewArchived()
    },
    isPID: function() {
      var sig = this.doc().currentSignatory();
      return sig.dkNemIDPIDAuthenticationToView() || sig.dkNemIDPIDAuthenticationToViewArchived()
    },
    isCVR: function() {
      var sig = this.doc().currentSignatory();
      return sig.dkNemIDCVRAuthenticationToView() || sig.dkNemIDCVRAuthenticationToViewArchived();
    },
    isDanishPersonal: function () {
      return this.isCPR() || this.isPID();
    },
    isDanishEmployee: function () {
      return this.isCVR();
    },
    isFinnish: function () {
      return false;
    },
    isSMSPin: function () {
      return false;
    },
    isVerimi: function () {
      return false;
    },
    isIDIN: function () {
      return false;
    },
    isIdentify: function () {
      return this.get("step") === "identify";
    },
    isProcessing: function () {
      return this.get("step") === "processing";
    },
    isLoading: function () {
      return this.get("step") === "loading";
    },
    setIdentify: function () {
      this.set({step: "identify"});
    },
    setProcessing: function () {
      this.set({step: "processing"});
    },
    setLoading: function () {
      this.set({step: "loading"});
    },
    identify: function () {
      this.setLoading();
      this.startTransaction();
    },
    transactionAccessUrl: function () {
      return this.get("transactionAccessUrl");
    },
    nemIDMethod: function () {
      if (this.isCPR()) {
        return "dk_nemid_cpr";
      }
      if (this.isPID()) {
        return "dk_nemid_pid";
      }
      if (this.isCVR()) {
        return "dk_nemid_cvr";
      }
      return "dk_nemid_pid";
    },
    startTransaction: function () {
      var self = this;
      new Submit({
        url: "/eid-service/start/nemid/view/" + this.doc().documentid() + "/" + this.siglinkid(),
        method: "POST",
        redirect: window.btoa(encodeURIComponent(window.location)),
        nemid_method: this.nemIDMethod(),
        ajax: true,
        ajaxsuccess: function (resp) {
          self.set({"transactionAccessUrl": resp.accessUrl}, {silent: true});
          self.setProcessing();
        }
      }).sendAjax();

    },
    nemIDLink: function () {
      return this.transactionAccessUrl();
    }
  });
