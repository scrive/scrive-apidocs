var _ = require("underscore");
var Backbone = require("backbone");
var Base64 = require("base64");
var LocationUtils = require("../../../common/location");

var link = link;

// ignore model in coverage for now.
/* istanbul ignore next */
  module.exports = Backbone.Model.extend({
    defaults: {
      doc: undefined,
      siglinkid: 0,
      step: "processing"
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
    personalnumber: function () {
      return this.doc().currentSignatory().personalnumber();
    },
    personalnumberFormatted: function () {
      // in Finnish SSN the '-' must not be removed, it carries century information.
      return this.doc().currentSignatory().personalnumber().toUpperCase();
    },
    isSwedish: function () {
      return false;
    },
    isNorwegian: function () {
      return false;
    },
    isDanish: function () {
      return false;
    },
    isFinnish: function () {
      return true;
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
    setIdentify: function () {
      this.set({step: "identify"});
    },
    setProcessing: function () {
      this.set({step: "processing"});
    },
    identify: function () {
      this.setProcessing();
    },
    fiTupasLink: function () {
      var netsTrustedDomain = window.netsTrustedDomain;
      var netsIdentifyUrl = window.netsIdentifyUrl;
      var netsMerchantIdentifier = window.netsMerchantIdentifier;
      var vendor = "fi_tupas";
      link = netsIdentifyUrl + "?mid=" + netsMerchantIdentifier + "&wi=r";
      link = link + "&forcepkivendor=" + vendor;
      link = link + "&style=" + window.netsTrustedDomain + "/css/assets/nets_fi.css";
      link = link + "&presetid=" + encodeURIComponent(Base64.encode(this.personalnumberFormatted()));
      var target = "(\"" + LocationUtils.origin() + "\"," + this.doc().documentid() +
                     "," + this.siglinkid() + ",\"" + window.location  + "\")";
      link = link + "&TARGET=" + encodeURIComponent(Base64.encode(target));

      var start = netsTrustedDomain + "/nets/start?status=";
      link = link + "&start=" + encodeURIComponent(start);

      var status = netsTrustedDomain + "/nets/status?status=";
      link = link + "&status=" + encodeURIComponent(status);

      var locale = "en_GB";
      if (this.doc().lang() == "sv") {
        locale = "sv_SE";
      } else if (this.doc().lang() == "no") {
        locale = "nb_NO";
      }  else if (this.doc().lang() == "da") {
        locale = "da_DK";
      }  else if (this.doc().lang() == "fi") {
        locale = "fi_FI";
      }
      link = link + "&locale=" + locale;

      return link;
    }
  });
