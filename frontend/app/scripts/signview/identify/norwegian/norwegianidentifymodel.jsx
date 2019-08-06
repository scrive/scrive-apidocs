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
      type: "desktop",
      step: "identify",
      mobile: "",
      canEditMobile: false
    },
    initialize: function (args) {
      this.set({
        mobile: this.mobileForNOBankIDFormat(this.doc().currentSignatory().mobile()),
        canEditMobile: this.doc().currentSignatory().mobile() == ""
      });
      _.bindAll(this, "identify");
    },
    mobileForNOBankIDFormat: function (mobile) {
      // We need to drop prefix. It's a standard for users of NO BankID not to have it.
      return mobile.replace("+47", "");
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
      return this.doc().currentSignatory().personalnumber().replace(/-/g, "");
    },
    dateOfBirth: function () {
      return this.personalnumberFormatted().slice(0, 6);
    },
    mobile: function () {
      return this.get("mobile");
    },
    mobileFormatted: function () {
      return this.get("mobile").replace(/[ -]/g, "");
    },
    setMobile: function (v) {
      if (this.canEditMobile()) {
        this.set({mobile: v});
      }
    },
    canEditMobile: function () {
      return this.get("canEditMobile");
    },
    isDesktopMode: function () {
      return this.get("type") == "desktop";
    },
    isMobileMode: function () {
      return this.get("type") == "mobile";
    },
    setDesktopMode: function () {
      this.set({type: "desktop"});
    },
    setMobileMode: function () {
      this.set({type: "mobile"});
    },
    isSwedish: function () {
      return false;
    },
    isNorwegian: function () {
      return true;
    },
    isDanish: function () {
      return false;
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
    noBankIDLink: function () {
      var netsTrustedDomain = window.netsTrustedDomain;
      var netsIdentifyUrl = window.netsIdentifyUrl;
      var netsMerchantIdentifier = window.netsMerchantIdentifier;
      var vendor = this.isDesktopMode() ? "no_bankid" : "no_bidmob";
      link = netsIdentifyUrl + "?mid=" + netsMerchantIdentifier + "&wi=r";
      link = link + "&forcepkivendor=" + vendor;
      if (this.isDesktopMode()) {
        link = link + "&presetid=" + encodeURIComponent(Base64.encode(this.personalnumberFormatted()));
      } else if (this.isMobileMode()) {
        link = link + "&celnr8=" + encodeURIComponent(Base64.encode(this.mobileFormatted()));
        link = link + "&dob6=" + encodeURIComponent(Base64.encode(this.dateOfBirth()));
      }
      var target = "(\"" + LocationUtils.origin() + "\"," + this.doc().documentid() +
                     "," + this.siglinkid() + ",\"" + window.location  + "\")";
      link = link + "&TARGET=" + encodeURIComponent(Base64.encode(target));

      var start = netsTrustedDomain + "/nets/start?status=";
      link = link + "&start=" + encodeURIComponent(start);

      var status = netsTrustedDomain + "/nets/status?status=";
      link = link + "&status=" + encodeURIComponent(status);

      if (this.isMobileMode()) {
        var style = netsTrustedDomain + "/css/assets/nets.css";
        link = link + "&style=" + encodeURIComponent(style);
      }

      var locale = "en_GB";
      if (this.doc().lang() == "sv") {
        locale = "sv_SE";
      } else if (this.doc().lang() == "no") {
        locale = "nb_NO";
      }  else if (this.doc().lang() == "da") {
        locale = "da_DK";
      }
      link = link + "&locale=" + locale;

      return link;
    }
  });
