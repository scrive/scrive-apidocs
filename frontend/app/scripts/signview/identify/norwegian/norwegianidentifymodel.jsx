// ignore model in coverage for now.
/* istanbul ignore next */
define(["legacy_code", "Underscore", "Backbone", "base64"],
  function (legacy_code, _, Backbone, _Base64) {
  return Backbone.Model.extend({
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
      _.bindAll(this, "identify", "cancel", "back");
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
    cancel: function () {
      if (this.transaction()) {
        this.transaction().cancel();
      }
      this.setIdentify();
    },
    back: function () {
      this.setIdentify();
    },
    noBankIDLink: function () {
      var location = window.location.origin; // For local testing - replace this with https domain
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
      var target = "(\"" + window.location.origin + "\"," + this.doc().documentid() +
                     "," + this.siglinkid() + ",\"" + window.location  + "\")";
      link = link + "&TARGET=" + encodeURIComponent(Base64.encode(target));

      var start = location + "/nets/start?status=";
      link = link + "&start=" + encodeURIComponent(start);

      var status = location + "/nets/status?status=";
      link = link + "&status=" + encodeURIComponent(status);

      if (this.isMobileMode()) {
        var style = location + "/assets/nets.css";
        link = link + "&style=" + encodeURIComponent(style);
      }

      var locale = "en_GB";
      if (this.doc().lang().simpleCode() == "sv") {
        locale = "sv_SE";
      } else if (this.doc().lang().simpleCode() == "no") {
        locale = "nb_NO";
      }  else if (this.doc().lang().simpleCode() == "da") {
        locale = "da_DK";
      }
      link = link + "&locale=" + locale;

      return link;
    }
  });
});
