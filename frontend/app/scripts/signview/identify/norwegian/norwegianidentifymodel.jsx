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
    mobileForNOBankIDFormat: function(mobile) {
      return mobile.replace("+47",""); // We need to drop prefix. It's a standard for users of NO BankID not to have it.
    },
    doc: function () {
      return this.get("doc");
    },
    siglinkid: function () {
      return this.get("siglinkid");
    },
    personalnumber: function() {
      return this.doc().currentSignatory().personalnumber();
    },
    dateOfBirth: function() {
      return this.personalnumber().slice(0, 6);
    },
    mobile: function(){
      return this.get("mobile");
    },
    setMobile: function (v) {
      if (this.canEditMobile()) {
        this.set({mobile: v});
      }
    },
    canEditMobile: function() {
      return this.get("canEditMobile");
    },
    isDesktopMode : function() {
      return this.get("type") == "desktop";
    },
    isMobileMode : function() {
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
    noBankIDLink : function() {
      var location = "https://scrive.com"; // window.location.origin
      var netsIdentifyUrl = window.netsIdentifyUrl;
      var netsMerchantIdentifier = window.netsMerchantIdentifier;
      var target = "KDMsNiwiaHR0cDovL2xvY2FsaG9zdDo4MDAwL3MvMy82Iik%3D";
      var vendor = this.isDesktopMode() ? "no_bankid" : "no_bidmob";
      link = netsIdentifyUrl + "?mid=" + netsMerchantIdentifier + "&wi=r";
      var vendor = this.isDesktopMode() ? "no_bankid" : "no_bidmob";
      link = link + "&forcepkivendor="+vendor;
      if (this.isDesktopMode()) {
        link = link + "&presetid=" + encodeURIComponent(Base64.encode(this.personalnumber()));
      } else if (this.isMobileMode()) {
        link = link + "&celnr8=" + encodeURIComponent(Base64.encode(this.mobile()));
        link = link + "&dob6=" + encodeURIComponent(Base64.encode(this.dateOfBirth()));
      }
      var target = "(" + this.doc().documentid() + "," + this.siglinkid() + ",\"" + window.location  + "\")";
      link = link + "&TARGET=" + encodeURIComponent(Base64.encode(target));

      var start = location + "/nets/start?status=";
      link = link + "&start=" + encodeURIComponent(start);

      var status = location + "/nets/status?status=";
      link = link + "&status=" + encodeURIComponent(status);

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
