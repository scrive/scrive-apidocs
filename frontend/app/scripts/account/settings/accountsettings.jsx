var Backbone = require("backbone");
var Submit = require("../../../js/submits.js").Submit;
var User = require("../../../js/account/user.js").User;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var $ = require("jquery");

/* View model for account settings */

module.exports = Backbone.Model.extend({
  initialize: function () {
    var self = this;
    var user = new User({});
    this.set({"user": user});
    user.bind("change", function () {
      self.reset();
    });
    this.user().set({"ready": false}, {silent: true});
    user.fetch({cache: false, processData: true});
    this.reset();
  },
  user: function () {
     return this.get("user");
  },
  company: function () {
    return this.user().company();
  },
  ready: function () {
     return this.user().ready();
  },
  fstname: function () {
     return this.get("fstname");
  },
  setFstname: function (v) {
     this.set({"fstname": v});
  },
  sndname: function () {
     return this.get("sndname");
  },
  setSndname: function (v) {
     this.set({"sndname": v});
  },
  personnumber: function () {
     return this.get("personnumber");
  },
  setPersonnumber: function (v) {
     this.set({"personnumber": v});
  },
  email: function () {
     return this.get("email");
  },
  setEmail: function (v) {
     this.set({"email": v});
  },
  newemail: function () {
     return this.get("newemail");
  },
  setNewEmail: function (v) {
     this.set({"newemail": v});
  },
  newemailagain: function () {
     return this.get("newemailagain");
  },
  setNewEmailAgain: function (v) {
     this.set({"newemailagain": v});
  },
  phone: function () {
     return this.get("phone");
  },
  setPhone: function (v) {
     this.set({"phone": v});
  },
  companyname: function () {
     return this.get("companyname");
  },
  setCompanyname: function (v) {
     this.set({"companyname": v});
  },
  companynumber: function () {
     return this.get("companynumber");
  },
  setCompanynumber: function (v) {
     this.set({"companynumber": v});
  },
  companyposition: function () {
     return this.get("companyposition");
  },
  setCompanyposition: function (v) {
     this.set({"companyposition": v});
  },
  companyaddress: function () {
     return this.get("companyaddress");
  },
  setCompanyaddress: function (v) {
     this.set({"companyaddress": v});
  },
  companyzip: function () {
     return this.get("companyzip");
  },
  setCompanyzip: function (v) {
     this.set({"companyzip": v});
  },
  companycity: function () {
     return this.get("companycity");
  },
  setCompanycity: function (v) {
     this.set({"companycity": v});
  },
  companycountry: function () {
     return this.get("companycountry");
  },
  setCompanycountry: function (v) {
     this.set({"companycountry": v});
  },
  lang: function () {
    return this.get("lang");
  },
  setLang: function (v) {
    this.set({"lang": v});
  },
  reset: function () {
    if (!this.ready()) {
      return;
    } else {
      this.set({
        fstname: this.user().fstname(),
        sndname: this.user().sndname(),
        personnumber: this.user().personalnumber(),
        email: this.user().email(),
        newemail: "",
        newemailagain: "",
        phone: this.user().phone(),
        lang:  this.user().lang() ||  "en",
        companyname: this.company().companyname(),
        companynumber: this.company().companynumber(),
        companyposition: this.user().companyposition(),
        companyaddress: this.company().address(),
        companyzip: this.company().zip(),
        companycity: this.company().city(),
        companycountry: this.company().country()
      }, {silent: true});
      this.trigger("change");
    }
  },
  changeEmail: function (callback) {
    var self = this;
    return new Submit({
      method: "POST",
      url: "/api/frontend/changeemail",
      ajax: true,
      ajaxsuccess: function (rs) {
        var res = JSON.parse(rs);
        if (res.send) {
          var msg = $("<span>" + localization.account.accountDetails.changeEmailMailSent + "</span>");
          msg.find(".email-confirmation-address").text(self.newemail());
          new FlashMessage({content: msg, type: "success"});
        } else {
          new FlashMessage({content: localization.account.accountDetails.emailAlreadyInUse, type: "error"});
        }
        if (callback != undefined) {
          callback();
        }

      },
      newemail: self.newemail()
    }).send();
  },
  updateProfile: function (callback) {
    return new Submit({
      method: "POST",
      url: "/api/frontend/updateprofile",
      ajax: true,
      ajaxsuccess: function () {
        if (callback != undefined) {
          callback();
        }
      },
      fstname: this.fstname(),
      sndname: this.sndname(),
      personalnumber: this.personnumber(),
      email: this.email(),
      phone: this.phone(),
      lang: this.lang(),
      companyname:  this.companyname(),
      companynumber:  this.companynumber(),
      companyposition: this.companyposition(),
      companyaddress:  this.companyaddress(),
      companyzip:      this.companyzip(),
      companycity:     this.companycity(),
      companycountry:  this.companycountry()
    }).send();
  },
  valid: function () {
    var number = this.companynumber();
    var message = null;
    var validCharsRegex = /^[a-zA-Z0-9-]*$/;
    if (number.length > 50) {
      message = localization.validation.companyNumberTooLong;
    } else if (number.match(validCharsRegex) === null) {
      message = localization.validation.companyNumberInvalidChars;
    }
    if (message !== null) {
      new FlashMessage({content: message, type: "error"});
      return false;
    } else {
      return true;
    }
  },
  save: function () {
    var self  = this;
    if (!self.valid()) {
      return;
    }
    var languageHasChanged = self.user().lang() != self.lang();
    this.updateProfile(function () {
      if (languageHasChanged) {
         // Flash message after language change should be displayed in new language.
         // This is why we are loading localization object in new language before reload.
         $.ajax("/localization/" + window.versioncode + "." + self.lang() + ".js", {
           cache: false,
           success: function (localization_script) {
             eval(localization_script);
             new FlashMessage({
               content: localization.account.accountDetails.detailSaved,
               type: "success",
               withReload: true
             });
           }
        });
      } else {
        new FlashMessage({
          content: localization.account.accountDetails.detailSaved,
          type: "success",
          withReload: false
        });
        self.user().fetch({cache: false, processData: true});
      }
    });
  },
  refresh: function () {
    this.user().set({"ready": false}, {silent: true});
    this.user().fetch({cache: false, processData: true});
    this.reset();
  }
});
