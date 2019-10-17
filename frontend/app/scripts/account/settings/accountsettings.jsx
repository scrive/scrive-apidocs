var Backbone = require("backbone");
var V = require("../../../js/validation.js");
var Submit = require("../../../js/submits.js").Submit;
var User = require("../../../js/account/user.js").User;
var FlashMessages = require("../../../js/flashmessages.js");
var FlashMessage = FlashMessages.FlashMessage;
var FlashMessageAfterReload = FlashMessages.FlashMessageAfterReload;
var $ = require("jquery");

/* View model for account settings */

module.exports = Backbone.Model.extend({
  initialize: function () {
    var self = this;
    var validate = function (validation) {
      return new validation().or(new V.EmptyValidation());
    };
    this._nameValidation = validate(V.NameValidation);
    this._phoneValidation = validate(V.PhoneValidation);
    this._personalNumberValidation = validate(V.PersonalNumberValidation);
    this._positionValidation = validate(V.PositionValidation);
    this._companyNameValidation = validate(V.CompanyNameValidation);
    this._companyNumberValidation = validate(V.CompanyNumberValidation);
    this._companyEntityNameValidation = validate(V.CompanyNameValidation);
    this._companyAddressValidation = validate(V.CompanyAddressValidation);
    this._companyZipValidation = validate(V.CompanyZipValidation);
    this._companyCityValidation = validate(V.CompanyCityValidation);
    this._companyCountryValidation = validate(V.CompanyCountryValidation);
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
  twoFactorActive: function () {
     return this.get("twofactoractive");
  },
  setTwoFactorActive: function (v) {
     this.set({"twofactoractive": v});
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
  fstnameValid: function (v) {
    return this._nameValidation.validateData(this.fstname());
  },
  sndnameValid: function (v) {
    return this._nameValidation.validateData(this.sndname());
  },
  personalnumberValid: function (v) {
    return this._personalNumberValidation.validateData(this.personnumber());
  },
  positionValid: function (v) {
    return this._positionValidation.validateData(this.companyposition());
  },
  companynameValid: function (v) {
    return this._companyNameValidation.validateData(this.companyname());
  },
  companynumberValid: function (v) {
    return this._companyNumberValidation.validateData(this.companynumber());
  },
  companyentitynameValid: function (v) {
    return this._companyEntityNameValidation.validateData(this.companyentityname());
  },
  companyaddressValid: function (v) {
    return this._companyAddressValidation.validateData(this.companyaddress());
  },
  companyzipValid: function (v) {
    return this._companyZipValidation.validateData(this.companyzip());
  },
  companycityValid: function (v) {
    return this._companyCityValidation.validateData(this.companycity());
  },
  companycountryValid: function (v) {
    return this._companyCountryValidation.validateData(this.companycountry());
  },
  phoneValid: function () {
    return this._phoneValidation.validateData(this.phone());
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
  companyentityname: function () {
     return this.get("companyentityname");
  },
  setCompanyentityname: function (v) {
     this.set({"companyentityname": v});
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
        twofactoractive: this.user().twofactoractive(),
        personnumber: this.user().personalnumber(),
        email: this.user().email(),
        newemail: "",
        newemailagain: "",
        phone: this.user().phone(),
        lang: this.user().lang() ||  "en",
        companyname: this.company().companyname(),
        companynumber: this.company().companynumber(),
        companyentityname: this.company().entityname(),
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
        if (rs.send) {
          var msg = $("<span>" + localization.account.accountDetails.changeEmailMailSent + "</span>");
          msg.find(".email-confirmation-address").text(self.newemail());
          new FlashMessage({content: msg.text(), type: "success"});
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
  updateProfile: function (callback, errorCallback) {
    var data = {method: "POST",
                url: "/api/frontend/updateprofile",
                ajax: true,
                ajaxsuccess: callback,
                ajaxerror: errorCallback,
                fstname: this.fstname(),
                sndname: this.sndname(),
                personalnumber: this.personnumber(),
                email: this.email(),
                phone: this.phone(),
                lang: this.lang(),
                companyposition: this.companyposition()};
    if (this.user().companyadmin()) {
      data["companyname"] = this.companyname();
      data["companynumber"] = this.companynumber();
      data["companyentityname"] = this.companyentityname();
      data["companyaddress"] = this.companyaddress();
      data["companyzip"] = this.companyzip();
      data["companycity"] = this.companycity();
      data["companycountry"] = this.companycountry();
    }
    return new Submit(data).send();
  },

  deleteUser: function (email, success, error) {
    return new Submit({
      method: "POST",
      url: "/api/frontend/deleteuser",
      ajax: true,
      ajaxsuccess: success,
      ajaxerror: error,
      email: email
    }).send();
  },

  isUserDeletable: function (callback) {
    return new Submit({
      method: "POST",
      url: "/api/frontend/isuserdeletable",
      ajax: true,
      ajaxsuccess: callback
    }).send();
  },

  validateUserSettings: function () {
    if (!this.fstnameValid()) {
      return this._nameValidation.message();
    } else if (!this.sndnameValid()) {
      return this._nameValidation.message();
    } else if (!this.personalnumberValid()) {
      return this._personalNumberValidation.message();
    } else if (!this.phoneValid()) {
      return this._phoneValidation.message();
    } else if (!this.positionValid()) {
      return this._positionValidation.message();
    }
  },

  validateCompanySettings: function () {
    if (!this.companynameValid()) {
      return this._companyNameValidation.message();
    } else if (!this.companynumberValid()) {
      return this._companyNumberValidation.message();
    } else if (!this.companyentitynameValid()) {
      return this._companyEntityNameValidation.message();
    } else if (!this.companyaddressValid()) {
      return this._companyAddressValidation.message();
    } else if (!this.companyzipValid()) {
      return this._companyZipValidation.message();
    } else if (!this.companycityValid()) {
      return this._companyCityValidation.message();
    } else if (!this.companycountryValid()) {
      return this._companyCountryValidation.message();
    }
  },

  valid: function () {
    var errorMessage = this.validateUserSettings() || this.validateCompanySettings();
    if (errorMessage !== undefined) {
      new FlashMessage({content: errorMessage, type: "error"});
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
           success: function (localizationScript) {
             eval(localizationScript);
             new FlashMessageAfterReload({
               content: localization.account.accountDetails.detailSaved,
               type: "success"
             });
             window.location.reload(true);
           }
        });
      } else {
        new FlashMessage({
          content: localization.account.accountDetails.detailSaved,
          type: "success"
        });
        self.user().fetch({cache: false, processData: true});
      }
    }, function () {
      new FlashMessage({
        content: localization.account.accountDetails.generalError,
        type: "error"
      });
    });
  },
  refresh: function () {
    this.user().set({"ready": false}, {silent: true});
    this.user().fetch({cache: false, processData: true});
    this.reset();
  }
});
