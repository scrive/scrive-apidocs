var Backbone = require("backbone");

var Submit = require("../../../../js/submits.js").Submit;

var CompanyDetailsViewModel = Backbone.Model.extend(
  {
    defaults: {
      companyId: "",
      name: "",
      parentid: null,
      parentgrouppath: [],
      companynumber: "",
      entityname: "",
      address: "",
      zip: "",
      city: "",
      country: "",
      addressIsInherited: false,
      inheritedAddress: null,
      ipaddressmasklist: "",
      cgidisplayname: null,
      cgiserviceid: null,
      idledoctimeoutpreparation: null,
      idledoctimeoutclosed: null,
      idledoctimeoutcanceled: null,
      idledoctimeouttimedout: null,
      idledoctimeoutrejected: null,
      idledoctimeouterror: null,
      immediatetrash: false,
      smsprovider: "",
      padappmode: "",
      padearchiveenabled: true,
      sendtimeoutnotification: true,
      totpismandatory: false,
      sessiontimeout: null,
      portalurl: null,
      eidservicetoken: null,
      settingsIsInherited: false,
      inheritedSettings: null,
      companyinternaltags: [],
      companyexternaltags: []
    },
    saveDetails: function () {
      return new Submit({
        url: "/adminonly/companyadmin/" + this.get("companyId"),
        method: "POST",
        companyname: this.get("name"),
        companyparentid: this.get("parentid"),
        companynumber: this.get("companynumber"),
        entityname: this.get("entityname"),
        companyaddress: this.get("address"),
        companyzip: this.get("zip"),
        companycity: this.get("city"),
        companycountry: this.get("country"),
        companyipaddressmasklist: this.get("ipaddressmasklist"),
        companycgidisplayname: this.get("cgidisplayname"),
        companycgiserviceid: this.get("cgiserviceid"),
        companyidledoctimeoutpreparation: this.get("idledoctimeoutpreparation"),
        companyidledoctimeoutclosed: this.get("idledoctimeoutclosed"),
        companyidledoctimeoutcanceled: this.get("idledoctimeoutcanceled"),
        companyidledoctimeouttimedout: this.get("idledoctimeouttimedout"),
        companyidledoctimeoutrejected: this.get("idledoctimeoutrejected"),
        companyimmediatetrash: this.get("immediatetrash"),
        companyidledoctimeouterror: this.get("idledoctimeouterror"),
        companysmsprovider: this.get("smsprovider"),
        companypadappmode: this.get("padappmode"),
        companypadearchiveenabled: this.get("padearchiveenabled"),
        companysendtimeoutnotification: this.get("sendtimeoutnotification"),
        companytotpismandatory: this.get("totpismandatory"),
        companysessiontimeout: this.get("sessiontimeout"),
        companyportalurl: this.get("portalurl"),
        companyeidservicetoken: this.get("eidservicetoken"),
        companyaddressisinherited: this.get("addressIsInherited"),
        companysettingsisinherited: this.get("settingsIsInherited"),
        companyinternaltags: JSON.stringify(this.get("companyinternaltags")),
        companyexternaltags: JSON.stringify(this.get("companyexternaltags"))
      });
    },
    mergeToCompany: function (newCompanyId) {
      return new Submit({
         url: "/adminonly/companyadmin/merge/" + this.get("companyId"),
         method: "POST",
         companyid: newCompanyId
      });
    }
  },
  {
    IDLE_DOC_TIMEOUT_MIN: 1,
    IDLE_DOC_TIMEOUT_MAX: 365
  }
);

module.exports = CompanyDetailsViewModel;
