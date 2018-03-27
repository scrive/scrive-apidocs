var Backbone = require("backbone");

var Submit = require("../../../../js/submits.js").Submit;

var CompanyDetailsViewModel = Backbone.Model.extend(
  {
    defaults: {
      companyId: "",
      name: "",
      number: "",
      address: "",
      zip: "",
      city: "",
      country: "",
      ipaddressmasklist: "",
      partnerid: 1,
      cgidisplayname: null,
      cgiserviceid: null,
      idledoctimeout: null,
      allowsavesafetycopy: true,
      smsprovider: "",
      padappmode: "",
      padearchiveenabled: true
    },
    saveDetails: function () {
      return new Submit({
        url: "/adminonly/companyadmin/" + this.get("companyId"),
        method: "POST",
        companyname: this.get("name"),
        companynumber: this.get("number"),
        companyaddress: this.get("address"),
        companyzip: this.get("zip"),
        companycity: this.get("city"),
        companycountry: this.get("country"),
        companyipaddressmasklist: this.get("ipaddressmasklist"),
        companypartnerid: this.get("partnerid"),
        companycgidisplayname: this.get("cgidisplayname"),
        companycgiserviceid: this.get("cgiserviceid"),
        companyidledoctimeout: this.get("idledoctimeout"),
        companyallowsavesafetycopy: this.get("allowsavesafetycopy"),
        companysmsprovider: this.get("smsprovider"),
        companypadappmode: this.get("padappmode"),
        companypadearchiveenabled: this.get("padearchiveenabled")
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
