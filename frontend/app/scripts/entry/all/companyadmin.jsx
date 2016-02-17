var CompanyAdmin = require("../../../js/admin/companyadmin").CompanyAdmin;
var $ = require("jquery");

$(function () {
  $(".admin").append(new CompanyAdmin({companyid: fromTemplate.companyId}).el());
});
