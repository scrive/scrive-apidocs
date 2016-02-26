var React = require("react");
var CompanyAccountsTable = require("../../../scripts/account/usersandstats/companyaccountstable");
var $ = require("jquery");

/*
 * Main archive definition. Its a tab based set of different documents lists.
 *
 * Instrument for Mixpanel
 */

var CompanyAccounts = exports.CompanyAccounts = function(args) {
    var el =  $("<div/>");
    var table = React.render(React.createElement(CompanyAccountsTable,{}), el[0]);
    return {
      refresh : function() {table.reload();},
      el  : function() {return el;}
    };
};

