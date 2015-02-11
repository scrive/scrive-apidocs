/*
 * Main archive definition. Its a tab based set of different documents lists.
 *
 * Instrument for Mixpanel
 */
define(['React','account/usersandstats/companyaccountstable', 'legacy_code'], function(React,CompanyAccountsTable) {

window.CompanyAccounts = function(args) {
    var el =  $("<div/>");
    var table = React.render(React.createElement(CompanyAccountsTable,{}), el[0]);
    return {
      refresh : function() {table.reload();},
      el  : function() {return el;}
    };
};

});
