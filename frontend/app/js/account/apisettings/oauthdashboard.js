/*
 * List of API keys + information about callback settings
 *
 *
 */
define(['React','account/apisettings/oauthdashboardview', 'legacy_code'], function(React,OauthDashboardView) {

window.OauthDashboard = function(args) {
    var el =  $("<div/>");
    var table = React.render(React.createElement(OauthDashboardView,{}), el[0]);
    return {
      refresh : function() {table.reload();},
      el  : function() {return el;}
    };
};

});

