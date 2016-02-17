var React = require("react");
var OauthDashboardView = require("../../../scripts/account/apisettings/oauthdashboardview");
var $ = require("jquery");

/*
 * List of API keys + information about callback settings
 *
 *
 */

var OauthDashboard = exports.OauthDashboard = function(args) {
    var el =  $("<div/>");
    var table = React.render(React.createElement(OauthDashboardView,{}), el[0]);
    return {
      refresh : function() {table.reload();},
      el  : function() {return el;}
    };
};


