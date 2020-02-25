var React = require("react");
var AcceptTOSView = require("../../account/accepttos");
var $ = require("jquery");
var Header = require("../../pages/special/header");
var Footer = require("../../pages/special/footer");
var Submit = require("../../../js/submits").Submit;
var PadList = require("../../padlist/padlist");

$(function () {
  (function () {
    var container = document.createElement("div");
    container.className = "short-input-section accept-tos s-accept-tos";

    var view = React.render(React.createElement(AcceptTOSView, {}), container);
    $(".inner").append(container);
  })();

   $(function(){
    var padlistDiv = $('<div/>')
    var padlist = React.render(React.createElement(PadList,{}),padlistDiv[0]);
    var refresherPadListStep = function() {
      setTimeout(function() {
        padlist.refreshStep();
        refresherPadListStep();
      },1000);
    };
    refresherPadListStep();
    var headerDiv = $('<div/>');
    React.render(React.createElement(Header,{
         linkText : fromTemplate.headerLogOut,
         linkOnClick : function() {
            new Submit({
              url : "/logout_ajax",
              ajax : true,
              method: "POST",
              ajaxsuccess : function() {
              window.location.reload();}
            }).send();
         }
      }), headerDiv[0]);

    var footerDiv = $('<div/>');
    React.render(React.createElement(Footer,{
    }), footerDiv[0]);
    $(".pad-list").prepend(headerDiv);
    $(".list-container").append(padlistDiv);
    $(".pad-list").append(footerDiv);
  });

  mixpanel.register({
    Context : 'List for pad signing'
  });

});
