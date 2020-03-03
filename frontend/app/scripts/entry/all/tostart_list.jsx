var React = require("react");
var $ = require("jquery");

var Header = require("../../pages/special/header");
var Footer = require("../../pages/special/footer");
var Submit = require("../../../js/submits").Submit;
var ToStartList = require("../../to-start/templatelist");

$(function () {
  var listDiv = $('.list-container');
  var component = React.render(React.createElement(ToStartList,{
  }), listDiv[0]);

  var headerDiv = $('<div/>');
  var component = React.render(React.createElement(Header,{
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
  var component = React.render(React.createElement(Footer,{
    }), footerDiv[0]);

  $(".to-start").prepend(headerDiv);
  $(".to-start").append(footerDiv);

  mixpanel.register({
    Context : 'List for to-start'
  });
});
