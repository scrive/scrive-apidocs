var React = require("react");
var Confirmation = require("../../../js/react_confirmations");
var Submit = require("../../../js/submits").Submit;
var $ = require("jquery");

$(function () {
  var content = $('<div/>').addClass('recovery-container');
  content.append($('<p/>').append(fromTemplate.body));
  new Confirmation({
    onAccept: function() {
      new Submit({method: 'POST', url: ''}).send();
    },
    onReject : function() { window.location = '/account' ; },
    title: fromTemplate.title,
    acceptButtonText: fromTemplate.acceptButtonText,
    content: content
  });
});
