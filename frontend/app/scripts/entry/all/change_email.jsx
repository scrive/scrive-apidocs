var React = require("react");
var Confirmation = require("../../../js/confirmations").Confirmation;
var Submit = require("../../../js/submits").Submit;
var $ = require("jquery");

$(function () {
  var content = $('<div/>').addClass('recovery-container');
  content.append($('<p/>').append(fromTemplate.body));
  var password_input = $('<input type="password" name="password" autocomplete="off"/>');
  content.append($('<p/>').append(password_input));
  new Confirmation({
    onAccept: function() {
      new Submit({
        method: 'POST',
        url: '',
        password: password_input.val()}).send();
      },
    onReject : function() { window.location = '/account' ; },
    title: fromTemplate.title,
    acceptButtonText: 'OK',
    content: content
  });
});
