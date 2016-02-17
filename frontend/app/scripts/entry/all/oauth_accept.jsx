var OAuthConfirmation = require("../../../js/oauth").OAuthConfirmation;
var $ = require("jquery");

$(function () {
  var target = $(".oauth-confirm");
  var confirmation =  new OAuthConfirmation(fromTemplate);

  target.append(confirmation.view().el);

  $(".js-deny").click(function() {
     confirmation.deny();
     return false;
  })
});
