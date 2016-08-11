var jQuery = require("jquery");
var FlashMessage = require("../../flashmessages.js").FlashMessage;
var Confirmation = require("../../confirmations.js").Confirmation;
var EmailValidation = require("../../validation.js").EmailValidation;
var Track = require("../../../scripts/common/track");

module.exports = function(model) {
  Track.track('Click change email button');
  var body = jQuery("<div/>");
  var p = jQuery("<p/>").text(localization.account.accountDetails.changeEmailExplaination);
  body.append(p);
  var table = jQuery("<table/>");

  var tr1 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.account.accountDetails.changeEmailCurrentEmail));
  var currentemail = jQuery("<label style='margin-left: 10px;'/>").text(model.user().email());
  tr1.append(jQuery("<td/>").append(currentemail));
  table.append(tr1);

  var tr2 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.account.accountDetails.newEmail ));
  var newemail = jQuery("<input type='text' name='newemail' autocomplete='off' style='margin: 5px 10px;'/>");
  newemail.change(function() {model.setNewEmail(newemail.val());});
  tr2.append(jQuery("<td/>").append(newemail));
  table.append(tr2);

  var tr3 = jQuery("<tr/>").append(jQuery("<td/>").text(localization.account.accountDetails.newEmailAgain));
  var newemailagain = jQuery("<input type='text' name='newemailagain' autocomplete='off' style='margin: 5px 10px;' />");
  newemailagain.change(function() {model.setNewEmailAgain(newemailagain.val());});
  tr3.append(jQuery("<td/>").append(newemailagain));
  table.append(tr3);

  body.append(table);

  var confirmation = new Confirmation({
    onAccept: function() {
      if ( ! new EmailValidation().validateData(model.newemail())) {
        new FlashMessage({type: 'error', content : localization.account.accountDetails.invalidEmail });
        return false;
      }
      if (model.newemail() != model.newemailagain()) {
        new FlashMessage({type: 'error', content : localization.account.accountDetails.mismatchedEmails });
        return false;
      }
      Track.track_timeout(
        'Accept',
        {'Accept' : 'Change email'},
        function () {
          model.changeEmail(
            function() {
              model.updateProfile(function() {
                confirmation.close();
                model.refresh();
              });
            }
          );
        }
      );
    },
    onReject : function() {
      Track.track('Reject', {'Reject' : 'Change email'});
      model.setNewEmail("");
      model.setNewEmailAgain("");
    },
    title: localization.account.accountDetails.changeEmailTitle,
    acceptButtonText: localization.account.accountDetails.changeEmailAccept,
    content: body
  });
};
