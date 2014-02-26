define(['Q', 'postsignview/analytics_service'], function(Q, AnalyticsService) {
  var expose = {};

  /**
   *  Create an account for a user that just have signed a document (PostSignView)
   *
   *  @param {boolean} stopUserLogin If true, the backend doesnt login the user
   *
   *  TODO: Make a real API call for it, so we dont need to send it as a FORM
   */
  expose.createAccountPSV = function(stopUserLogin, currentSignatory) {
    var deferred = Q.defer();

    // Should be posted as a form
    var submitValues = {
      url: currentSignatory.saveurl(),
      method: 'POST',
      acceptaccount: true,
      tos: 'on',
      email: currentSignatory.email(),
      ajax: true,
      stopLogin: stopUserLogin,
      ajaxsuccess: function(userObject) {
        userObjectParsed = JSON.parse(userObject);

        // simple way to handle errors..
        // TODO(jens): Make error handling better
        if(userObjectParsed.userid) {
          deferred.resolve(userObjectParsed.userid);
        }
      }
    };

    // Submit form to backend
    new window.Submit(submitValues).send();

    return deferred.promise;
  };


  /**
   *  @description
   *  Create new account for a user that dont have a Scrive account and then redirect the
   *  user to the right place.
   */
  expose.registerUser = function(document) {
    var currentSignatory = document.currentSignatory(),
    padDelivery = currentSignatory.padDelivery(),
    email = currentSignatory.email();

    return expose.createAccountPSV(padDelivery, currentSignatory).then(function (userId) {

      // When we register a user on postsignview, this user get's a new userid,
      // which is different from the id the user have when he sign the document =>
      // Use userid that we get from the ajax request when we register the user in analytics.
      AnalyticsService.setNewAnalyticsUser(userId);


      var documentLanguage = document.lang().simpleCode();

      AnalyticsService.setMixpanelUserData(currentSignatory, documentLanguage, document.author().company());

    });

  };

  return expose;
});
