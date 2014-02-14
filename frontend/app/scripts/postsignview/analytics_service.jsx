'use strict';

define(function() {
  var expose = {};

  /**
   *  @description
   *  Register a new user in mixpanel and track the event 'create new account'
   *
   *  @param {number} userId
   */
  expose.setNewAnalyticsUser = function(userId) {
    mixpanel.alias(userId);
    mixpanel.identify(userId);
    mixpanel.track('Create new account',
                   {'Signup Method' : 'BySigning'});
    mixpanel.track('Sign TOS');
  };

  /**
   *
   *  @description
   *  Prepare user data and register it in Mixpanel,
   *  for the currently identified Mixpanel user
   *
   *  @param {User} user Object with user-data to register in mixpanel
   *  @param {string} language The language the user have in Scrive
   *  @param {string} referringCompany Which company that referred the user
   *  @param {string} signupMethid Where the user signed up
   */
  expose.setMixpanelUserData = function(user, language, referringCompany, signupMethod) {
    // Set default argument values
    signupMethod = typeof signupMethod !== 'undefined' ? signupMethod : 'BySigning';
    referringCompany = typeof referringCompany !== 'undefined' ? referringCompany : '';

    var userData = {'TOS Date': new Date(),
                    'Full Name': user.name(),
                    '$first_name': user.fstname(),
                    '$last_name': user.sndname(),
                    '$email': user.email(),
                    'Language': language,
                    'Referring Company': referringCompany,
                    'Signup Method': signupMethod
                   };

    // Don't set company name to anything if not set, for easier filtering in MixPanel.
    if(user.company()) {
      userData['Company name'] = user.company();
    }

    // Register current user's data in Mixpanel
    mixpanel.register(userData); // as super property
    mixpanel.people.set(userData); // as regular person property
  };

  return expose;
});
