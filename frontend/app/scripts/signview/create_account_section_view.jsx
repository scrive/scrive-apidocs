/** @jsx React.DOM */

define(['React', 'common/utilities_service', 'postsignview/create_account_views', 'postsignview/user_service', 'Backbone', 'Underscore'], function(React, UtilitiesService, CreateAccountViews, UserService, Backbone, _) {
  var expose = {};
  
  /**
   *  @description
   *  Register a new user and redirect him or change content of supplied element
   */
  var bannerRegisterUser = function(document, companyName, sectionElement) {
    UserService.registerUser(document).then(function() {
      mixpanel.people.set({
        'Accepted Promotion': true,
        'Promotion': companyName
      });
      
      if(document.currentSignatory().padDelivery()) {
        React.renderComponent(CreateAccountViews.PadSafetyCopySaved(null), sectionElement);
      } else {
        window.location.pathname = '/newdocument';
      }
    }.bind(this));
  };
  
  /**
   *  @description
   *  Register a new user that signed on a pad and change content of supplied element
   */
  var padRegisterUser = function(document, sectionElement) {
    UserService.registerUser(document).then(function() {
      React.renderComponent(CreateAccountViews.PadSafetyCopySaved(null), sectionElement);
    });
  };
  
  /**
   *  @description
   *  Render different types of create account section on postsignview,
   *  depending on a few different conditions.
   *
   *  TODO(jens): This should be a React component, when the signview is made to be a React component.
   *
   */
  expose.render = function(document, sectionElement) {
    var promotionImg = '/img/partnerbanners/',
    language = UtilitiesService.getCurrentLanguage(),
    sectionElement = sectionElement[0]; // Make it a raw html object instead of jQuery object
    
    if(document.author().company() === 'Phone House') {
      // Phone house, create account banner
      var component = CreateAccountViews.BrandedBanner({
        bannerType: 'phone-house',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'Phone House', sectionElement)
      });
    } else if(null !== /^nj.*scrive.com/.exec(location.host)) {
      // Nordsteds juridik, create account section
      
      var component = CreateAccountViews.BrandedBanner({
        bannerType: 'nj',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'NJ', sectionElement)
      });
    } else if(document.currentSignatory().padDelivery()) {
      // Signing in padqueue, create account banner
      
      var component = CreateAccountViews.SaveBackupCopy({
        isSmallScreen: BrowserInfo.isSmallScreen(),
        registerUser: _.partial(padRegisterUser, document, sectionElement)
      });
    } else {
      var component = CreateAccountViews.SaveBackupCopy({
        isSmallScreen: BrowserInfo.isSmallScreen(),
        registerUser: _.partial(padRegisterUser, document, sectionElement)
      });
      console.log("Show questionnare");
      // TODO(jens): Show questionnare
    }
    React.renderComponent(component, sectionElement);
  };
  
  return expose;
});
