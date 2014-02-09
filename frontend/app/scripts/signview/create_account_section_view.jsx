/** @jsx React.DOM */

define(['React', 'common/language_service', 'postsignview/questionnaire_view', 'postsignview/create_account_views', 'postsignview/user_service', 'Backbone', 'Underscore'], function(React, LanguageService, QuestionareView, CreateAccountViews, UserService, Backbone, _) {
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

  var normalRegisterUser = function(document, sectionElement) {
    UserService.registerUser(document).then(function() {
      window.location = '/r/#/postsignview/archive';
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
    language = LanguageService.currentLanguage(),
    // React.renderComponent need a html object to attach to, not a jquery html object
    sectionElementRaw = sectionElement[0]; 
    
    if(document.author().company() === 'Phone House') {
      // Phone house, create account banner
      var component = CreateAccountViews.BrandedBanner({
        bannerType: 'phone-house',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'Phone House', sectionElementRaw)
      });
      React.renderComponent(component, sectionElementRaw);
    } else if(null !== /^nj.*scrive.com/.exec(location.host)) {
      // Nordsteds juridik, create account section
      
      var component = CreateAccountViews.BrandedBanner({
        bannerType: 'nj',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'NJ', sectionElementRaw)
      });
      React.renderComponent(component, sectionElementRaw);
    } else if(document.currentSignatory().company() !== '') {
      // B2B contracts
      var view = new QuestionareView({model: document});
      sectionElement.append(view.render());
    } else {

      var registerUser;
      if(BrowserInfo.isSmallScreen()) {
	registerUser = _.partial(padRegisterUser, document, sectionElementRaw)
      } else {
	registerUser = _.partial(normalRegisterUser, document, sectionElementRaw)
      }

      var component = CreateAccountViews.SaveBackupCopy({
        isSmallScreen: BrowserInfo.isSmallScreen(),
        registerUser: registerUser
      });
      React.renderComponent(component, sectionElementRaw);
    }
  };
  
  return expose;
});
