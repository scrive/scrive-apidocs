/** @jsx React.DOM */

define(['React', 'utils/browserinfo', 'common/language_service', 'postsignview/questionnaire_view', 'postsignview/create_account_views', 'postsignview/user_service', 'Backbone', 'Underscore'], function(React, BrowserInfo, LanguageService, QuestionareView, CreateAccountViews, UserService, Backbone, _) {
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

      window.location.pathname = '/newdocument';
    }.bind(this));
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
    var promotionName = "";
    var promotionImg = '/img/partnerbanners/',
        language = LanguageService.currentLanguage(),
        component,
        // React.renderComponent need a html object to attach to, not a jquery html object
        sectionElementRaw = sectionElement[0];

    // TODO(jens): Remove this when we drop support for IE7
    if(BrowserInfo.isIE7orLower()) {
      return;
    }

    if(document.author().company() === 'Phone House') {
      // Phone house, create account banner
      promotionName = "Phone House";
      component = CreateAccountViews.BrandedBanner({
        bannerType: 'phone-house',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'Phone House', sectionElementRaw)
      });
      React.renderComponent(component, sectionElementRaw);
    } else if(null !== /^nj.*scrive.com/.exec(location.host)) {
      promotionName = "NJ";
      // Nordsteds juridik, create account section

      component = CreateAccountViews.BrandedBanner({
        bannerType: 'nj',
        language: language,
        registerUser: _.partial(bannerRegisterUser, document, 'NJ', sectionElementRaw)
      });
      React.renderComponent(component, sectionElementRaw);
    } else if(document.currentSignatory().company() !== '') {
      promotionName = "Questionnaire";
      // B2B contracts
      React.renderComponent(QuestionareView({document: document}), sectionElementRaw);
    } else {

      var registerUser = _.partial(normalRegisterUser, document, sectionElementRaw);

      component = CreateAccountViews.SaveBackupCopy({
        isSmallScreen: BrowserInfo.isSmallScreen(),
        registerUser: registerUser
      });
      React.renderComponent(component, sectionElementRaw);
    }

    mixpanel.track("Store copy button shown", promotionName ? { promo: promotionName } : {});
  };

  return expose;
});
